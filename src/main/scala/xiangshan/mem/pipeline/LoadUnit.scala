/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan.ExceptionNO._
import xiangshan._
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.util.SdtrigExt
import xiangshan.cache._
import xiangshan.cache.mmu.{TlbCmd, TlbReq, TlbRequestIO, TlbResp}

class LoadToLsqIO(implicit p: Parameters) extends XSBundle {
  val loadIn = ValidIO(new LqWriteBundle)
  val loadPaddrIn = ValidIO(new LqPaddrWriteBundle)
  val ldout = Flipped(DecoupledIO(new ExuOutput))
  val ldRawData = Input(new LoadDataFromLQBundle)
  val s2_load_data_forwarded = Output(Bool())
  val s3_delayed_load_error = Output(Bool())
  val s2_dcache_require_replay = Output(Bool())
  val s3_replay_from_fetch = Output(Bool()) // update uop.ctrl.replayInst in load queue in s3
  val forward = new PipeLoadForwardQueryIO
  val loadViolationQuery = new LoadViolationQueryIO
  val trigger = Flipped(new LqTriggerIO)
}

class LoadToLoadIO(implicit p: Parameters) extends XSBundle {
  // load to load fast path is limited to ld (64 bit) used as vaddr src1 only
  val data = UInt(XLEN.W)
  val valid = Bool()
}

class LoadUnitTriggerIO(implicit p: Parameters) extends XSBundle {
  val tdata2 = Input(UInt(64.W))
  val matchType = Input(UInt(2.W))
  val tEnable = Input(Bool()) // timing is calculated before this
  val addrHit = Output(Bool())
  val lastDataHit = Output(Bool())
}

// Load Pipeline Stage 0
// Generate addr, use addr to query DCache and DTLB
class LoadUnit_S0(implicit p: Parameters) extends XSModule with HasDCacheParameters{
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new ExuInput))
    val out = Decoupled(new LsPipelineBundle)
    val dtlbReq = DecoupledIO(new TlbReq)
    val dcacheReq = DecoupledIO(new DCacheWordReq)
    val rsIdx = Input(UInt(log2Up(IssQueSize).W))
    val isFirstIssue = Input(Bool())
    val fastpath = Input(new LoadToLoadIO)
    val s0_kill = Input(Bool())
  })
  require(LoadPipelineWidth == exuParameters.LduCnt)

  val imm12 = io.in.bits.uop.ctrl.imm(11, 0)
  val s0_vaddr = WireInit(io.in.bits.src(0) + SignExt(imm12, VAddrBits))
  val s0_mask = WireInit(genWmask(s0_vaddr, io.in.bits.uop.ctrl.fuOpType(1,0)))
  val s0_uop = WireInit(io.in.bits.uop)

  val s0_replayShouldWait = io.in.valid && isAfter(io.replay.bits.uop.robIdx, io.in.bits.uop.robIdx)
  // load flow select/gen
  //
  // src0: load replayed by LSQ (io.replay)
  // src1: hardware prefetch from prefetchor (high confidence) (io.prefetch)
  // src2: int read / software prefetch first issue from RS (io.in)
  // src3: vec read first issue from RS (TODO)
  // src4: load try pointchaising when no issued or replayed load (io.fastpath)
  // src5: hardware prefetch from prefetchor (high confidence) (io.prefetch)

  // load flow source valid
  val lfsrc0_loadFastReplay_valid = io.fastReplay.valid
  val lfsrc1_loadReplay_valid = io.replay.valid && !s0_replayShouldWait 
  val lfsrc2_highconfhwPrefetch_valid = io.prefetch_in.valid && io.prefetch_in.bits.confidence > 0.U
  val lfsrc3_intloadFirstIssue_valid = io.in.valid // int flow first issue or software prefetch
  val lfsrc4_vecloadFirstIssue_valid = WireInit(false.B) // TODO
  val lfsrc5_l2lForward_valid = io.fastpath.valid
  val lfsrc6_lowconfhwPrefetch_valid = io.prefetch_in.valid && io.prefetch_in.bits.confidence === 0.U
  dontTouch(lfsrc0_loadFastReplay_valid)
  dontTouch(lfsrc1_loadReplay_valid)
  dontTouch(lfsrc2_highconfhwPrefetch_valid)
  dontTouch(lfsrc3_intloadFirstIssue_valid)
  dontTouch(lfsrc4_vecloadFirstIssue_valid)
  dontTouch(lfsrc5_l2lForward_valid)
  dontTouch(lfsrc6_lowconfhwPrefetch_valid)
  
  // load flow source ready
  val lfsrc_loadFastReplay_ready = WireInit(true.B)
  val lfsrc_loadReplay_ready = !lfsrc0_loadFastReplay_valid
  val lfsrc_highconfhwPrefetch_ready = !lfsrc0_loadFastReplay_valid &&
    !lfsrc1_loadReplay_valid 
  val lfsrc_intloadFirstIssue_ready = !lfsrc0_loadFastReplay_valid &&
    !lfsrc1_loadReplay_valid &&
    !lfsrc2_highconfhwPrefetch_valid
  val lfsrc_vecloadFirstIssue_ready = !lfsrc0_loadFastReplay_valid &&
    !lfsrc1_loadReplay_valid &&
    !lfsrc2_highconfhwPrefetch_valid &&
    !lfsrc3_intloadFirstIssue_valid
  val lfsrc_l2lForward_ready = !lfsrc0_loadFastReplay_valid &&
    !lfsrc1_loadReplay_valid &&
    !lfsrc2_highconfhwPrefetch_valid &&
    !lfsrc3_intloadFirstIssue_valid &&
    !lfsrc4_vecloadFirstIssue_valid
  val lfsrc_lowconfhwPrefetch_ready = !lfsrc0_loadFastReplay_valid &&
    !lfsrc1_loadReplay_valid && 
    !lfsrc2_highconfhwPrefetch_valid &&
    !lfsrc3_intloadFirstIssue_valid &&
    !lfsrc4_vecloadFirstIssue_valid &&
    !lfsrc5_l2lForward_valid
  dontTouch(lfsrc_loadFastReplay_ready)
  dontTouch(lfsrc_loadReplay_ready)
  dontTouch(lfsrc_highconfhwPrefetch_ready)
  dontTouch(lfsrc_intloadFirstIssue_ready)
  dontTouch(lfsrc_vecloadFirstIssue_ready)
  dontTouch(lfsrc_l2lForward_ready)
  dontTouch(lfsrc_lowconfhwPrefetch_ready)
    
  // load flow source select (OH)
  val lfsrc_loadFastReplay_select = lfsrc0_loadFastReplay_valid && lfsrc_loadFastReplay_ready
  val lfsrc_loadReplay_select = lfsrc1_loadReplay_valid && lfsrc_loadReplay_ready
  val lfsrc_hwprefetch_select = lfsrc_highconfhwPrefetch_ready && lfsrc2_highconfhwPrefetch_valid || 
    lfsrc_lowconfhwPrefetch_ready && lfsrc6_lowconfhwPrefetch_valid
  val lfsrc_intloadFirstIssue_select = lfsrc_intloadFirstIssue_ready && lfsrc3_intloadFirstIssue_valid
  val lfsrc_vecloadFirstIssue_select = lfsrc_vecloadFirstIssue_ready && lfsrc4_vecloadFirstIssue_valid
  val lfsrc_l2lForward_select = lfsrc_l2lForward_ready && lfsrc5_l2lForward_valid
  assert(!lfsrc_vecloadFirstIssue_select) // to be added
  dontTouch(lfsrc_loadFastReplay_select)
  dontTouch(lfsrc_loadReplay_select)
  dontTouch(lfsrc_hwprefetch_select)
  dontTouch(lfsrc_intloadFirstIssue_select)
  dontTouch(lfsrc_vecloadFirstIssue_select)
  dontTouch(lfsrc_l2lForward_select)

  io.l2lForward_select := lfsrc_l2lForward_select

  // s0_valid == ture iff there is a valid load flow in load_s0
  val s0_valid = lfsrc0_loadFastReplay_valid ||
    lfsrc1_loadReplay_valid ||
    lfsrc2_highconfhwPrefetch_valid ||
    lfsrc3_intloadFirstIssue_valid ||
    lfsrc4_vecloadFirstIssue_valid ||
    lfsrc5_l2lForward_valid ||
    lfsrc6_lowconfhwPrefetch_valid

  // prefetch related ctrl signal
  val isPrefetch = WireInit(false.B)
  val isPrefetchRead = WireInit(s0_uop.ctrl.fuOpType === LSUOpType.prefetch_r)
  val isPrefetchWrite = WireInit(s0_uop.ctrl.fuOpType === LSUOpType.prefetch_w)
  val isHWPrefetch = lfsrc_hwprefetch_select
  val isHlv = WireInit(LSUOpType.isHlv(s0_uop.ctrl.fuOpType))
  val isHlvx = WireInit(LSUOpType.isHlvx(s0_uop.ctrl.fuOpType))

  // query DTLB
  io.dtlbReq.valid := s0_valid
  // hw prefetch addr does not need to be translated, give tlb paddr
  io.dtlbReq.bits.vaddr := Mux(lfsrc_hwprefetch_select, io.prefetch_in.bits.paddr, s0_vaddr) 
  io.dtlbReq.bits.cmd := Mux(isPrefetch,
    Mux(isPrefetchWrite, TlbCmd.write, TlbCmd.read),
    TlbCmd.read
  )
  io.dtlbReq.bits.hyperinst := isHlv
  io.dtlbReq.bits.hlvx := isHlvx
  io.dtlbReq.bits.size := LSUOpType.size(s0_uop.ctrl.fuOpType)
  io.dtlbReq.bits.kill := DontCare
  io.dtlbReq.bits.memidx.is_ld := true.B
  io.dtlbReq.bits.memidx.is_st := false.B
  io.dtlbReq.bits.memidx.idx := s0_uop.lqIdx.value
  io.dtlbReq.bits.debug.robIdx := s0_uop.robIdx
  // hw prefetch addr does not need to be translated
  io.dtlbReq.bits.no_translate := lfsrc_hwprefetch_select
  io.dtlbReq.bits.debug.pc := s0_uop.cf.pc
  io.dtlbReq.bits.debug.isFirstIssue := s0_isFirstIssue

  // query DCache
  io.dcache.req.valid             := s0_valid
  io.dcache.req.bits.cmd          := Mux(s0_prf_rd,
                                      MemoryOpConstants.M_PFR,
                                      Mux(s0_prf_wr, MemoryOpConstants.M_PFW, MemoryOpConstants.M_XRD)
                                    )
  io.dcache.req.bits.vaddr        := s0_vaddr
  io.dcache.req.bits.mask         := s0_mask
  io.dcache.req.bits.data         := DontCare
  io.dcache.req.bits.isFirstIssue := s0_isFirstIssue
  io.dcache.req.bits.instrtype    := Mux(s0_prf, DCACHE_PREFETCH_SOURCE.U, LOAD_SOURCE.U)
  io.dcache.req.bits.debug_robIdx := s0_uop.robIdx.value
  io.dcache.req.bits.replayCarry  := s0_rep_carry
  io.dcache.req.bits.id           := DontCare // TODO: update cache meta

  // load flow priority mux
  def fromNullSource() = {
    s0_vaddr         := 0.U
    s0_mask          := 0.U
    s0_uop           := 0.U.asTypeOf(new MicroOp)
    s0_try_l2l       := false.B
    s0_has_rob_entry := false.B
    s0_rsIdx         := 0.U
    s0_rep_carry     := 0.U.asTypeOf(s0_rep_carry.cloneType)
    s0_mshrid        := 0.U
    s0_isFirstIssue  := false.B
    s0_fast_rep      := false.B
    s0_ld_rep        := false.B
    s0_l2l_fwd       := false.B
    s0_prf           := false.B
    s0_prf_rd        := false.B
    s0_prf_wr        := false.B
    s0_sched_idx     := 0.U
    s0_hlv           := false.B
    s0_hlvx          := false.B
  }

  def fromFastReplaySource(src: LqWriteBundle) = {
    s0_vaddr         := src.vaddr
    s0_mask          := src.mask
    s0_uop           := src.uop
    s0_try_l2l       := false.B
    s0_has_rob_entry := src.hasROBEntry
    s0_rep_carry     := src.rep_info.rep_carry
    s0_mshrid        := src.rep_info.mshr_id
    s0_rsIdx         := src.rsIdx
    s0_isFirstIssue  := false.B
    s0_fast_rep      := true.B
    s0_ld_rep        := src.isLoadReplay
    s0_l2l_fwd       := false.B
    s0_prf           := LSUOpType.isPrefetch(src.uop.ctrl.fuOpType)
    s0_prf_rd        := src.uop.ctrl.fuOpType === LSUOpType.prefetch_r
    s0_prf_wr        := src.uop.ctrl.fuOpType === LSUOpType.prefetch_w
    s0_sched_idx     := src.schedIndex
    s0_hlv           := LSUOpType.isHlv(src.uop.ctrl.fuOpType)
    s0_hlvx          := LSUOpType.isHlvx(src.uop.ctrl.fuOpType)
  }
  io.dcacheReq.bits.debug_robIdx := s0_uop.robIdx.value
  io.dcacheReq.bits.replayCarry := s0_replayCarry

  // TODO: update cache meta
  io.dcacheReq.bits.id := DontCare

  // assign default value
  def fromNormalReplaySource(src: LsPipelineBundle) = {
    s0_vaddr         := src.vaddr
    s0_mask          := genVWmask(src.vaddr, src.uop.ctrl.fuOpType(1, 0))
    s0_uop           := src.uop
    s0_try_l2l       := false.B
    s0_has_rob_entry := true.B
    s0_rsIdx         := src.rsIdx
    s0_rep_carry     := src.replayCarry
    s0_mshrid        := src.mshrid
    s0_isFirstIssue  := false.B
    s0_fast_rep      := false.B
    s0_ld_rep        := true.B
    s0_l2l_fwd       := false.B
    s0_prf           := LSUOpType.isPrefetch(src.uop.ctrl.fuOpType)
    s0_prf_rd        := src.uop.ctrl.fuOpType === LSUOpType.prefetch_r
    s0_prf_wr        := src.uop.ctrl.fuOpType === LSUOpType.prefetch_w
    s0_sched_idx     := src.schedIndex
    s0_hlv           := LSUOpType.isHlv(src.uop.ctrl.fuOpType)
    s0_hlvx          := LSUOpType.isHlvx(src.uop.ctrl.fuOpType)
  }

  def fromPrefetchSource(src: L1PrefetchReq) = {
    s0_vaddr         := src.getVaddr()
    s0_mask          := 0.U
    s0_uop           := DontCare
    s0_try_l2l       := false.B
    s0_has_rob_entry := false.B
    s0_rsIdx         := 0.U
    s0_rep_carry     := 0.U.asTypeOf(s0_rep_carry.cloneType)
    s0_mshrid        := 0.U
    s0_isFirstIssue  := false.B
    s0_fast_rep      := false.B
    s0_ld_rep        := false.B
    s0_l2l_fwd       := false.B
    s0_prf           := true.B
    s0_prf_rd        := !src.is_store
    s0_prf_wr        := src.is_store
    s0_sched_idx     := 0.U
    s0_hlv           := false.B
    s0_hlvx          := false.B
  }

  def fromIntIssueSource(src: ExuInput) = {
    s0_vaddr         := src.src(0) + SignExt(src.uop.ctrl.imm(11, 0), VAddrBits)
    s0_mask          := genVWmask(s0_vaddr, src.uop.ctrl.fuOpType(1,0))
    s0_uop           := src.uop
    s0_try_l2l       := false.B
    s0_has_rob_entry := true.B
    s0_rsIdx         := io.rsIdx
    s0_rep_carry     := 0.U.asTypeOf(s0_rep_carry.cloneType)
    s0_mshrid        := 0.U
    s0_isFirstIssue  := true.B
    s0_fast_rep      := false.B
    s0_ld_rep        := false.B
    s0_l2l_fwd       := false.B
    s0_prf           := LSUOpType.isPrefetch(src.uop.ctrl.fuOpType)
    s0_prf_rd        := src.uop.ctrl.fuOpType === LSUOpType.prefetch_r
    s0_prf_wr        := src.uop.ctrl.fuOpType === LSUOpType.prefetch_w
    s0_sched_idx     := 0.U
    s0_hlv           := LSUOpType.isHlv(src.uop.ctrl.fuOpType)
    s0_hlvx          := LSUOpType.isHlvx(src.uop.ctrl.fuOpType)
  }

  def fromVecIssueSource() = {
    s0_vaddr         := 0.U
    s0_mask          := 0.U
    s0_uop           := 0.U.asTypeOf(new MicroOp)
    s0_try_l2l       := false.B
    s0_has_rob_entry := false.B
    s0_rsIdx         := 0.U
    s0_rep_carry     := 0.U.asTypeOf(s0_rep_carry.cloneType)
    s0_mshrid        := 0.U
    s0_isFirstIssue  := false.B
    s0_fast_rep      := false.B
    s0_ld_rep        := false.B
    s0_l2l_fwd       := false.B
    s0_prf           := false.B
    s0_prf_rd        := false.B
    s0_prf_wr        := false.B
    s0_sched_idx     := 0.U
    s0_hlv           := false.B
    s0_hlvx          := false.B
  }

  def fromLoadToLoadSource(src: LoadToLoadIO) = {
    s0_vaddr              := Cat(src.data(XLEN-1, 6), s0_ptr_chasing_vaddr(5,0))
    s0_mask               := genVWmask(s0_vaddr, io.ld_fast_fuOpType(1, 0))
    // When there's no valid instruction from RS and LSQ, we try the load-to-load forwarding.
    // Assume the pointer chasing is always ld.
    s0_uop.ctrl.fuOpType  := io.ld_fast_fuOpType
    s0_try_l2l            := true.B
    // we dont care s0_isFirstIssue and s0_rsIdx and s0_sqIdx in S0 when trying pointchasing
    // because these signals will be updated in S1
    s0_has_rob_entry      := false.B
    s0_rsIdx              := 0.U
    s0_mshrid             := 0.U
    s0_rep_carry          := 0.U.asTypeOf(s0_rep_carry.cloneType)
    s0_isFirstIssue       := true.B
    s0_fast_rep           := false.B
    s0_ld_rep             := false.B
    s0_l2l_fwd            := true.B
    s0_prf                := false.B
    s0_prf_rd             := false.B
    s0_prf_wr             := false.B
    s0_sched_idx          := 0.U
    s0_hlv                := LSUOpType.isHlv(s0_uop.ctrl.fuOpType)
    s0_hlvx               := LSUOpType.isHlvx(s0_uop.ctrl.fuOpType)
  }

  // set default
  s0_uop := DontCare
  // load flow priority mux
  when (lfsrc_loadFastReplay_select) {
    s0_vaddr := io.fastReplay.bits.vaddr 
    s0_mask := io.fastReplay.bits.mask 
    s0_uop := io.fastReplay.bits.uop
    s0_isFirstIssue := false.B
    s0_sqIdx := io.fastReplay.bits.uop.sqIdx
    s0_replayCarry := io.fastReplay.bits.replayInfo.replayCarry
    s0_mshrid := io.fastReplay.bits.replayInfo.missMSHRId
    s0_rsIdx := io.fastReplay.bits.rsIdx
    s0_isLoadReplay := io.fastReplay.bits.isLoadReplay
    s0_sleepIndex := io.fastReplay.bits.sleepIndex
    val replayUopIsPrefetch = WireInit(LSUOpType.isPrefetch(io.fastReplay.bits.uop.ctrl.fuOpType))
    when (replayUopIsPrefetch) {
      isPrefetch := true.B
    }    
  } .elsewhen(lfsrc_loadReplay_select) {
    s0_vaddr := io.replay.bits.vaddr
    s0_mask := genWmask(io.replay.bits.vaddr, io.replay.bits.uop.ctrl.fuOpType(1, 0))
    s0_uop := io.replay.bits.uop
    s0_isFirstIssue := io.replay.bits.isFirstIssue
    s0_hasROBEntry := true.B
    s0_sqIdx := io.replay.bits.uop.sqIdx
    s0_rsIdx := io.replay.bits.rsIdx
    s0_replayCarry := io.replay.bits.replayCarry
    s0_mshrid := io.replay.bits.mshrid
    s0_isLoadReplay := true.B
    s0_sleepIndex := io.replay.bits.sleepIndex
    val replayUopIsPrefetch = WireInit(LSUOpType.isPrefetch(io.replay.bits.uop.ctrl.fuOpType))
    when (replayUopIsPrefetch) {
      isPrefetch := true.B
    }
  }.elsewhen(lfsrc_hwprefetch_select) {
    // vaddr based index for dcache
    s0_vaddr := io.prefetch_in.bits.getVaddr()
    s0_mask := 0.U
    s0_uop := DontCare
    s0_isFirstIssue := false.B
    s0_rsIdx := DontCare
    s0_sqIdx := DontCare
    s0_replayCarry := DontCare
    s0_mshrid := DontCare
    s0_isLoadReplay := DontCare
    // ctrl signal
    isPrefetch := true.B
    isPrefetchRead := !io.prefetch_in.bits.is_store
    isPrefetchWrite := io.prefetch_in.bits.is_store
  }.elsewhen(lfsrc_intloadFirstIssue_select) {
    val imm12 = io.in.bits.uop.ctrl.imm(11, 0)
    s0_vaddr := io.in.bits.src(0) + SignExt(imm12, VAddrBits)
    s0_mask := genWmask(s0_vaddr, io.in.bits.uop.ctrl.fuOpType(1,0))
    s0_uop := io.in.bits.uop
    s0_isFirstIssue := true.B
    s0_hasROBEntry := true.B
    s0_rsIdx := io.rsIdx
    s0_sqIdx := io.in.bits.uop.sqIdx
    s0_isLoadReplay := false.B
    s0_mshrid := DontCare
    val issueUopIsPrefetch = WireInit(LSUOpType.isPrefetch(io.in.bits.uop.ctrl.fuOpType))
    when (issueUopIsPrefetch) {
      isPrefetch := true.B
    }
  }.otherwise {
    if (EnableLoadToLoadForward) {
      s0_tryFastpath := lfsrc_l2lForward_select
      // When there's no valid instruction from RS and LSQ, we try the load-to-load forwarding.
      s0_vaddr := io.fastpath.data
      // Assume the pointer chasing is always ld.
      s0_uop.ctrl.fuOpType := LSUOpType.ld
      s0_mask := genWmask(0.U, LSUOpType.ld)
    }
  }

  val isSoftPrefetch = LSUOpType.isPrefetch(s0_uop.ctrl.fuOpType)
  val isSoftPrefetchRead = s0_uop.ctrl.fuOpType === LSUOpType.prefetch_r
  val isSoftPrefetchWrite = s0_uop.ctrl.fuOpType === LSUOpType.prefetch_w

  // query DTLB
  io.dtlbReq.valid := io.in.valid || tryFastpath
  io.dtlbReq.bits.vaddr := s0_vaddr
  io.dtlbReq.bits.cmd := TlbCmd.read
  io.dtlbReq.bits.size := LSUOpType.size(s0_uop.ctrl.fuOpType)
  io.dtlbReq.bits.robIdx := s0_uop.robIdx
  io.dtlbReq.bits.debug.pc := s0_uop.cf.pc
  io.dtlbReq.bits.debug.isFirstIssue := io.isFirstIssue

  // query DCache
  io.dcacheReq.valid := io.in.valid || tryFastpath
  when (isSoftPrefetchRead) {
    io.dcacheReq.bits.cmd  := MemoryOpConstants.M_PFR
  }.elsewhen (isSoftPrefetchWrite) {
    io.dcacheReq.bits.cmd  := MemoryOpConstants.M_PFW
  }.otherwise {
    io.dcacheReq.bits.cmd  := MemoryOpConstants.M_XRD
  }
  io.dcacheReq.bits.addr := s0_vaddr
  io.dcacheReq.bits.mask := s0_mask
  io.dcacheReq.bits.data := DontCare
  when(isSoftPrefetch) {
    io.dcacheReq.bits.instrtype := SOFT_PREFETCH.U
  }.otherwise {
    io.dcacheReq.bits.instrtype := LOAD_SOURCE.U
  }

  // TODO: update cache meta
  io.dcacheReq.bits.id   := DontCare

  val addrAligned = LookupTree(s0_uop.ctrl.fuOpType(1, 0), List(
    "b00".U   -> true.B,                   //b
    "b01".U   -> (s0_vaddr(0)    === 0.U), //h
    "b10".U   -> (s0_vaddr(1, 0) === 0.U), //w
    "b11".U   -> (s0_vaddr(2, 0) === 0.U)  //d
  ))

  io.out.valid := (io.in.valid || tryFastpath) && io.dcacheReq.ready && !io.s0_kill

  io.out.bits := DontCare
  io.out.bits.vaddr := s0_vaddr
  io.out.bits.mask := s0_mask
  io.out.bits.uop := s0_uop
  io.out.bits.uop.cf.exceptionVec(loadAddrMisaligned) := !addrAligned
  io.out.bits.rsIdx := io.rsIdx
  io.out.bits.isFirstIssue := io.isFirstIssue
  io.out.bits.isSoftPrefetch := isSoftPrefetch

  io.in.ready := !io.in.valid || (io.out.ready && io.dcacheReq.ready)

  XSDebug(io.dcacheReq.fire,
    p"[DCACHE LOAD REQ] pc ${Hexadecimal(s0_uop.cf.pc)}, vaddr ${Hexadecimal(s0_vaddr)}\n"
  )
  XSPerfAccumulate("in_valid", io.in.valid)
  XSPerfAccumulate("in_fire", io.in.fire)
  XSPerfAccumulate("in_fire_first_issue", io.in.valid && io.isFirstIssue)
  XSPerfAccumulate("stall_out", io.out.valid && !io.out.ready && io.dcacheReq.ready)
  XSPerfAccumulate("stall_dcache", io.out.valid && io.out.ready && !io.dcacheReq.ready)
  XSPerfAccumulate("addr_spec_success", io.out.fire && s0_vaddr(VAddrBits-1, 12) === io.in.bits.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("addr_spec_failed", io.out.fire && s0_vaddr(VAddrBits-1, 12) =/= io.in.bits.src(0)(VAddrBits-1, 12))
  XSPerfAccumulate("addr_spec_success_once", io.out.fire && s0_vaddr(VAddrBits-1, 12) === io.in.bits.src(0)(VAddrBits-1, 12) && io.isFirstIssue)
  XSPerfAccumulate("addr_spec_failed_once", io.out.fire && s0_vaddr(VAddrBits-1, 12) =/= io.in.bits.src(0)(VAddrBits-1, 12) && io.isFirstIssue)
}


// Load Pipeline Stage 1
// TLB resp (send paddr to dcache)
class LoadUnit_S1(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val s1_kill = Input(Bool())
    val out = Decoupled(new LsPipelineBundle)
    val dtlbResp = Flipped(DecoupledIO(new TlbResp(2)))
    val lsuPAddr = Output(UInt(PAddrBits.W))
    val dcachePAddr = Output(UInt(PAddrBits.W))
    val dcacheKill = Output(Bool())
    val dcacheBankConflict = Input(Bool())
    val fullForwardFast = Output(Bool())
    val sbuffer = new LoadForwardQueryIO
    val lsq = new PipeLoadForwardQueryIO
    val loadViolationQueryReq = Decoupled(new LoadViolationQueryReq)
    val rsFeedback = ValidIO(new RSFeedback)
    val csrCtrl = Flipped(new CustomCSRCtrlIO)
    val needLdVioCheckRedo = Output(Bool())
  })

  val s1_fast_rep_dly_err = RegNext(io.fast_rep_in.bits.delayedLoadError)
  val s1_fast_rep_kill    = s1_fast_rep_dly_err && s1_in.isFastReplay
  val s1_l2l_fwd_dly_err  = RegNext(io.l2l_fwd_in.dly_ld_err)
  val s1_l2l_fwd_kill     = s1_l2l_fwd_dly_err && s1_in.isFastPath
  val s1_late_kill        = s1_fast_rep_kill || s1_l2l_fwd_kill
  val s1_vaddr_hi         = Wire(UInt())
  val s1_vaddr_lo         = Wire(UInt())
  val s1_vaddr            = Wire(UInt())
  val s1_paddr_dup_lsu    = Wire(UInt())
  val s1_gpaddr_dup_lsu   = Wire(UInt())
  val s1_paddr_dup_dcache = Wire(UInt())
  val s1_exception        = ExceptionNO.selectByFu(s1_out.uop.cf.exceptionVec, lduCfg).asUInt.orR   // af & pf exception were modified below.
  val s1_tlb_miss         = io.tlb.resp.bits.miss
  val s1_prf              = s1_in.isPrefetch
  val s1_hw_prf           = s1_in.isHWPrefetch
  val s1_sw_prf           = s1_prf && !s1_hw_prf
  val s1_tlb_memidx       = io.tlb.resp.bits.memidx

  io.out.bits := io.in.bits // forwardXX field will be updated in s1
  s1_vaddr_hi         := s1_in.vaddr(VAddrBits - 1, 6)
  s1_vaddr_lo         := s1_in.vaddr(5, 0)
  s1_vaddr            := Cat(s1_vaddr_hi, s1_vaddr_lo)
  s1_paddr_dup_lsu    := io.tlb.resp.bits.paddr(0)
  s1_paddr_dup_dcache := io.tlb.resp.bits.paddr(1)
  s1_gpaddr_dup_lsu   := io.tlb.resp.bits.gpaddr(0)

  io.dtlbResp.ready := true.B

  io.lsuPAddr := s1_paddr_dup_lsu
  io.dcachePAddr := s1_paddr_dup_dcache
  //io.dcacheKill := s1_tlb_miss || s1_exception || s1_mmio
  io.dcacheKill := s1_tlb_miss || s1_exception || io.s1_kill
  // load forward query datapath
  io.sbuffer.valid := io.in.valid && !(s1_exception || s1_tlb_miss || io.s1_kill)
  io.sbuffer.vaddr := io.in.bits.vaddr
  io.sbuffer.paddr := s1_paddr_dup_lsu
  io.sbuffer.gpaddr:= s1_gpaddr_dup_lsu
  io.sbuffer.uop   := s1_in.uop
  io.sbuffer.sqIdx := s1_in.uop.sqIdx
  io.sbuffer.mask  := s1_in.mask
  io.sbuffer.pc    := s1_in.uop.cf.pc // FIXME: remove it

  io.lsq.valid := io.in.valid && !(s1_exception || s1_tlb_miss || io.s1_kill)
  io.lsq.vaddr := io.in.bits.vaddr
  io.lsq.paddr := s1_paddr_dup_lsu
  io.lsq.gpaddr := s1_gpaddr_dup_lsu
  io.lsq.uop := s1_uop
  io.lsq.sqIdx := s1_uop.sqIdx
  io.lsq.sqIdxMask := DontCare // will be overwritten by sqIdxMask pre-generated in s0
  io.lsq.mask := s1_mask
  io.lsq.pc := s1_uop.cf.pc // FIXME: remove it

  // ld-ld violation query
  io.loadViolationQueryReq.valid := io.in.valid && !(s1_exception || s1_tlb_miss || io.s1_kill)
  io.loadViolationQueryReq.bits.paddr := s1_paddr_dup_lsu
  io.loadViolationQueryReq.bits.uop := s1_uop

  s1_out                   := s1_in
  s1_out.vaddr             := s1_vaddr
  s1_out.paddr             := s1_paddr_dup_lsu
  s1_out.gpaddr            := s1_gpaddr_dup_lsu
  s1_out.tlbMiss           := s1_tlb_miss
  s1_out.ptwBack           := io.tlb.resp.bits.ptwBack
  s1_out.rsIdx             := s1_in.rsIdx
  s1_out.rep_info.debug    := s1_in.uop.debugInfo
  s1_out.rep_info.nuke     := s1_nuke && !s1_sw_prf
  s1_out.lateKill          := s1_late_kill

  // Generate feedback signal caused by:
  // * dcache bank conflict
  // * need redo ld-ld violation check
  val needLdVioCheckRedo = io.loadViolationQueryReq.valid &&
    !io.loadViolationQueryReq.ready &&
    RegNext(io.csrCtrl.ldld_vio_check_enable)
  io.needLdVioCheckRedo := needLdVioCheckRedo
  io.rsFeedback.valid := io.in.valid && (s1_bank_conflict || needLdVioCheckRedo) && !io.s1_kill
  io.rsFeedback.bits.hit := false.B // we have found s1_bank_conflict / re do ld-ld violation check
  io.rsFeedback.bits.rsIdx := io.in.bits.rsIdx
  io.rsFeedback.bits.flushState := io.in.bits.ptwBack
  io.rsFeedback.bits.sourceType := Mux(s1_bank_conflict, RSFeedbackType.bankConflict, RSFeedbackType.ldVioCheckRedo)
  io.rsFeedback.bits.dataInvalidSqIdx := DontCare

  // if replay is detected in load_s1,
  // load inst will be canceled immediately
  io.out.valid := io.in.valid && !io.rsFeedback.valid && !io.s1_kill
  io.out.bits.paddr := s1_paddr_dup_lsu
  io.out.bits.gpaddr := s1_gpaddr_dup_lsu
  io.out.bits.tlbMiss := s1_tlb_miss

  // current ori test will cause the case of ldest == 0, below will be modifeid in the future.
  // af & pf exception were modified
  io.out.bits.uop.cf.exceptionVec(loadPageFault) := io.dtlbResp.bits.excp(0).pf.ld
  io.out.bits.uop.cf.exceptionVec(loadAccessFault) := io.dtlbResp.bits.excp(0).af.ld
  io.out.bits.uop.cf.exceptionVec(loadGuestPageFault) := io.dtlbResp.bits.excp(0).pf.ldG
  io.out.bits.ptwBack := io.dtlbResp.bits.ptwBack
  io.out.bits.rsIdx := io.in.bits.rsIdx

  io.out.bits.isSoftPrefetch := io.in.bits.isSoftPrefetch

  io.in.ready := !io.in.valid || io.out.ready

  XSPerfAccumulate("in_valid", io.in.valid)
  XSPerfAccumulate("in_fire", io.in.fire)
  XSPerfAccumulate("in_fire_first_issue", io.in.fire && io.in.bits.isFirstIssue)
  XSPerfAccumulate("tlb_miss", io.in.fire && s1_tlb_miss)
  XSPerfAccumulate("tlb_miss_first_issue", io.in.fire && s1_tlb_miss && io.in.bits.isFirstIssue)
  XSPerfAccumulate("stall_out", io.out.valid && !io.out.ready)
}

// Load Pipeline Stage 2
// DCache resp
class LoadUnit_S2(implicit p: Parameters) extends XSModule with HasLoadHelper {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val out = Decoupled(new LsPipelineBundle)
    val rsFeedback = ValidIO(new RSFeedback)
    val dcacheResp = Flipped(DecoupledIO(new BankedDCacheWordResp))
    val pmpResp = Flipped(new PMPRespBundle())
    val lsq = new LoadForwardQueryIO
    val dataInvalidSqIdx = Input(UInt())
    val sbuffer = new LoadForwardQueryIO
    val dataForwarded = Output(Bool())
    val s2_dcache_require_replay = Output(Bool())
    val fullForward = Output(Bool())
    val dcache_kill = Output(Bool())
    val s3_delayed_load_error = Output(Bool())
    val loadViolationQueryResp = Flipped(Valid(new LoadViolationQueryResp))
    val csrCtrl = Flipped(new CustomCSRCtrlIO)
    val sentFastUop = Input(Bool())
    val static_pm = Input(Valid(Bool())) // valid for static, bits for mmio
    val s2_can_replay_from_fetch = Output(Bool()) // dirty code
    val loadDataFromDcache = Output(new LoadDataFromDcacheBundle)
  })

  val pmp = WireInit(io.pmpResp)
  when (io.static_pm.valid) {
    pmp.ld := false.B
    pmp.st := false.B
    pmp.instr := false.B
    pmp.mmio := io.static_pm.bits
  }

  val s2_is_prefetch = io.in.bits.isSoftPrefetch

  // exception that may cause load addr to be invalid / illegal
  //
  // if such exception happen, that inst and its exception info
  // will be force writebacked to rob
  val s2_exception_vec = WireInit(io.in.bits.uop.cf.exceptionVec)
  s2_exception_vec(loadAccessFault) := io.in.bits.uop.cf.exceptionVec(loadAccessFault) || pmp.ld
  // soft prefetch will not trigger any exception (but ecc error interrupt may be triggered)
  when (s2_is_prefetch) {
    s2_exception_vec := 0.U.asTypeOf(s2_exception_vec.cloneType)
  }
  val s2_exception = ExceptionNO.selectByFu(s2_exception_vec, lduCfg).asUInt.orR

  // writeback access fault caused by ecc error / bus error
  //
  // * ecc data error is slow to generate, so we will not use it until load stage 3
  // * in load stage 3, an extra signal io.load_error will be used to

  // now cache ecc error will raise an access fault
  // at the same time, error info (including error paddr) will be write to
  // an customized CSR "CACHE_ERROR"
  // if (EnableAccurateLoadError) {
  //   io.s3_delayed_load_error := io.dcacheResp.bits.error_delayed &&
  //     io.csrCtrl.cache_error_enable &&
  //     RegNext(io.out.valid)
  // } else {
    io.s3_delayed_load_error := false.B
  // }

  val actually_mmio = pmp.mmio
  val s2_uop = io.in.bits.uop
  val s2_mask = io.in.bits.mask
  val s2_paddr = io.in.bits.paddr
  val s2_tlb_miss = io.in.bits.tlbMiss
  val s2_mmio = !s2_is_prefetch && actually_mmio && !s2_exception
  val s2_cache_miss = io.dcacheResp.bits.miss
  val s2_cache_replay = io.dcacheResp.bits.replay
  val s2_cache_tag_error = 0.U.asTypeOf(io.dcacheResp.bits.tag_error.cloneType)
  val s2_forward_fail = io.lsq.matchInvalid || io.sbuffer.matchInvalid
  val s2_ldld_violation = io.loadViolationQueryResp.valid &&
    io.loadViolationQueryResp.bits.have_violation &&
    RegNext(io.csrCtrl.ldld_vio_check_enable)
  val s2_data_invalid = io.lsq.dataInvalid && !s2_ldld_violation && !s2_exception

  io.dcache_kill := pmp.ld || pmp.mmio // move pmp resp kill to outside
  io.dcacheResp.ready := true.B
  val dcacheShouldResp = !(s2_tlb_miss || s2_exception || s2_mmio || s2_is_prefetch)
  assert(!(io.in.valid && (dcacheShouldResp && !io.dcacheResp.valid)), "DCache response got lost")

  // merge forward result
  // lsq has higher priority than sbuffer
  val forwardMask = Wire(Vec(8, Bool()))
  val forwardData = Wire(Vec(8, UInt(8.W)))

  val fullForward = ((~forwardMask.asUInt).asUInt & s2_mask) === 0.U && !io.lsq.dataInvalid
  io.lsq := DontCare
  io.sbuffer := DontCare
  io.fullForward := fullForward

  // generate XLEN/8 Muxs
  for (i <- 0 until XLEN / 8) {
    forwardMask(i) := io.lsq.forwardMask(i) || io.sbuffer.forwardMask(i)
    forwardData(i) := Mux(io.lsq.forwardMask(i), io.lsq.forwardData(i), io.sbuffer.forwardData(i))
  }

  XSDebug(io.out.fire, "[FWD LOAD RESP] pc %x fwd %x(%b) + %x(%b)\n",
    s2_uop.cf.pc,
    io.lsq.forwardData.asUInt, io.lsq.forwardMask.asUInt,
    io.in.bits.forwardData.asUInt, io.in.bits.forwardMask.asUInt
  )

  // data merge
  // val rdataVec = VecInit((0 until XLEN / 8).map(j =>
  //   Mux(forwardMask(j), forwardData(j), io.dcacheResp.bits.data(8*(j+1)-1, 8*j))
  // )) // s2_rdataVec will be write to load queue
  // val rdata = rdataVec.asUInt
  // val rdataSel = LookupTree(s2_paddr(2, 0), List(
  //   "b000".U -> rdata(63, 0),
  //   "b001".U -> rdata(63, 8),
  //   "b010".U -> rdata(63, 16),
  //   "b011".U -> rdata(63, 24),
  //   "b100".U -> rdata(63, 32),
  //   "b101".U -> rdata(63, 40),
  //   "b110".U -> rdata(63, 48),
  //   "b111".U -> rdata(63, 56)
  // ))
  // val rdataPartialLoad = rdataHelper(s2_uop, rdataSel) // s2_rdataPartialLoad is not used

  io.out.valid := io.in.valid && !s2_tlb_miss && !s2_data_invalid
  // Inst will be canceled in store queue / lsq,
  // so we do not need to care about flush in load / store unit's out.valid
  io.out.bits := io.in.bits
  // io.out.bits.data := rdataPartialLoad
  io.out.bits.data := 0.U // data will be generated in load_s3
  // when exception occurs, set it to not miss and let it write back to rob (via int port)
  if (EnableFastForward) {
    io.out.bits.miss := s2_cache_miss &&
      !s2_exception &&
      !fullForward &&
      !s2_is_prefetch
  } else {
    io.out.bits.miss := s2_cache_miss &&
      !s2_exception &&
      !s2_is_prefetch
  }
  io.out.bits.uop.ctrl.fpWen := io.in.bits.uop.ctrl.fpWen && !s2_exception

  io.loadDataFromDcache.bankedDcacheData := io.dcacheResp.bits.bank_data
  io.loadDataFromDcache.bank_oh := io.dcacheResp.bits.bank_oh
  // io.loadDataFromDcache.dcacheData := io.dcacheResp.bits.data
  io.loadDataFromDcache.forwardMask := forwardMask
  io.loadDataFromDcache.forwardData := forwardData
  io.loadDataFromDcache.uop := io.out.bits.uop
  io.loadDataFromDcache.addrOffset := s2_paddr(2, 0)

  io.s2_can_replay_from_fetch := !s2_mmio && !s2_is_prefetch && !s2_tlb_miss
  // if forward fail, replay this inst from fetch
  val debug_forwardFailReplay = s2_forward_fail && !s2_mmio && !s2_is_prefetch && !s2_tlb_miss
  // if ld-ld violation is detected, replay from this inst from fetch
  val debug_ldldVioReplay = s2_ldld_violation && !s2_mmio && !s2_is_prefetch && !s2_tlb_miss
  // io.out.bits.uop.ctrl.replayInst := false.B

  io.out.bits.mmio := s2_mmio
  io.out.bits.uop.ctrl.flushPipe := s2_mmio && io.sentFastUop
  io.out.bits.uop.cf.exceptionVec := s2_exception_vec // cache error not included

  // For timing reasons, sometimes we can not let
  // io.out.bits.miss := s2_cache_miss && !s2_exception && !fullForward
  // We use io.dataForwarded instead. It means:
  // 1. Forward logic have prepared all data needed,
  //    and dcache query is no longer needed.
  // 2. ... or data cache tag error is detected, this kind of inst
  //    will not update miss queue. That is to say, if miss, that inst
  //    may not be refilled
  // Such inst will be writebacked from load queue.
  io.dataForwarded := s2_cache_miss && !s2_exception &&
    (fullForward || io.csrCtrl.cache_error_enable && s2_cache_tag_error)
  // io.out.bits.forwardX will be send to lq
  io.out.bits.forwardMask := forwardMask
  // data from dcache is not included in io.out.bits.forwardData
  io.out.bits.forwardData := forwardData

  io.in.ready := io.out.ready || !io.in.valid

  // feedback tlb result to RS
  io.rsFeedback.valid := io.in.valid
  val s2_need_replay_from_rs = Wire(Bool())
  if (EnableFastForward) {
    s2_need_replay_from_rs :=
      s2_tlb_miss || // replay if dtlb miss
      s2_cache_replay && !s2_is_prefetch && !s2_mmio && !s2_exception && !fullForward || // replay if dcache miss queue full / busy
      s2_data_invalid && !s2_is_prefetch // replay if store to load forward data is not ready
  } else {
    // Note that if all parts of data are available in sq / sbuffer, replay required by dcache will not be scheduled
    s2_need_replay_from_rs :=
      s2_tlb_miss || // replay if dtlb miss
      s2_cache_replay && !s2_is_prefetch && !s2_mmio && !s2_exception && !io.dataForwarded || // replay if dcache miss queue full / busy
      s2_data_invalid && !s2_is_prefetch // replay if store to load forward data is not ready
  }
  io.rsFeedback.bits.hit := !s2_need_replay_from_rs
  io.rsFeedback.bits.rsIdx := io.in.bits.rsIdx
  io.rsFeedback.bits.flushState := io.in.bits.ptwBack
  io.rsFeedback.bits.sourceType := Mux(s2_tlb_miss, RSFeedbackType.tlbMiss,
    Mux(s2_cache_replay,
      RSFeedbackType.mshrFull,
      RSFeedbackType.dataInvalid
    )
  )
  io.rsFeedback.bits.dataInvalidSqIdx.value := io.dataInvalidSqIdx
  io.rsFeedback.bits.dataInvalidSqIdx.flag := DontCare

  // s2_cache_replay is quite slow to generate, send it separately to LQ
  if (EnableFastForward) {
    io.s2_dcache_require_replay := s2_cache_replay && !fullForward
  } else {
    io.s2_dcache_require_replay := s2_cache_replay &&
      !io.rsFeedback.bits.hit &&
      !io.dataForwarded &&
      !s2_is_prefetch &&
      io.out.bits.miss
  }

  XSPerfAccumulate("in_valid", io.in.valid)
  XSPerfAccumulate("in_fire", io.in.fire)
  XSPerfAccumulate("in_fire_first_issue", io.in.fire && io.in.bits.isFirstIssue)
  XSPerfAccumulate("dcache_miss", io.in.fire && s2_cache_miss)
  XSPerfAccumulate("dcache_miss_first_issue", io.in.fire && s2_cache_miss && io.in.bits.isFirstIssue)
  XSPerfAccumulate("full_forward", io.in.valid && fullForward)
  XSPerfAccumulate("dcache_miss_full_forward", io.in.valid && s2_cache_miss && fullForward)
  XSPerfAccumulate("replay",  io.rsFeedback.valid && !io.rsFeedback.bits.hit)
  XSPerfAccumulate("replay_tlb_miss", io.rsFeedback.valid && !io.rsFeedback.bits.hit && s2_tlb_miss)
  XSPerfAccumulate("replay_cache", io.rsFeedback.valid && !io.rsFeedback.bits.hit && !s2_tlb_miss && s2_cache_replay)
  XSPerfAccumulate("stall_out", io.out.valid && !io.out.ready)
  XSPerfAccumulate("replay_from_fetch_forward", io.out.valid && debug_forwardFailReplay)
  XSPerfAccumulate("replay_from_fetch_load_vio", io.out.valid && debug_ldldVioReplay)
}

class LoadUnit(implicit p: Parameters) extends XSModule with HasLoadHelper with HasPerfEvents with SdtrigExt {
  val io = IO(new Bundle() {
    val ldin = Flipped(Decoupled(new ExuInput))
    val ldout = Decoupled(new ExuOutput)
    val redirect = Flipped(ValidIO(new Redirect))
    val feedbackSlow = ValidIO(new RSFeedback)
    val feedbackFast = ValidIO(new RSFeedback)
    val rsIdx = Input(UInt(log2Up(IssQueSize).W))
    val isFirstIssue = Input(Bool())
    val dcache = new DCacheLoadIO
    val sbuffer = new LoadForwardQueryIO
    val lsq = new LoadToLsqIO
    val fastUop = ValidIO(new MicroOp) // early wakeup signal generated in load_s1, send to RS in load_s2
    val trigger = Vec(TriggerNum, new LoadUnitTriggerIO)

    val tlb = new TlbRequestIO(2)
    val pmp = Flipped(new PMPRespBundle()) // arrive same to tlb now

    // provide prefetch info
    val prefetch_train = ValidIO(new LsPipelineBundle())

    val fastpathOut = Output(new LoadToLoadIO)
    val fastpathIn = Input(new LoadToLoadIO)
    val loadFastMatch = Input(Bool())
    val loadFastImm = Input(UInt(12.W))

    val s3_delayed_load_error = Output(Bool()) // load ecc error
    // Note that io.s3_delayed_load_error and io.lsq.s3_delayed_load_error is different

    val csrCtrl = Flipped(new CustomCSRCtrlIO)
    val s2IsPointerChasing = Output(Bool())
  })

  val load_s0 = Module(new LoadUnit_S0)
  val load_s1 = Module(new LoadUnit_S1)
  val load_s2 = Module(new LoadUnit_S2)

  // load s0
  load_s0.io.in <> io.ldin
  load_s0.io.dtlbReq <> io.tlb.req
  load_s0.io.dcacheReq <> io.dcache.req
  load_s0.io.rsIdx := io.rsIdx
  load_s0.io.isFirstIssue := io.isFirstIssue
  load_s0.io.s0_kill := false.B
  val s0_tryPointerChasing = !io.ldin.valid && io.fastpathIn.valid
  val s0_pointerChasingVAddr = io.fastpathIn.data(5, 0) +& io.loadFastImm(5, 0)
  load_s0.io.fastpath.valid := io.fastpathIn.valid
  load_s0.io.fastpath.data := Cat(io.fastpathIn.data(XLEN-1, 6), s0_pointerChasingVAddr(5,0))

  val s1_data = PipelineConnect(load_s0.io.out, load_s1.io.in, true.B,
    load_s0.io.out.bits.uop.robIdx.needFlush(io.redirect) && !s0_tryPointerChasing).get

  // load s1
  load_s1.io.s1_kill := RegEnable(load_s0.io.s0_kill, false.B, load_s0.io.in.valid || io.fastpathIn.valid)
  io.tlb.req_kill := load_s1.io.s1_kill
  load_s1.io.dtlbResp <> io.tlb.resp
  io.dcache.s1_paddr_dup_lsu <> load_s1.io.lsuPAddr
  io.dcache.s1_paddr_dup_dcache <> load_s1.io.dcachePAddr
  io.dcache.s1_kill <> load_s1.io.dcacheKill
  load_s1.io.sbuffer <> io.sbuffer
  load_s1.io.lsq <> io.lsq.forward
  load_s1.io.loadViolationQueryReq <> io.lsq.loadViolationQuery.req
  load_s1.io.dcacheBankConflict <> io.dcache.s1_bank_conflict
  load_s1.io.csrCtrl <> io.csrCtrl

  // provide paddr for lq
  io.lsq.loadPaddrIn.valid := load_s1.io.out.valid
  io.lsq.loadPaddrIn.bits.lqIdx := load_s1.io.out.bits.uop.lqIdx
  io.lsq.loadPaddrIn.bits.paddr := load_s1.io.lsuPAddr

  val s0_doTryPointerChasing = s0_tryPointerChasing && load_s0.io.in.ready && load_s0.io.dcacheReq.ready
  val s1_tryPointerChasing = RegNext(s0_doTryPointerChasing, false.B)
  val s1_pointerChasingVAddr = RegEnable(s0_pointerChasingVAddr, s0_doTryPointerChasing)
  val cancelPointerChasing = WireInit(false.B)
  if (EnableLoadToLoadForward) {
    // Sometimes, we need to cancel the load-load forwarding.
    // These can be put at S0 if timing is bad at S1.
    // Case 0: CACHE_SET(base + offset) != CACHE_SET(base) (lowest 6-bit addition has an overflow)
    val addressMisMatch = s1_pointerChasingVAddr(6) || RegEnable(io.loadFastImm(11, 6).orR, s0_doTryPointerChasing)
    // Case 1: the address is not 64-bit aligned or the fuOpType is not LD
    val addressNotAligned = s1_pointerChasingVAddr(2, 0).orR
    val fuOpTypeIsNotLd = io.ldin.bits.uop.ctrl.fuOpType =/= LSUOpType.ld
    // Case 2: this is not a valid load-load pair
    val notFastMatch = RegEnable(!io.loadFastMatch, s0_tryPointerChasing)
    // Case 3: this load-load uop is cancelled
    val isCancelled = !io.ldin.valid
    when (s1_tryPointerChasing) {
      cancelPointerChasing := addressMisMatch || addressNotAligned || fuOpTypeIsNotLd || notFastMatch || isCancelled
      load_s1.io.in.bits.uop := io.ldin.bits.uop
      val spec_vaddr = s1_data.vaddr
      val vaddr = Cat(spec_vaddr(VAddrBits - 1, 6), s1_pointerChasingVAddr(5, 3), 0.U(3.W))
      load_s1.io.in.bits.vaddr := vaddr
      load_s1.io.in.bits.rsIdx := io.rsIdx
      load_s1.io.in.bits.isFirstIssue := io.isFirstIssue
      // We need to replace vaddr(5, 3).
      val spec_paddr = io.tlb.resp.bits.paddr(0)
      load_s1.io.dtlbResp.bits.paddr.foreach(_ := Cat(spec_paddr(PAddrBits - 1, 6), s1_pointerChasingVAddr(5, 3), 0.U(3.W)))
    }
    when (cancelPointerChasing) {
      load_s1.io.s1_kill := true.B
    }.otherwise {
      load_s0.io.s0_kill := s1_tryPointerChasing
      when (s1_tryPointerChasing) {
        io.ldin.ready := true.B
      }
    }

    XSPerfAccumulate("load_to_load_forward", s1_tryPointerChasing && !cancelPointerChasing)
    XSPerfAccumulate("load_to_load_forward_try", s1_tryPointerChasing)
    XSPerfAccumulate("load_to_load_forward_fail", cancelPointerChasing)
    XSPerfAccumulate("load_to_load_forward_fail_cancelled", cancelPointerChasing && isCancelled)
    XSPerfAccumulate("load_to_load_forward_fail_wakeup_mismatch", cancelPointerChasing && !isCancelled && notFastMatch)
    XSPerfAccumulate("load_to_load_forward_fail_op_not_ld",
      cancelPointerChasing && !isCancelled && !notFastMatch && fuOpTypeIsNotLd)
    XSPerfAccumulate("load_to_load_forward_fail_addr_align",
      cancelPointerChasing && !isCancelled && !notFastMatch && !fuOpTypeIsNotLd && addressNotAligned)
    XSPerfAccumulate("load_to_load_forward_fail_set_mismatch",
      cancelPointerChasing && !isCancelled && !notFastMatch && !fuOpTypeIsNotLd && !addressNotAligned && addressMisMatch)
  }

  PipelineConnect(load_s1.io.out, load_s2.io.in, true.B,
    load_s1.io.out.bits.uop.robIdx.needFlush(io.redirect) || cancelPointerChasing)

  // load s2
  io.s2IsPointerChasing := RegEnable(s1_tryPointerChasing && !cancelPointerChasing, load_s1.io.out.fire)
  io.prefetch_train.bits := load_s2.io.in.bits
  // override miss bit
  io.prefetch_train.bits.miss := io.dcache.resp.bits.miss
  io.prefetch_train.valid := load_s2.io.in.fire && !load_s2.io.out.bits.mmio && !load_s2.io.in.bits.tlbMiss
  io.dcache.s2_kill := load_s2.io.dcache_kill // to kill mmio resp which are redirected
  load_s2.io.dcacheResp <> io.dcache.resp
  load_s2.io.pmpResp <> io.pmp
  load_s2.io.static_pm := RegNext(io.tlb.resp.bits.static_pm)
  load_s2.io.lsq.forwardData <> io.lsq.forward.forwardData
  load_s2.io.lsq.forwardMask <> io.lsq.forward.forwardMask
  load_s2.io.lsq.forwardMaskFast <> io.lsq.forward.forwardMaskFast // should not be used in load_s2
  load_s2.io.lsq.dataInvalid <> io.lsq.forward.dataInvalid
  load_s2.io.lsq.matchInvalid <> io.lsq.forward.matchInvalid
  load_s2.io.sbuffer.forwardData <> io.sbuffer.forwardData
  load_s2.io.sbuffer.forwardMask <> io.sbuffer.forwardMask
  load_s2.io.sbuffer.forwardMaskFast <> io.sbuffer.forwardMaskFast // should not be used in load_s2
  load_s2.io.sbuffer.dataInvalid <> io.sbuffer.dataInvalid // always false
  load_s2.io.sbuffer.matchInvalid <> io.sbuffer.matchInvalid
  load_s2.io.dataForwarded <> io.lsq.s2_load_data_forwarded
  load_s2.io.dataInvalidSqIdx := io.lsq.forward.dataInvalidSqIdx // provide dataInvalidSqIdx to make wakeup faster
  load_s2.io.loadViolationQueryResp <> io.lsq.loadViolationQuery.resp
  load_s2.io.csrCtrl <> io.csrCtrl
  load_s2.io.sentFastUop := io.fastUop.valid

  // feedback bank conflict / ld-vio check struct hazard to rs
  io.feedbackFast.bits := RegNext(load_s1.io.rsFeedback.bits)
  io.feedbackFast.valid := RegNext(load_s1.io.rsFeedback.valid && !load_s1.io.out.bits.uop.robIdx.needFlush(io.redirect))

  // pre-calcuate sqIdx mask in s0, then send it to lsq in s1 for forwarding
  val sqIdxMaskReg = RegNext(UIntToMask(load_s0.io.in.bits.uop.sqIdx.value, StoreQueueSize))
  // to enable load-load, sqIdxMask must be calculated based on ldin.uop
  // If the timing here is not OK, load-load forwarding has to be disabled.
  // Or we calculate sqIdxMask at RS??
  io.lsq.forward.sqIdxMask := sqIdxMaskReg
  if (EnableLoadToLoadForward) {
    when (s1_tryPointerChasing) {
      io.lsq.forward.sqIdxMask := UIntToMask(io.ldin.bits.uop.sqIdx.value, StoreQueueSize)
    }
  }

  // generate io.fastUop.valid for RS
  // now io.fastUop.valid is sent to RS in load_s2
  val s2_dcache_hit = io.dcache.s2_hit // dcache hit dup in lsu side

  io.fastUop.valid := RegNext(
      !io.dcache.s1_disable_fast_wakeup &&  // load fast wakeup should be disabled when dcache data read is not ready
      load_s1.io.in.valid && // valid load request
      !load_s1.io.s1_kill && // killed by load-load forwarding
      !load_s1.io.dtlbResp.bits.fast_miss && // not mmio or tlb miss, pf / af not included here
      !io.lsq.forward.dataInvalidFast // forward failed
    ) &&
    !RegNext(load_s1.io.needLdVioCheckRedo) && // load-load violation check: load paddr cam struct hazard
    !RegNext(load_s1.io.out.bits.uop.robIdx.needFlush(io.redirect)) &&
    s2_dcache_hit // dcache hit in lsu side

  io.fastUop.bits := RegNext(load_s1.io.out.bits.uop)

  XSDebug(load_s0.io.out.valid,
    p"S0: pc ${Hexadecimal(load_s0.io.out.bits.uop.cf.pc)}, lId ${Hexadecimal(load_s0.io.out.bits.uop.lqIdx.asUInt)}, " +
    p"vaddr ${Hexadecimal(load_s0.io.out.bits.vaddr)}, mask ${Hexadecimal(load_s0.io.out.bits.mask)}\n")
  XSDebug(load_s1.io.out.valid,
    p"S1: pc ${Hexadecimal(load_s1.io.out.bits.uop.cf.pc)}, lId ${Hexadecimal(load_s1.io.out.bits.uop.lqIdx.asUInt)}, tlb_miss ${io.tlb.resp.bits.miss}, " +
    p"paddr ${Hexadecimal(load_s1.io.out.bits.paddr)}, mmio ${load_s1.io.out.bits.mmio}\n")

  // writeback to LSQ
  // Current dcache use MSHR
  // Load queue will be updated at s2 for both hit/miss int/fp load
  io.lsq.loadIn.valid := load_s2.io.out.valid
  // generate LqWriteBundle from LsPipelineBundle
  io.lsq.loadIn.bits.fromLsPipelineBundle(load_s2.io.out.bits)
  // generate duplicated load queue data wen
  val load_s2_valid_vec = RegInit(0.U(6.W))
  val load_s2_leftFire = load_s1.io.out.valid && load_s2.io.in.ready
  load_s2_valid_vec := 0x0.U(6.W)
  when (load_s2_leftFire) { load_s2_valid_vec := 0x3f.U(6.W)}
  when (load_s1.io.out.bits.uop.robIdx.needFlush(io.redirect)) { load_s2_valid_vec := 0x0.U(6.W) }
  assert(RegNext(load_s2.io.in.valid === load_s2_valid_vec(0)))
  io.lsq.loadIn.bits.lq_data_wen_dup := load_s2_valid_vec.asBools()

  // s2_dcache_require_replay signal will be RegNexted, then used in s3
  io.lsq.s2_dcache_require_replay := load_s2.io.s2_dcache_require_replay

  // write to rob and writeback bus
  val s2_wb_valid = load_s2.io.out.valid && !load_s2.io.out.bits.miss && !load_s2.io.out.bits.mmio

  // Int load, if hit, will be writebacked at s2
  val hitLoadOut = Wire(Valid(new ExuOutput))
  hitLoadOut.valid := s2_wb_valid
  hitLoadOut.bits.uop := load_s2.io.out.bits.uop
  hitLoadOut.bits.data := load_s2.io.out.bits.data
  hitLoadOut.bits.redirectValid := false.B
  hitLoadOut.bits.redirect := DontCare
  hitLoadOut.bits.debug.isMMIO := load_s2.io.out.bits.mmio
  hitLoadOut.bits.debug.isPerfCnt := false.B
  hitLoadOut.bits.debug.paddr := load_s2.io.out.bits.paddr
  hitLoadOut.bits.debug.vaddr := load_s2.io.out.bits.vaddr
  hitLoadOut.bits.fflags := DontCare

  load_s2.io.out.ready := true.B

  // load s3
  val s3_load_wb_meta_reg = RegNext(Mux(hitLoadOut.valid, hitLoadOut.bits, io.lsq.ldout.bits))

  // data from load queue refill
  val s3_loadDataFromLQ = RegEnable(io.lsq.ldRawData, io.lsq.ldout.valid)
  val s3_rdataLQ = s3_loadDataFromLQ.mergedData()
  val s3_rdataSelLQ = LookupTree(s3_loadDataFromLQ.addrOffset, List(
    "b000".U -> s3_rdataLQ(63, 0),
    "b001".U -> s3_rdataLQ(63, 8),
    "b010".U -> s3_rdataLQ(63, 16),
    "b011".U -> s3_rdataLQ(63, 24),
    "b100".U -> s3_rdataLQ(63, 32),
    "b101".U -> s3_rdataLQ(63, 40),
    "b110".U -> s3_rdataLQ(63, 48),
    "b111".U -> s3_rdataLQ(63, 56)
  ))
  val s3_rdataPartialLoadLQ = rdataHelper(s3_loadDataFromLQ.uop, s3_rdataSelLQ)

  // data from dcache hit
  val s3_loadDataFromDcache = RegEnable(load_s2.io.loadDataFromDcache, load_s2.io.in.valid)
  val s3_rdataDcache = s3_loadDataFromDcache.mergedData()
  val s3_rdataSelDcache = LookupTree(s3_loadDataFromDcache.addrOffset, List(
    "b000".U -> s3_rdataDcache(63, 0),
    "b001".U -> s3_rdataDcache(63, 8),
    "b010".U -> s3_rdataDcache(63, 16),
    "b011".U -> s3_rdataDcache(63, 24),
    "b100".U -> s3_rdataDcache(63, 32),
    "b101".U -> s3_rdataDcache(63, 40),
    "b110".U -> s3_rdataDcache(63, 48),
    "b111".U -> s3_rdataDcache(63, 56)
  ))
  val s3_rdataPartialLoadDcache = rdataHelper(s3_loadDataFromDcache.uop, s3_rdataSelDcache)

  io.ldout.bits := s3_load_wb_meta_reg
  io.ldout.bits.data := Mux(RegNext(hitLoadOut.valid), s3_rdataPartialLoadDcache, s3_rdataPartialLoadLQ)
  io.ldout.valid := RegNext(hitLoadOut.valid) && !RegNext(load_s2.io.out.bits.uop.robIdx.needFlush(io.redirect)) ||
    RegNext(io.lsq.ldout.valid) && !RegNext(io.lsq.ldout.bits.uop.robIdx.needFlush(io.redirect)) && !RegNext(hitLoadOut.valid)

  io.ldout.bits.uop.cf.exceptionVec(loadAccessFault) := s3_load_wb_meta_reg.uop.cf.exceptionVec(loadAccessFault) //||
    //RegNext(hitLoadOut.valid) && load_s2.io.s3_delayed_load_error

  // fast load to load forward
  io.fastpathOut.valid := RegNext(load_s2.io.out.valid) // for debug only
  io.fastpathOut.data := s3_loadDataFromDcache.mergedData() // fastpath is for ld only

  // feedback tlb miss / dcache miss queue full
  io.feedbackSlow.bits := RegNext(load_s2.io.rsFeedback.bits)
  io.feedbackSlow.valid := RegNext(load_s2.io.rsFeedback.valid && !load_s2.io.out.bits.uop.robIdx.needFlush(io.redirect))
  // If replay is reported at load_s1, inst will be canceled (will not enter load_s2),
  // in that case:
  // * replay should not be reported twice
  assert(!(RegNext(io.feedbackFast.valid) && io.feedbackSlow.valid))
  // * io.fastUop.valid should not be reported
  assert(!RegNext(io.feedbackFast.valid && io.fastUop.valid))

  // load forward_fail/ldld_violation check
  // check for inst in load pipeline
  val s3_forward_fail = RegNext(io.lsq.forward.matchInvalid || io.sbuffer.matchInvalid)
  val s3_ldld_violation = RegNext(
    io.lsq.loadViolationQuery.resp.valid &&
    io.lsq.loadViolationQuery.resp.bits.have_violation &&
    RegNext(io.csrCtrl.ldld_vio_check_enable)
  )
  val s3_need_replay_from_fetch = s3_forward_fail || s3_ldld_violation
  val s3_can_replay_from_fetch = RegEnable(load_s2.io.s2_can_replay_from_fetch, load_s2.io.out.valid)
  // 1) use load pipe check result generated in load_s3 iff load_hit
  when (RegNext(hitLoadOut.valid)) {
    io.ldout.bits.uop.ctrl.replayInst := s3_need_replay_from_fetch
  }
  // 2) otherwise, write check result to load queue
  io.lsq.s3_replay_from_fetch := s3_need_replay_from_fetch && s3_can_replay_from_fetch

  // s3_delayed_load_error path is not used for now, as we writeback load result in load_s3
  // but we keep this path for future use
  io.s3_delayed_load_error := false.B
  io.lsq.s3_delayed_load_error := false.B //load_s2.io.s3_delayed_load_error

  io.lsq.ldout.ready := !hitLoadOut.valid

  when(io.feedbackSlow.valid && !io.feedbackSlow.bits.hit){
    assert(RegNext(!hitLoadOut.valid))
    assert(RegNext(!io.lsq.loadIn.valid) || RegNext(load_s2.io.s2_dcache_require_replay))
  }

  val lastValidData = RegEnable(io.ldout.bits.data, io.ldout.fire)
  val hitLoadAddrTriggerHitVec = Wire(Vec(TriggerNum, Bool()))
  val lqLoadAddrTriggerHitVec = io.lsq.trigger.lqLoadAddrTriggerHitVec
  (0 until TriggerNum).map{i => {
    val tdata2 = io.trigger(i).tdata2
    val matchType = io.trigger(i).matchType
    val tEnable = io.trigger(i).tEnable

    hitLoadAddrTriggerHitVec(i) := TriggerCmp(load_s2.io.out.bits.vaddr, tdata2, matchType, tEnable)
    io.trigger(i).addrHit := RegNext(Mux(hitLoadOut.valid, hitLoadAddrTriggerHitVec(i), lqLoadAddrTriggerHitVec(i)))
    io.trigger(i).lastDataHit := TriggerCmp(lastValidData, tdata2, matchType, tEnable)
  }}
  io.lsq.trigger.hitLoadAddrTriggerHitVec := hitLoadAddrTriggerHitVec

  val perfEvents = Seq(
    ("load_s0_in_fire         ", load_s0.io.in.fire                                                                                                              ),
    ("load_to_load_forward    ", load_s1.io.out.valid && s1_tryPointerChasing && !cancelPointerChasing                                                           ),
    ("stall_dcache            ", load_s0.io.out.valid && load_s0.io.out.ready && !load_s0.io.dcacheReq.ready                                                     ),
    ("load_s1_in_fire         ", load_s1.io.in.fire                                                                                                              ),
    ("load_s1_tlb_miss        ", load_s1.io.in.fire && load_s1.io.dtlbResp.bits.miss                                                                             ),
    ("load_s2_in_fire         ", load_s2.io.in.fire                                                                                                              ),
    ("load_s2_dcache_miss     ", load_s2.io.in.fire && load_s2.io.dcacheResp.bits.miss                                                                           ),
    ("load_s2_replay          ", load_s2.io.rsFeedback.valid && !load_s2.io.rsFeedback.bits.hit                                                                  ),
    ("load_s2_replay_tlb_miss ", load_s2.io.rsFeedback.valid && !load_s2.io.rsFeedback.bits.hit && load_s2.io.in.bits.tlbMiss                                    ),
    ("load_s2_replay_cache    ", load_s2.io.rsFeedback.valid && !load_s2.io.rsFeedback.bits.hit && !load_s2.io.in.bits.tlbMiss && load_s2.io.dcacheResp.bits.miss),
  )
  generatePerfEvent()

  when(io.ldout.fire){
    XSDebug("ldout %x\n", io.ldout.bits.uop.cf.pc)
  }
}
