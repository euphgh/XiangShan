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

package xiangshan.cache.mmu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util._

import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import utility._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import xiangshan.backend.fu.{PMP, PMPChecker, PMPReqBundle, PMPRespBundle}
import xiangshan.backend.fu.util.HasCSRConst
import difftest._

class L2TLB()(implicit p: Parameters) extends LazyModule with HasPtwConst {
  override def shouldBeInlined: Boolean = false

  val node = TLClientNode(Seq(TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "ptw",
      sourceId = IdRange(0, MemReqWidth)
    )),
    requestFields = Seq(ReqSourceField())
  )))

  lazy val module = new L2TLBImp(this)
}


class L2TLBImp(outer: L2TLB)(implicit p: Parameters) extends PtwModule(outer) with HasCSRConst with HasPerfEvents {
  val perfEvents  = Seq()
  generatePerfEvent()
}

/** BlockHelper, block missqueue, not to send too many req to cache
 *  Parameter:
 *    enable: enable BlockHelper, mq should not send too many reqs
 *    start: when miss queue out fire and need, block miss queue's out
 *    block: block miss queue's out
 *    latency: last missqueue out's cache access latency
 */
class BlockHelper(latency: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val start = Input(Bool())
    val block = Output(Bool())
  })

  val count = RegInit(0.U(log2Ceil(latency).W))
  val valid = RegInit(false.B)
  val work = RegInit(true.B)

  io.block := valid

  when (io.start && work) { valid := true.B }
  when (valid) { count := count + 1.U }
  when (count === (latency.U) || io.enable) {
    valid := false.B
    work := io.enable
    count := 0.U
  }
}

class PTEHelper() extends ExtModule {
  val clock  = IO(Input(Clock()))
  val enable = IO(Input(Bool()))
  val satp   = IO(Input(UInt(64.W)))
  val vpn    = IO(Input(UInt(64.W)))
  val pte    = IO(Output(UInt(64.W)))
  val level  = IO(Output(UInt(8.W)))
  val pf     = IO(Output(UInt(8.W)))
}

class PTWDelayN[T <: Data](gen: T, n: Int, flush: Bool) extends Module {
  val io = IO(new Bundle() {
    val in = Input(gen)
    val out = Output(gen)
    val ptwflush = Input(flush.cloneType)
  })
  val out = RegInit(VecInit(Seq.fill(n)(0.U.asTypeOf(gen))))
  val t = RegInit(VecInit(Seq.fill(n)(0.U.asTypeOf(gen))))
  out(0) := io.in
  if (n == 1) {
    io.out := out(0)
  } else {
    when (io.ptwflush) {
      for (i <- 0 until n) {
        t(i) := 0.U.asTypeOf(gen)
        out(i) := 0.U.asTypeOf(gen)
      }
      io.out := 0.U.asTypeOf(gen)
    } .otherwise {
      for (i <- 1 until n) {
        t(i-1) := out(i-1)
        out(i) := t(i-1)
      }
      io.out := out(n-1)
    }
  }
}

object PTWDelayN {
  def apply[T <: Data](in: T, n: Int, flush: Bool): T = {
    val delay = Module(new PTWDelayN(in.cloneType, n, flush))
    delay.io.in := in
    delay.io.ptwflush := flush
    delay.io.out
  }
}

class FakePTW()(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new L2TLBIO)
  val flush = VecInit(Seq.fill(PtwWidth)(false.B))
  flush(0) := DelayN(io.sfence.valid || io.csr.tlb.satp.changed, itlbParams.fenceDelay)
  flush(1) := DelayN(io.sfence.valid || io.csr.tlb.satp.changed, ldtlbParams.fenceDelay)
  for (i <- 0 until PtwWidth) {
    val helper = Module(new PTEHelper())
    helper.clock := clock
    helper.satp := io.csr.tlb.satp.ppn

    if (coreParams.softPTWDelay == 1) {
      helper.enable := io.tlb(i).req(0).fire
      helper.vpn := io.tlb(i).req(0).bits.vpn
    } else {
      helper.enable := PTWDelayN(io.tlb(i).req(0).fire, coreParams.softPTWDelay - 1, flush(i))
      helper.vpn := PTWDelayN(io.tlb(i).req(0).bits.vpn, coreParams.softPTWDelay - 1, flush(i))
    }

    val pte = helper.pte.asTypeOf(new PteBundle)
    val level = helper.level
    val pf = helper.pf
    val empty = RegInit(true.B)
    when (io.tlb(i).req(0).fire) {
      empty := false.B
    } .elsewhen (io.tlb(i).resp.fire || flush(i)) {
      empty := true.B
    }

    io.tlb(i).req(0).ready := empty || io.tlb(i).resp.fire
    io.tlb(i).resp.valid := PTWDelayN(io.tlb(i).req(0).fire, coreParams.softPTWDelay, flush(i))
    assert(!io.tlb(i).resp.valid || io.tlb(i).resp.ready)
    io.tlb(i).resp.bits.s1.entry.tag := PTWDelayN(io.tlb(i).req(0).bits.vpn, coreParams.softPTWDelay, flush(i))
    io.tlb(i).resp.bits.s1.entry.ppn := pte.ppn
    io.tlb(i).resp.bits.s1.entry.perm.map(_ := pte.getPerm())
    io.tlb(i).resp.bits.s1.entry.level.map(_ := level)
    io.tlb(i).resp.bits.s1.pf := pf
    io.tlb(i).resp.bits.s1.af := DontCare // TODO: implement it
    io.tlb(i).resp.bits.s1.entry.v := !pf
    io.tlb(i).resp.bits.s1.entry.prefetch := DontCare
    io.tlb(i).resp.bits.s1.entry.asid := io.csr.tlb.satp.asid
  }
}

class L2TLBWrapper()(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false
  val useSoftPTW = coreParams.softPTW
  val node = if (!useSoftPTW) TLIdentityNode() else null
  val ptw = if (!useSoftPTW) LazyModule(new L2TLB()) else null
  if (!useSoftPTW) {
    node := ptw.node
  }

  class L2TLBWrapperImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) with HasPerfEvents {
    val perfEvents = Seq()
    generatePerfEvent()
  }

  lazy val module = new L2TLBWrapperImp(this)
}