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

package xiangshan.cache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util._
import xiangshan._
import utils._
import utility._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{BundleFieldBase, UIntToOH1}
import huancun.{AliasField, AliasKey, DirtyField, PreferCacheField, PrefetchField}
import mem.{AddPipelineReg}

import scala.math.max

// DCache specific parameters
case class DCacheParameters
(
  nSets: Int = 256,
  nWays: Int = 8,
  rowBits: Int = 64,
  tagECC: Option[String] = None,
  dataECC: Option[String] = None,
  replacer: Option[String] = Some("setplru"),
  nMissEntries: Int = 1,
  nProbeEntries: Int = 1,
  nReleaseEntries: Int = 1,
  nMMIOEntries: Int = 1,
  nMMIOs: Int = 1,
  blockBytes: Int = 64,
  alwaysReleaseData: Boolean = true
) extends L1CacheParameters {
  // if sets * blockBytes > 4KB(page size),
  // cache alias will happen,
  // we need to avoid this by recoding additional bits in L2 cache
  val setBytes = nSets * blockBytes
  val aliasBitsOpt = if(setBytes > pageSize) Some(log2Ceil(setBytes / pageSize)) else None
  val reqFields: Seq[BundleFieldBase] = Seq(
    PrefetchField(),
    PreferCacheField()
  ) ++ aliasBitsOpt.map(AliasField)
  val echoFields: Seq[BundleFieldBase] = Seq(DirtyField())

  def tagCode: Code = Code.fromString(tagECC)

  def dataCode: Code = Code.fromString(dataECC)
}

//           Physical Address
// --------------------------------------
// |   Physical Tag |  PIndex  | Offset |
// --------------------------------------
//                  |
//                  DCacheTagOffset
//
//           Virtual Address
// --------------------------------------
// | Above index  | Set | Bank | Offset |
// --------------------------------------
//                |     |      |        |
//                |     |      |        0
//                |     |      DCacheBankOffset
//                |     DCacheSetOffset
//                DCacheAboveIndexOffset

// Default DCache size = 64 sets * 8 ways * 8 banks * 8 Byte = 32K Byte

trait HasDCacheParameters extends HasL1CacheParameters {
  val cacheParams = dcacheParameters
  val cfg = cacheParams

  def encWordBits = cacheParams.dataCode.width(wordBits)

  def encRowBits = encWordBits * rowWords // for DuplicatedDataArray only
  def eccBits = encWordBits - wordBits

  def encTagBits = cacheParams.tagCode.width(tagBits)
  def eccTagBits = encTagBits - tagBits

  def blockProbeAfterGrantCycles = 8 // give the processor some time to issue a request after a grant

  def nSourceType = 3
  def sourceTypeWidth = log2Up(nSourceType)
  def LOAD_SOURCE = 0
  def STORE_SOURCE = 1
  def AMO_SOURCE = 2
  def SOFT_PREFETCH = 3

  // each source use a id to distinguish its multiple reqs
  def reqIdWidth = log2Up(nEntries) max log2Up(StoreBufferSize)

  require(isPow2(cfg.nMissEntries)) // TODO
  // require(isPow2(cfg.nReleaseEntries))
  require(cfg.nMissEntries < cfg.nReleaseEntries)
  val nEntries = cfg.nMissEntries + cfg.nReleaseEntries
  val releaseIdBase = cfg.nMissEntries

  // banked dcache support
  val DCacheSets = cacheParams.nSets
  val DCacheWays = cacheParams.nWays
  val DCacheBanks = 8 // hardcoded
  val DCacheSRAMRowBits = cacheParams.rowBits // hardcoded
  val DCacheWordBits = 64 // hardcoded
  val DCacheWordBytes = DCacheWordBits / 8
  require(DCacheSRAMRowBits == 64)

  val DCacheSizeBits = DCacheSRAMRowBits * DCacheBanks * DCacheWays * DCacheSets
  val DCacheSizeBytes = DCacheSizeBits / 8
  val DCacheSizeWords = DCacheSizeBits / 64 // TODO

  val DCacheSameVPAddrLength = 12

  val DCacheSRAMRowBytes = DCacheSRAMRowBits / 8
  val DCacheWordOffset = log2Up(DCacheWordBytes)

  val DCacheBankOffset = log2Up(DCacheSRAMRowBytes)
  val DCacheSetOffset = DCacheBankOffset + log2Up(DCacheBanks)
  val DCacheAboveIndexOffset = DCacheSetOffset + log2Up(DCacheSets)
  val DCacheTagOffset = DCacheAboveIndexOffset min DCacheSameVPAddrLength
  val DCacheLineOffset = DCacheSetOffset

  // parameters about duplicating regs to solve fanout
  // In Main Pipe:
    // tag_write.ready -> data_write.valid * 8 banks
    // tag_write.ready -> meta_write.valid
    // tag_write.ready -> tag_write.valid
    // tag_write.ready -> err_write.valid
    // tag_write.ready -> wb.valid
  val nDupTagWriteReady = DCacheBanks + 4
  // In Main Pipe:
    // data_write.ready -> data_write.valid * 8 banks
    // data_write.ready -> meta_write.valid
    // data_write.ready -> tag_write.valid
    // data_write.ready -> err_write.valid
    // data_write.ready -> wb.valid
  val nDupDataWriteReady = DCacheBanks + 4
  val nDupWbReady = DCacheBanks + 4
  val nDupStatus = nDupTagWriteReady + nDupDataWriteReady
  val dataWritePort = 0
  val metaWritePort = DCacheBanks
  val tagWritePort = metaWritePort + 1
  val errWritePort = tagWritePort + 1
  val wbPort = errWritePort + 1

  def addr_to_dcache_bank(addr: UInt) = {
    require(addr.getWidth >= DCacheSetOffset)
    addr(DCacheSetOffset-1, DCacheBankOffset)
  }

  def addr_to_dcache_set(addr: UInt) = {
    require(addr.getWidth >= DCacheAboveIndexOffset)
    addr(DCacheAboveIndexOffset-1, DCacheSetOffset)
  }

  def get_data_of_bank(bank: Int, data: UInt) = {
    require(data.getWidth >= (bank+1)*DCacheSRAMRowBits)
    data(DCacheSRAMRowBits * (bank + 1) - 1, DCacheSRAMRowBits * bank)
  }

  def get_mask_of_bank(bank: Int, data: UInt) = {
    require(data.getWidth >= (bank+1)*DCacheSRAMRowBytes)
    data(DCacheSRAMRowBytes * (bank + 1) - 1, DCacheSRAMRowBytes * bank)
  }

  def arbiter[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    out <> arb.io.out
  }

  def arbiter_with_pipereg[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    AddPipelineReg(arb.io.out, out, false.B)
  }

  def arbiter_with_pipereg_N_dup[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    dups: Seq[DecoupledIO[T]],
    name: Option[String] = None): Unit = {
    val arb = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    for (dup <- dups) {
      AddPipelineReg(arb.io.out, dup, false.B)
    }
    AddPipelineReg(arb.io.out, out, false.B)
  }

  def rrArbiter[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new RRArbiterInit[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    out <> arb.io.out
  }

  def fastArbiter[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new FastArbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    out <> arb.io.out
  }

  val numReplaceRespPorts = 2

  require(isPow2(nSets), s"nSets($nSets) must be pow2")
  require(isPow2(nWays), s"nWays($nWays) must be pow2")
  require(full_divide(rowBits, wordBits), s"rowBits($rowBits) must be multiple of wordBits($wordBits)")
  require(full_divide(beatBits, rowBits), s"beatBits($beatBits) must be multiple of rowBits($rowBits)")
}

abstract class DCacheModule(implicit p: Parameters) extends L1CacheModule
  with HasDCacheParameters

abstract class DCacheBundle(implicit p: Parameters) extends L1CacheBundle
  with HasDCacheParameters

class ReplacementAccessBundle(implicit p: Parameters) extends DCacheBundle {
  val set = UInt(log2Up(nSets).W)
  val way = UInt(log2Up(nWays).W)
}

class ReplacementWayReqIO(implicit p: Parameters) extends DCacheBundle {
  val set = ValidIO(UInt(log2Up(nSets).W))
  val way = Input(UInt(log2Up(nWays).W))
}

// memory request in word granularity(load, mmio, lr/sc, atomics)
class DCacheWordReq(implicit p: Parameters)  extends DCacheBundle
{
  val cmd    = UInt(M_SZ.W)
  val addr   = UInt(PAddrBits.W)
  val data   = UInt(DataBits.W)
  val mask   = UInt((DataBits/8).W)
  val id     = UInt(reqIdWidth.W)
  val instrtype   = UInt(sourceTypeWidth.W)
  def dump() = {
    XSDebug("DCacheWordReq: cmd: %x addr: %x data: %x mask: %x id: %d\n",
      cmd, addr, data, mask, id)
  }
}

// memory request in word granularity(store)
class DCacheLineReq(implicit p: Parameters)  extends DCacheBundle
{
  val cmd    = UInt(M_SZ.W)
  val vaddr  = UInt(VAddrBits.W)
  val addr   = UInt(PAddrBits.W)
  val data   = UInt((cfg.blockBytes * 8).W)
  val mask   = UInt(cfg.blockBytes.W)
  val id     = UInt(reqIdWidth.W)
  def dump() = {
    XSDebug("DCacheLineReq: cmd: %x addr: %x data: %x mask: %x id: %d\n",
      cmd, addr, data, mask, id)
  }
  def idx: UInt = get_idx(vaddr)
}

class DCacheWordReqWithVaddr(implicit p: Parameters) extends DCacheWordReq {
  val vaddr = UInt(VAddrBits.W)
  val wline = Bool()
}

class BaseDCacheWordResp(implicit p: Parameters) extends DCacheBundle
{
  val data   = UInt(DataBits.W)
  val id     = UInt(reqIdWidth.W)

  // cache req missed, send it to miss queue
  val miss   = Bool()
  // cache miss, and failed to enter the missqueue, replay from RS is needed
  val replay = Bool()
  // data has been corrupted
  val tag_error = Bool() // tag error
  def dump() = {
    XSDebug("DCacheWordResp: data: %x id: %d miss: %b replay: %b\n",
      data, id, miss, replay)
  }
}

class DCacheWordResp(implicit p: Parameters) extends BaseDCacheWordResp
{
  // 1 cycle after data resp
  val error_delayed = Bool() // all kinds of errors, include tag error
}

class BankedDCacheWordResp(implicit p: Parameters) extends DCacheWordResp
{
  val bank_data = Vec(DCacheBanks, Bits(DCacheSRAMRowBits.W))
  val bank_oh = UInt(DCacheBanks.W)
}

class DCacheWordRespWithError(implicit p: Parameters) extends BaseDCacheWordResp
{
  val error = Bool() // all kinds of errors, include tag error
}

class DCacheLineResp(implicit p: Parameters) extends DCacheBundle
{
  val data   = UInt((cfg.blockBytes * 8).W)
  // cache req missed, send it to miss queue
  val miss   = Bool()
  // cache req nacked, replay it later
  val replay = Bool()
  val id     = UInt(reqIdWidth.W)
  def dump() = {
    XSDebug("DCacheLineResp: data: %x id: %d miss: %b replay: %b\n",
      data, id, miss, replay)
  }
}

class Refill(implicit p: Parameters) extends DCacheBundle
{
  val addr   = UInt(PAddrBits.W)
  val data   = UInt(l1BusDataWidth.W)
  val error  = Bool() // refilled data has been corrupted
  // for debug usage
  val data_raw = UInt((cfg.blockBytes * 8).W)
  val hasdata = Bool()
  val refill_done = Bool()
  def dump() = {
    XSDebug("Refill: addr: %x data: %x\n", addr, data)
  }
}

class Release(implicit p: Parameters) extends DCacheBundle
{
  val paddr  = UInt(PAddrBits.W)
  def dump() = {
    XSDebug("Release: paddr: %x\n", paddr(PAddrBits-1, DCacheTagOffset))
  }
}

class DCacheWordIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheWordReq)
  val resp = Flipped(DecoupledIO(new BankedDCacheWordResp))
}

class UncacheWordIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheWordReq)
  val resp = Flipped(DecoupledIO(new DCacheWordRespWithError))
}

class AtomicWordIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheWordReqWithVaddr)
  val resp = Flipped(DecoupledIO(new DCacheWordRespWithError))
}

// used by load unit
class DCacheLoadIO(implicit p: Parameters) extends DCacheWordIO
{
  // kill previous cycle's req
  val s1_kill  = Output(Bool())
  val s2_kill  = Output(Bool())
  // cycle 0: virtual address: req.addr
  // cycle 1: physical address: s1_paddr
  val s1_paddr_dup_lsu = Output(UInt(PAddrBits.W)) // lsu side paddr
  val s1_paddr_dup_dcache = Output(UInt(PAddrBits.W)) // dcache side paddr
  val s1_disable_fast_wakeup = Input(Bool())
  val s1_bank_conflict = Input(Bool())
  // cycle 2: hit signal
  val s2_hit = Input(Bool()) // hit signal for lsu,

  // debug
  val debug_s1_hit_way = Input(UInt(nWays.W))
}

class DCacheLineIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheLineReq)
  val resp = Flipped(DecoupledIO(new DCacheLineResp))
}

class DCacheToSbufferIO(implicit p: Parameters) extends DCacheBundle {
  // sbuffer will directly send request to dcache main pipe
  val req = Flipped(Decoupled(new DCacheLineReq))

  val main_pipe_hit_resp = ValidIO(new DCacheLineResp)
  val refill_hit_resp = ValidIO(new DCacheLineResp)

  val replay_resp = ValidIO(new DCacheLineResp)

  def hit_resps: Seq[ValidIO[DCacheLineResp]] = Seq(main_pipe_hit_resp, refill_hit_resp)
}

class DCacheToLsuIO(implicit p: Parameters) extends DCacheBundle {
  val load  = Vec(LoadPipelineWidth, Flipped(new DCacheLoadIO)) // for speculative load
  val lsq = ValidIO(new Refill)  // refill to load queue, wake up load misses
  val store = new DCacheToSbufferIO // for sbuffer
  val atomics  = Flipped(new AtomicWordIO)  // atomics reqs
  val release = ValidIO(new Release) // cacheline release hint for ld-ld violation check
}

class DCacheIO(implicit p: Parameters) extends DCacheBundle {
  val hartId = Input(UInt(8.W))
  val l2_pf_store_only = Input(Bool())
  val lsu = new DCacheToLsuIO
  val csr = new L1CacheToCsrIO
  val error = new L1CacheErrorInfo
  val mshrFull = Output(Bool())
}


class DCache()(implicit p: Parameters) extends LazyModule with HasDCacheParameters {
  override def shouldBeInlined: Boolean = false

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "dcache",
      sourceId = IdRange(0, nEntries + 1),
      supportsProbe = TransferSizes(cfg.blockBytes)
    )),
    requestFields = cacheParams.reqFields,
    echoFields = cacheParams.echoFields
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new DCacheImp(this)
}


class DCacheImp(outer: DCache) extends LazyModuleImp(outer) with HasDCacheParameters with HasPerfEvents {
  val perfEvents = Seq()
  generatePerfEvent()
}

class AMOHelper() extends ExtModule {
  val clock  = IO(Input(Clock()))
  val enable = IO(Input(Bool()))
  val cmd    = IO(Input(UInt(5.W)))
  val addr   = IO(Input(UInt(64.W)))
  val wdata  = IO(Input(UInt(64.W)))
  val mask   = IO(Input(UInt(8.W)))
  val rdata  = IO(Output(UInt(64.W)))
}

class DCacheWrapper()(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  val useDcache = coreParams.dcacheParametersOpt.nonEmpty
  val clientNode = if (useDcache) TLIdentityNode() else null
  val dcache = if (useDcache) LazyModule(new DCache()) else null
  if (useDcache) {
    clientNode := dcache.clientNode
  }

  class DCacheWrapperImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) with HasPerfEvents {
    val perfEvents = Seq()
    generatePerfEvent()
  }

  lazy val module = new DCacheWrapperImp(this)
}
