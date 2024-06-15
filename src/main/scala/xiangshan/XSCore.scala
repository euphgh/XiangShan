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

package xiangshan

import org.chipsalliance.cde.config
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}
import freechips.rocketchip.tile.HasFPUParameters
import freechips.rocketchip.tilelink.TLBuffer
import huancun.utils.{ModuleNode, ResetGen, ResetGenNode}
import system.HasSoCParameter
import utils._
import utility._
import xiangshan.backend._
import xiangshan.backend.exu.{ExuConfig, Wb2Ctrl, WbArbiterWrapper}
import xiangshan.cache.mmu._
import xiangshan.frontend._

import scala.collection.mutable.ListBuffer

abstract class XSModule(implicit val p: Parameters) extends Module
  with HasXSParameter
  with HasFPUParameters

//remove this trait after impl module logic
trait NeedImpl {
  this: RawModule =>
  protected def IO[T <: Data](iodef: T): T = {
    println(s"[Warn]: (${this.name}) please reomve 'NeedImpl' after implement this module")
    val io = chisel3.IO(iodef)
    io <> DontCare
    io
  }
}

class WritebackSourceParams(
  var exuConfigs: Seq[Seq[ExuConfig]] = Seq()
 ) {
  def length: Int = exuConfigs.length
  def ++(that: WritebackSourceParams): WritebackSourceParams = {
    new WritebackSourceParams(exuConfigs ++ that.exuConfigs)
  }
}

trait HasWritebackSource {
  val writebackSourceParams: Seq[WritebackSourceParams]
  final def writebackSource(sourceMod: HasWritebackSourceImp): Seq[Seq[Valid[ExuOutput]]] = {
    require(sourceMod.writebackSource.isDefined, "should not use Valid[ExuOutput]")
    val source = sourceMod.writebackSource.get
    require(source.length == writebackSourceParams.length, "length mismatch between sources")
    for ((s, p) <- source.zip(writebackSourceParams)) {
      require(s.length == p.length, "params do not match with the exuOutput")
    }
    source
  }
  final def writebackSource1(sourceMod: HasWritebackSourceImp): Seq[Seq[DecoupledIO[ExuOutput]]] = {
    require(sourceMod.writebackSource1.isDefined, "should not use DecoupledIO[ExuOutput]")
    val source = sourceMod.writebackSource1.get
    require(source.length == writebackSourceParams.length, "length mismatch between sources")
    for ((s, p) <- source.zip(writebackSourceParams)) {
      require(s.length == p.length, "params do not match with the exuOutput")
    }
    source
  }
  val writebackSourceImp: HasWritebackSourceImp
}

trait HasWritebackSourceImp {
  def writebackSource: Option[Seq[Seq[Valid[ExuOutput]]]] = None
  def writebackSource1: Option[Seq[Seq[DecoupledIO[ExuOutput]]]] = None
}

trait HasWritebackSink {
  // Caches all sources. The selected source will be the one with smallest length.
  var writebackSinks = ListBuffer.empty[(Seq[HasWritebackSource], Seq[Int])]
  def addWritebackSink(source: Seq[HasWritebackSource], index: Option[Seq[Int]] = None): HasWritebackSink = {
    val realIndex = if (index.isDefined) index.get else Seq.fill(source.length)(0)
    writebackSinks += ((source, realIndex))
    this
  }

  def writebackSinksParams: Seq[WritebackSourceParams] = {
    writebackSinks.map{ case (s, i) => s.zip(i).map(x => x._1.writebackSourceParams(x._2)).reduce(_ ++ _) }.toSeq
  }
  final def writebackSinksMod(
     thisMod: Option[HasWritebackSource] = None,
     thisModImp: Option[HasWritebackSourceImp] = None
   ): Seq[Seq[HasWritebackSourceImp]] = {
    require(thisMod.isDefined == thisModImp.isDefined)
    writebackSinks.map(_._1.map(source =>
      if (thisMod.isDefined && source == thisMod.get) thisModImp.get else source.writebackSourceImp)
    ).toSeq
  }
  final def writebackSinksImp(
    thisMod: Option[HasWritebackSource] = None,
    thisModImp: Option[HasWritebackSourceImp] = None
  ): Seq[Seq[ValidIO[ExuOutput]]] = {
    val sourceMod = writebackSinksMod(thisMod, thisModImp)
    writebackSinks.zip(sourceMod).map{ case ((s, i), m) =>
      s.zip(i).zip(m).flatMap(x => x._1._1.writebackSource(x._2)(x._1._2))
    }.toSeq
  }
  def selWritebackSinks(func: WritebackSourceParams => Int): Int = {
    writebackSinksParams.zipWithIndex.minBy(params => func(params._1))._2
  }
  def generateWritebackIO(
    thisMod: Option[HasWritebackSource] = None,
    thisModImp: Option[HasWritebackSourceImp] = None
   ): Unit
}

abstract class XSBundle(implicit val p: Parameters) extends Bundle
  with HasXSParameter

abstract class XSCoreBase()(implicit p: config.Parameters) extends LazyModule
  with HasXSParameter with HasExuWbHelper
{
  override def shouldBeInlined: Boolean = false
  // interrupt sinks
  val clint_int_sink = IntSinkNode(IntSinkPortSimple(1, 2))
  val debug_int_sink = IntSinkNode(IntSinkPortSimple(1, 1))
  val plic_int_sink = IntSinkNode(IntSinkPortSimple(2, 1))
  // outer facing nodes
  val frontend = LazyModule(new Frontend())
  val ptw = LazyModule(new L2TLBWrapper())
  val ptw_to_l2_buffer = LazyModule(new TLBuffer)
  val csrOut = BundleBridgeSource(Some(() => new DistributedCSRIO()))
  ptw_to_l2_buffer.node := ptw.node
  val memBlock = LazyModule(new MemBlock())
}

class XSCore()(implicit p: config.Parameters) extends XSCoreBase
  with HasXSDts
{
  lazy val module = new XSCoreImp(this)
}

class XSCoreImp(outer: XSCoreBase) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasSoCParameter {
  val io = IO(new Bundle {
    val hartId = Input(UInt(64.W))
    val cpu_halt = Output(Bool())
    val l2_pf_enable = Output(Bool())
    val perfEvents = Input(Vec(numPCntHc * coreParams.L2NBanks, new PerfEvent))
    val beu_errors = Output(new XSL1BusErrors())
  })
  io <> 0.U.asTypeOf(io)
  dontTouch(io)
}
