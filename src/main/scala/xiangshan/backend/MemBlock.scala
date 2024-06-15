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

package xiangshan.backend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.HasFPUParameters
import huancun.PrefetchRecv
import utils._
import utility._
import xiangshan._
import xiangshan.backend.exu.StdExeUnit
import xiangshan.backend.fu._
import xiangshan.backend.fu.util.SdtrigExt
import xiangshan.backend.rob.RobLsqIO
import xiangshan.cache._
import xiangshan.cache.mmu.{VectorTlbPtwIO, TLBNonBlock, TlbReplace}
import xiangshan.mem._
import xiangshan.mem.prefetch.{BasePrefecher, SMSParams, SMSPrefetcher}

class Std(implicit p: Parameters) extends FunctionUnit {
  io.in.ready := true.B
  io.out.valid := io.in.valid
  io.out.bits.uop := io.in.bits.uop
  io.out.bits.data := io.in.bits.src(0)
}

class MemBlock()(implicit p: Parameters) extends LazyModule
  with HasXSParameter with HasWritebackSource {
  override def shouldBeInlined: Boolean = false

  val dcache = LazyModule(new DCacheWrapper())
  val uncache = LazyModule(new Uncache())
  val pf_sender_opt = coreParams.prefetcher.map(_ =>
    BundleBridgeSource(() => new PrefetchRecv)
  )

  lazy val module = new MemBlockImp(this)

  override val writebackSourceParams: Seq[WritebackSourceParams] = {
    val params = new WritebackSourceParams
    params.exuConfigs = (loadExuConfigs ++ storeExuConfigs).map(cfg => Seq(cfg))
    Seq(params)
  }
  override lazy val writebackSourceImp: HasWritebackSourceImp = module
}

class MemBlockImp(outer: MemBlock) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasFPUParameters
  with HasWritebackSourceImp
  with HasPerfEvents
  with SdtrigExt
{

  override def writebackSource1: Option[Seq[Seq[DecoupledIO[ExuOutput]]]] = Some(Seq())

  val perfEvents = HPerfMonitor(Seq(), Seq()).getPerfEvents
  generatePerfEvent()
}
