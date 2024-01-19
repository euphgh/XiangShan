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

package top

import org.chipsalliance.cde.config.Parameters
import chisel3._
import device.{AXI4RAMWrapper, SimJTAG}
import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule, LazyModuleImp}
import utility._
import xiangshan.{DebugOptions, DebugOptionsKey}
import freechips.rocketchip.devices.debug._
import difftest._
import freechips.rocketchip.util.ElaborationArtefacts
import top.XiangShanStage
import chisel3.stage.ChiselGeneratorAnnotation

class SimTop(implicit p: Parameters) extends Module {
  val debugOpts = p(DebugOptionsKey)
  val useDRAMSim = debugOpts.UseDRAMSim

  val l_soc = LazyModule(new XSTop())
  val soc = Module(l_soc.module)

  l_soc.module.dma <> 0.U.asTypeOf(l_soc.module.dma)

  val l_simMMIO = LazyModule(new SimMMIO(l_soc.misc.peripheralNode.in.head._2))
  val simMMIO = Module(l_simMMIO.module)
  l_simMMIO.io_axi4 <> soc.peripheral

  if(!useDRAMSim){
    val l_simAXIMem = LazyModule(new AXI4RAMWrapper(
      l_soc.misc.memAXI4SlaveNode, 16L * 1024 * 1024 * 1024, useBlackBox = true
    ))
    val simAXIMem = Module(l_simAXIMem.module)
    l_simAXIMem.io_axi4 <> soc.memory
  }
  else {
    io.memAXI <> soc.memory
  }

  val io = IO(new Bundle(){
    val logCtrl = new LogCtrlIO
    val perfInfo = new PerfInfoIO
    val uart = new UARTIO
    val memAXI = if(useDRAMSim) soc.memory.cloneType else null
  })

  soc.io.clock := clock.asBool
  soc.io.reset := reset.asAsyncReset
  soc.io.extIntrs := simMMIO.io.interrupt.intrVec
  soc.io.sram_config := 0.U
  soc.io.pll0_lock := true.B
  soc.io.cacheable_check := DontCare

  val success = Wire(Bool())
  val jtag = Module(new SimJTAG(tickDelay=3)(p)).connect(soc.io.systemjtag.jtag, clock, reset.asBool, !reset.asBool, success)
  soc.io.systemjtag.reset := reset.asAsyncReset
  soc.io.systemjtag.mfr_id := 0.U(11.W)
  soc.io.systemjtag.part_number := 0.U(16.W)
  soc.io.systemjtag.version := 0.U(4.W)

  val difftest = DifftestModule.finish("XiangShan")

  simMMIO.io.uart <> io.uart

  val hasPerf = !debugOpts.FPGAPlatform && debugOpts.EnablePerfDebug
  val hasLog = !debugOpts.FPGAPlatform && debugOpts.EnableDebug
  val hasPerfLog = hasPerf || hasLog
  val timer = if (hasPerfLog) GTimer() else WireDefault(0.U(64.W))
  val logEnable = if (hasPerfLog) WireDefault((timer >= io.logCtrl.log_begin) && (timer < io.logCtrl.log_end))
    else WireDefault(false.B)
  val clean = if (hasPerf) WireDefault(io.perfInfo.clean) else WireDefault(false.B)
  val dump = if (hasPerf) WireDefault(io.perfInfo.dump) else WireDefault(false.B)

  dontTouch(timer)
  dontTouch(logEnable)
  dontTouch(clean)
  dontTouch(dump)
}

object SimTop extends App {
    // Keep this the same as TopMain except that SimTop is used here instead of XSTop
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  // tools: init to close dpi-c when in fpga
  val envInFPGA = config(DebugOptionsKey).FPGAPlatform
  val enableChiselDB = config(DebugOptionsKey).EnableChiselDB
  val enableConstantin = config(DebugOptionsKey).EnableConstantin
  Constantin.init(enableConstantin && !envInFPGA)
  ChiselDB.init(enableChiselDB && !envInFPGA)

  Generator.execute(
    firrtlOpts,
    DisableMonitors(p => new SimTop()(p))(config),
    firtoolOpts
  )

  // tools: write cpp files
  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write(fileDir = "./build")
}