package esc

import spinal.core._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}
import spinal.lib.com.uart._
import spinal.lib.graphic.{Rgb, RgbConfig}
import spinal.lib.{CounterFreeRun, Stream, master}

import sys.process._

class Top extends Component {
  val io = new Bundle {
    val led = LedPanel()
    val uart = master(Uart())
    val leds = out Bits(5 bits)
    val buttons = in Bits(3 bits)
  }

  val clockControl = new Area {
    val pll = PLL30MHz()
    pll.io.clockIn := ClockDomain.current.readClockWire
    val domain = ClockDomain.internal(
      name = "core",
      frequency = FixedFrequency(30 MHz)
    )
    domain.clock := pll.io.clockOut
  }

  val core = new ClockingArea(clockControl.domain) {
    val uartCtrl = new UartCtrl()
    uartCtrl.io.config.setClockDivider(9600 Hz)
    uartCtrl.io.config.frame.dataLength := 7  //8 bits
    uartCtrl.io.config.frame.parity := UartParityType.NONE
    uartCtrl.io.config.frame.stop := UartStopType.ONE
    uartCtrl.io.uart <> io.uart
    uartCtrl.io.writeBreak := False
    io.leds := uartCtrl.io.read.toFlow.toReg()(4 downto 0)

    val write = Stream(Bits(8 bits))
    write.valid := CounterFreeRun(2000).willOverflow
    write.payload := 0
    write >-> uartCtrl.io.write

    val ctrl = LedPanelController()
    ctrl.io.led <> io.led
  }

}

object ProjectConfig extends SpinalConfig(
  defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT),
  defaultClockDomainFrequency = FixedFrequency(12 MHz))

object MyTopLevelVerilogWithCustomConfig {
  def main(args: Array[String]) {
    ProjectConfig.generateVerilog(new Top)
    "build.bat" !
  }
}