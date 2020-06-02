package esc

import spinal.core._
import spinal.lib.com.uart._
import spinal.lib._

import sys.process._

class Top extends Component {
  val io = new Bundle {
    val ledPanel = LedPanel()
    val uart = master(Uart())
    val leds = out Bits(5 bits)
  }

  val clockControl = new Area {
    val pll = PLL60MHz()
    pll.io.clockIn := ClockDomain.current.readClockWire
    val domain = ClockDomain.internal(
      name = "core",
      frequency = FixedFrequency(60 MHz)
    )
    domain.clock := pll.io.clockOut
  }

  val core = new ClockingArea(clockControl.domain) {
    io.leds(0) := ClockDomain.current.readClockWire
    io.leds(1) := ClockDomain.current.readClockWire
    io.leds(2) := ClockDomain.current.readClockWire
    io.leds(3) := ClockDomain.current.readClockWire
    io.leds(4) := ClockDomain.current.readClockWire
    val uartRgbReader = UartRgbReader()
    uartRgbReader.io.uart <> io.uart

    val panelCtrl = BufferedLedPanelController()
    panelCtrl.io.ledPanel <> io.ledPanel
    panelCtrl.io.write << uartRgbReader.io.color
    panelCtrl.io.present <> uartRgbReader.io.frameComplete
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