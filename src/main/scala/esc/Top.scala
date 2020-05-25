package esc

import spinal.core._
import spinal.lib.com.uart._
import spinal.lib._

import sys.process._

class Top extends Component {
  val io = new Bundle {
    val ledPanel = LedPanel()
    val uart = master(Uart())
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
    val uartRgbReader = UartRgbReader()
    uartRgbReader.io.uart <> io.uart

    val panelCtrl = LedPanelController()
    panelCtrl.io.ledPanel <> io.ledPanel
    panelCtrl.io.colorStream << uartRgbReader.io.color.toStream
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