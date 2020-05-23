package esc

import spinal.core._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}
import spinal.lib.com.uart._
import spinal.lib.fsm.{EntryPoint, StateMachine}
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
    io.leds := 0


    val uartCtrl = new UartCtrl()
    uartCtrl.io.config.setClockDivider(921600 Hz)
    uartCtrl.io.config.frame.dataLength := 7
    uartCtrl.io.config.frame.parity := UartParityType.NONE
    uartCtrl.io.config.frame.stop := UartStopType.ONE
    uartCtrl.io.uart <> io.uart
    uartCtrl.io.writeBreak := False


    val test = Reg(Rgb(RgbConfig(8, 8, 8)))
    test.r init(0)
    test.g init(0)
    test.b init(0)

    val counter = Reg(UInt(2 bits)) init(0)
    val data = Reg(Vec(Bits(8 bits), 3))
    val colorStream = Stream(Rgb(RgbConfig(8, 8, 8)))

    when (uartCtrl.io.read.valid) {
      data(counter) := uartCtrl.io.read.payload
      counter := counter + 1
    }

    when (counter === 3) {
      counter := 0
      colorStream.payload.r := U(data(0))
      colorStream.payload.g := U(data(1))
      colorStream.payload.b := U(data(2))
      colorStream.valid := True
    } otherwise {
      colorStream.payload.r := 0
      colorStream.payload.g := 0
      colorStream.payload.b := 0
      colorStream.valid := False
    }

    val write = Stream(Bits(8 bits))
    write.valid := CounterFreeRun(2000).willOverflow
    write.payload := 0
    write >-> uartCtrl.io.write

    val ctrl = LedPanelController()
    ctrl.io.led <> io.led
    colorStream >> ctrl.io.color
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