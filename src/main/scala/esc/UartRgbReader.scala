package esc

import spinal.core._
import spinal.lib.com.uart._
import spinal.lib.graphic.{Rgb, RgbConfig}
import spinal.lib._

case class UartRgbReader() extends Component {
  val io = new Bundle {
    val uart = master(Uart())
    val color = master Flow(Rgb(RgbConfig(8, 8, 8)))
  }

  val uartCtrl = new UartCtrl()
  uartCtrl.io.config.setClockDivider(921600*4 Hz)
  uartCtrl.io.config.frame.dataLength := 7
  uartCtrl.io.config.frame.parity := UartParityType.NONE
  uartCtrl.io.config.frame.stop := UartStopType.ONE
  uartCtrl.io.uart <> io.uart
  uartCtrl.io.writeBreak := False

  val counter = Reg(UInt(2 bits)) init(0)
  val data = Reg(Vec(Bits(8 bits), 3))
  val colorFlow = master Flow(Rgb(RgbConfig(8, 8, 8)))

  uartCtrl.io.read.ready := True
  when (uartCtrl.io.read.valid) {
    data(counter) := uartCtrl.io.read.payload
    counter := counter + 1
  }

  io.color.payload.r := U(data(0))
  io.color.payload.g := U(data(1))
  io.color.payload.b := U(data(2))

  when (counter === 3) {
    counter := 0
    io.color.valid := True
  } otherwise {
    io.color.valid := False
  }

  val write = Stream(Bits(8 bits))
  write.valid := CounterFreeRun(2000).willOverflow
  write.payload := 0
  write >-> uartCtrl.io.write
}
