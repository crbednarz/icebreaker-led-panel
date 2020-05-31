package esc

import spinal.core._
import spinal.lib.graphic._
import spinal.lib.slave


case class Frame() extends Component {
  val io = new Bundle {
    val inColor = slave Stream(Rgb(RgbConfig(8, 8, 8)))

    val read = new Bundle() {
      val address = in UInt(12 bits)
      val color = out(Rgb(RgbConfig(5, 6, 5)))
    }
  }
  val buffer = DualSPRAM()
  val flipBuffers = Reg(Bool) init(False)
  buffer.io.swapped := flipBuffers

  val input = new Area {
    val writeAddress = Reg(UInt(12 bits)) init(0)
    buffer.io.write.address := writeAddress.resize(14)
    buffer.io.write.enable := io.inColor.valid
    buffer.io.write.dataIn := (io.inColor.payload.r >> 3) @@ (io.inColor.payload.g >> 2) @@ (io.inColor.payload.b >> 3)

    io.inColor.ready := True
    when (io.inColor.valid) {
      writeAddress := writeAddress + 1
      when (~writeAddress === 0) {
        flipBuffers := ~flipBuffers
      }
    }
  }

  val output = new Area {
    buffer.io.read.address := io.read.address.resized
    buffer.io.read.enable := True
    io.read.color.r := buffer.io.read.dataOut(15 downto 11)
    io.read.color.g := buffer.io.read.dataOut(10 downto 5)
    io.read.color.b := buffer.io.read.dataOut(4 downto 0)
  }
}
