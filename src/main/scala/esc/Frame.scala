package esc

import spinal.core._
import spinal.lib.graphic._
import spinal.lib._

case class FrameWrite() extends Bundle {
  val address = UInt(12 bits)
  val color = Rgb(8, 8, 8)
}

case class Frame() extends Component {
  val io = new Bundle {
    val present = in Bool
    val write = slave Flow(FrameWrite())
    val read = new Bundle() {
      val address = in UInt(12 bits)
      val color = out(Rgb(5, 6, 5))
    }
  }
  val buffer = DualSPRAM()
  val flipBuffers = Reg(Bool) init(False)
  buffer.io.swapped := flipBuffers

  when (io.present) {
    flipBuffers := !flipBuffers
  }

  val input = new Area {
    val writeAddress = io.write.payload.address
    buffer.io.write.address := writeAddress.resize(14)
    buffer.io.write.enable := io.write.valid
    buffer.io.write.dataIn := (io.write.payload.color.r >> 3) @@ (io.write.payload.color.g >> 2) @@ (io.write.payload.color.b >> 3)
  }

  val output = new Area {
    buffer.io.read.address := io.read.address.resized
    buffer.io.read.enable := True
    io.read.color.r := buffer.io.read.dataOut(15 downto 11)
    io.read.color.g := buffer.io.read.dataOut(10 downto 5)
    io.read.color.b := buffer.io.read.dataOut(4 downto 0)
  }
}
