package esc

import spinal.core._
import spinal.lib.graphic._
import spinal.lib.slave


case class Frame() extends Component {
  val io = new Bundle {
    val inColor = slave Stream(Rgb(RgbConfig(8, 8, 8)))

    val read = new Bundle() {
      val row = in UInt(6 bits)
      val column = in UInt(6 bits)
      val color = out(Rgb(RgbConfig(5, 6, 5)))
    }
  }
  val buffer = DualSPRAM()
  val flipBuffers = Reg(Bool) init(False)
  buffer.io.swapped := flipBuffers

  val input = new Area {
    val writeAddress = Reg(UInt(12 bits)) init(0)
    val writeEnable = Reg(Bool) init(True)
    buffer.io.write.address <> writeAddress.resize(14)
    buffer.io.write.enable <> writeEnable

    io.inColor.ready := True
    when (io.inColor.valid) {
      val reducedColor = UInt(16 bits)
      reducedColor := (io.inColor.payload.r >> 3) @@ (io.inColor.payload.g >> 2) @@ (io.inColor.payload.b >> 3)
      buffer.io.write.dataIn <> reducedColor

      writeAddress := writeAddress + 1
      when (~writeAddress === 0) {
        flipBuffers := ~flipBuffers
      }

      writeEnable := True
    } otherwise {
      buffer.io.write.dataIn <> 0
      writeEnable := False
    }
  }

  val output = new Area {
    val color = Rgb(RgbConfig(5, 6, 5))

    val readAddress = UInt(12 bits)
    readAddress := io.read.row @@ io.read.column

    buffer.io.read.address := readAddress.resize(14)
    buffer.io.read.enable := True

    color.r := buffer.io.read.dataOut(15 downto 11)
    color.g := buffer.io.read.dataOut(10 downto  5)
    color.b := buffer.io.read.dataOut(4 downto 0)

    io.read.color := color
  }
}
