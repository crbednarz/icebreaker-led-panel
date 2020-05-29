package esc

import spinal.core._

case class SPRAM() extends Component {
  val io = new Bundle {
    val address = in UInt(14 bits)
    val writeEnable = in Bool
    val enable = in Bool
    val dataIn = in UInt(16 bits)
    val dataOut = out UInt(16 bits)
  }
  val ram = SB_SPRAM256KA()
  ram.io.ADDRESS <> io.address
  ram.io.DATAIN <> io.dataIn
  ram.io.DATAOUT <> io.dataOut
  ram.io.MASKWREN <> U"1111"
  ram.io.WREN <> io.writeEnable
  ram.io.CHIPSELECT <> io.enable
  ram.io.STANDBY <> False
  ram.io.SLEEP <> False
  ram.io.POWEROFF <> False
}

case class SB_SPRAM256KA() extends BlackBox {
  val io = new Bundle {
    val ADDRESS = in UInt(14 bits)
    val DATAIN = in UInt(16 bits)
    val DATAOUT = out UInt(16 bits)
    val MASKWREN = in UInt(4 bits)
    val WREN = in Bool
    val CHIPSELECT = in Bool
    val CLOCK = in Bool
    val STANDBY = in Bool
    val SLEEP = in Bool
    val POWEROFF = in Bool
  }
  mapClockDomain(clock=io.CLOCK)
  noIoPrefix()
}
