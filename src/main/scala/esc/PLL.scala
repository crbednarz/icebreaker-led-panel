package esc

import spinal.core._

case class PLL30MHz() extends BlackBox {
  val io = new Bundle {
    val clockIn = in Bool
    val clockOut = out Bool
  }
  noIoPrefix()
}

case class PLL60MHz() extends BlackBox {
  val io = new Bundle {
    val clockIn = in Bool
    val clockOut = out Bool
  }
  noIoPrefix()
}

case class PLL90MHz() extends BlackBox {
  val io = new Bundle {
    val clockIn = in Bool
    val clockOut = out Bool
  }
  noIoPrefix()
}