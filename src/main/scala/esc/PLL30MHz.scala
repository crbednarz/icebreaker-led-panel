package esc

import spinal.core._

case class PLL30MHz() extends BlackBox {
  val io = new Bundle {
    val clockIn = in Bool
    val clockOut = out Bool
  }
  noIoPrefix()
}