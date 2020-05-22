package esc

import spinal.core._


case class DDR() extends BlackBox {
  val io = new Bundle {
    val clockIn = in Bool
    val clockEnable = in Bool
    val clockOut = out Bool
  }
  noIoPrefix()
}