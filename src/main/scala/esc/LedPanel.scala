package esc

import spinal.core._

case class LedPanel() extends Bundle {
  val address = out UInt(5 bits)
  val top = RgbShift()
  val bottom = RgbShift()
  val clock = out Bool
  val latch = out Bool
  val blank = out Bool
}