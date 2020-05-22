package esc

import spinal.core._

case class RgbShift() extends Bundle {
  val r = out Bool
  val g = out Bool
  val b = out Bool
}