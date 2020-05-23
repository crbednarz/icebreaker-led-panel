package esc

import spinal.core._
import spinal.lib.{Reverse, slave}
import spinal.lib.fsm._
import spinal.lib.graphic.{Rgb, RgbConfig}

case class LedPanel() extends Bundle {
  val address = out UInt(5 bits)
  val top = RgbShift()
  val bottom = RgbShift()
  val clock = out Bool
  val latch = out Bool
  val blank = out Bool
}

case class PwmPainter() extends Component {
  val io = new Bundle {
    val frameCount = in UInt(8 bits)
    val inColor = in(Rgb(RgbConfig(8, 8, 8)))
    val outColor = out(RgbShift())
  }
  io.outColor.r := (io.inColor.r > Reverse(io.frameCount))
  io.outColor.g := (io.inColor.g > Reverse(io.frameCount))
  io.outColor.b := (io.inColor.b > Reverse(io.frameCount))
}

case class LedPanelController() extends Component {
  val io = new Bundle {
    val led = LedPanel()
    val color = slave Stream(Rgb(RgbConfig(8, 8, 8)))
  }

  val led = Reg(LedPanel())
  led.address init(0)
  led.top.r init(False)
  led.top.g init(False)
  led.top.b init(False)
  led.bottom.r init(False)
  led.bottom.g init(False)
  led.bottom.b init(False)
  led.latch init(True)
  led.blank init(True)
  io.led.address <> led.address
  io.led.top <> led.top
  io.led.bottom <> led.bottom
  io.led.latch <> led.latch
  io.led.blank <> led.blank

  val frame = Mem(Rgb(RgbConfig(8, 8, 8)), 32 * 32)

  val row = Reg(UInt(5 bits)) init(0)
  val column = Reg(UInt(7 bits))
  val clockEnabled = Reg(Bool) init(False)
  val frameCount = Reg(UInt(32 bits)) init(0)
  val readAddress = UInt(10 bits)
  val writeAddress = Reg(UInt(10 bits)) init(0)

  val topColor = Reg(Rgb(RgbConfig(8, 8, 8)))
  val topPainter = PwmPainter()
  topPainter.io.frameCount <> frameCount(7 downto 0)
  led.top <> topPainter.io.outColor
  topPainter.io.inColor <> topColor

  val bottomColor = Reg(Rgb(RgbConfig(8, 8, 8)))
  val bottomPainter = PwmPainter()
  bottomPainter.io.frameCount <> frameCount(7 downto 0)
  led.bottom <> bottomPainter.io.outColor
  bottomPainter.io.inColor <> bottomColor

  readAddress := (row >> 1).resize(5) @@ (column >> 1).resize(5)
  topColor := frame(readAddress)
  bottomColor := frame(readAddress + 512)

  when (io.color.valid) {
    frame(writeAddress) := io.color.payload
    writeAddress := writeAddress + 1
  }

  val ddr = DDR()
  ddr.io.clockIn <> ClockDomain.current.readClockWire
  ddr.io.clockEnable <> clockEnabled
  io.led.clock <> ddr.io.clockOut

  val stateMachine = new StateMachine {
    val blankState = new State with EntryPoint
    val latchState = new State
    val unlatchState = new State
    val unblankState = new State
    val shiftState = new State

    blankState.whenIsActive {
      led.blank := True
      led.address := row
      goto(latchState)
    }

    latchState.whenIsActive {
      led.latch := True
      goto(unlatchState)
    }

    unlatchState.whenIsActive {
      led.latch := False
      goto(unblankState)
    }

    unblankState.whenIsActive {
      led.blank := False
      row := row + 1
      when (row === 31) {
        frameCount := frameCount + 1
      }
      goto(shiftState)
    }

    shiftState
      .onEntry {
        column := 0
        readAddress := 0
        clockEnabled := True
      }
      .whenIsActive {
        column := column + 1
        when (column(6)) {
          goto(blankState)
        }
      }
      .onExit {
        clockEnabled := False

      }
  }
}