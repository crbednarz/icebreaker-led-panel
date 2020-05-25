package esc

import spinal.core._
import spinal.lib.{Reverse, master, slave}
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

case class Frame() extends Component {
  val io = new Bundle {
    val inColor = slave Stream(Rgb(RgbConfig(8, 8, 8)))
    val readRow = in UInt(5 bits)
    val readColumn = in UInt(6 bits)
    val topColor = out(Rgb(RgbConfig(8, 8, 8)))
    val bottomColor = out(Rgb(RgbConfig(8, 8, 8)))
  }

  val frame = Mem(Rgb(RgbConfig(8, 8, 8)), 32 * 32)

  val input = new Area {
    val writeAddress = Reg(UInt(10 bits)) init(0)
    io.inColor.ready := True
    when (io.inColor.valid) {
      frame(writeAddress) := io.inColor.payload
      writeAddress := writeAddress + 1
    }
  }

  val topColor = Reg(Rgb(RgbConfig(8, 8, 8)))
  val bottomColor = Reg(Rgb(RgbConfig(8, 8, 8)))
  val output = new Area {
    val readAddress = UInt(9 bits)
    readAddress := (io.readRow >> 1) @@ (io.readColumn >> 1)
    topColor := frame(U"0" @@ readAddress)
    bottomColor := frame(U"1" @@ readAddress)

    io.topColor := topColor
    io.bottomColor := bottomColor
  }
}

case class LedPanelController() extends Component {
  val io = new Bundle {
    val ledPanel = LedPanel()
    val colorStream = slave Stream(Rgb(RgbConfig(8, 8, 8)))
  }

  val row = Reg(UInt(5 bits)) init(0)
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
  io.ledPanel.address <> row - 1
  io.ledPanel.top <> led.top
  io.ledPanel.bottom <> led.bottom
  io.ledPanel.latch <> led.latch
  io.ledPanel.blank <> led.blank
  val column = Reg(UInt(6 bits)) init(0)
  val clockEnabled = Reg(Bool) init(False)
  val frameCount = Reg(UInt(32 bits)) init(0)

  val topPainter = PwmPainter()
  topPainter.io.frameCount := frameCount(7 downto 0)
  led.top <> topPainter.io.outColor

  val bottomPainter = PwmPainter()
  bottomPainter.io.frameCount <> frameCount(7 downto 0)
  led.bottom <> bottomPainter.io.outColor

  val ddr = DDR()
  ddr.io.clockIn <> ClockDomain.current.readClockWire
  ddr.io.clockEnable <> clockEnabled
  io.ledPanel.clock <> ddr.io.clockOut

  val frame = Frame()
  frame.io.inColor <> io.colorStream
  frame.io.readRow := row
  frame.io.readColumn := column
  bottomPainter.io.inColor <> frame.io.bottomColor
  topPainter.io.inColor <> frame.io.topColor

  val stateMachine = new StateMachine {
    val blankState = new State
    val latchState = new State
    val unlatchState = new State
    val unblankState = new State
    val shiftState = new State with EntryPoint
    val finishShiftState = new State

    blankState.whenIsActive {
      led.blank := True
      goto(latchState)
    }

    latchState.whenIsActive {
      led.latch := True
      row := row + 1
      goto(unlatchState)
    }

    unlatchState.whenIsActive {
      led.latch := False
      goto(unblankState)
    }

    unblankState.whenIsActive {
      led.blank := False
      when (row === 31) {
        frameCount := frameCount + 1
      }
      goto(shiftState)
    }

    shiftState
      .whenIsActive {
        clockEnabled := True
        column := column + 1
        when (column === 63)  {
          goto(finishShiftState)
        }
      }

    finishShiftState.whenIsActive {
      clockEnabled := False
      goto(blankState)
    }
  }
}