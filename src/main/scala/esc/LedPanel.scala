package esc

import spinal.core._
import spinal.lib.{Reverse, master, slave}
import spinal.lib.fsm._
import spinal.lib.graphic.{Rgb, RgbConfig}

case class LedPanel() extends Bundle {
  val address = out UInt(5 bits)
  val top = out(Rgb(RgbConfig(1, 1, 1)))
  val bottom = out(Rgb(RgbConfig(1, 1, 1)))
  val clock = out Bool
  val latch = out Bool
  val blank = out Bool
}

case class ColorToPwm() extends Component {
  val io = new Bundle {
    val frameCount = in UInt(8 bits)
    val inColor = in(Rgb(RgbConfig(5, 6, 5)))
    val outColor = out(Rgb(RgbConfig(1, 1, 1)))
  }
  io.outColor.r(0) := (io.inColor.r > Reverse(io.frameCount(4 downto 0)))
  io.outColor.g(0) := (io.inColor.g > Reverse(io.frameCount(5 downto 0)))
  io.outColor.b(0) := (io.inColor.b > Reverse(io.frameCount(4 downto 0)))
}

case class Frame() extends Component {
  val io = new Bundle {
    val inColor = slave Stream(Rgb(RgbConfig(8, 8, 8)))

    val read = new Bundle() {
      val row = in UInt(5 bits)
      val column = in UInt(6 bits)
      val top = out(Rgb(RgbConfig(5, 6, 5)))
      val bottom = out(Rgb(RgbConfig(5, 6, 5)))
    }
  }
  val frame = Mem(Rgb(RgbConfig(5, 6, 5)), 64 * 64)

  val input = new Area {
    val writeAddress = Reg(UInt(12 bits)) init(0)
    io.inColor.ready := True
    when (io.inColor.valid) {
      val reducedColor = Rgb(RgbConfig(5, 6, 5))
      reducedColor.r := io.inColor.payload.r >> 3
      reducedColor.g := io.inColor.payload.g >> 2
      reducedColor.b := io.inColor.payload.b >> 3
      frame(writeAddress) := reducedColor
      writeAddress := writeAddress + 1
    }
  }

  val topColor = Reg(Rgb(RgbConfig(5, 6, 5)))
  val bottomColor = Reg(Rgb(RgbConfig(5, 6, 5)))
  val output = new Area {
    val readAddress = UInt(11 bits)
    readAddress := io.read.row @@ io.read.column
    topColor := frame(U"0" @@ readAddress)
    bottomColor := frame(U"1" @@ readAddress)
    io.read.top := topColor
    io.read.bottom := bottomColor
  }
}

case class LedPanelController() extends Component {
  val io = new Bundle {
    val ledPanel = LedPanel()
    val readRow = out UInt(5 bits)
    val readColumn = out UInt(6 bits)
    val topColor = in(Rgb(RgbConfig(5, 6, 5)))
    val bottomColor = in(Rgb(RgbConfig(5, 6, 5)))
  }

  val row = Reg(UInt(5 bits))
  io.ledPanel.address <> row - 1

  val topShift = Reg(Rgb(RgbConfig(1, 1, 1)))
  io.ledPanel.top <> topShift

  val bottomShift = Reg(Rgb(RgbConfig(1, 1, 1)))
  io.ledPanel.bottom <> bottomShift

  val latch = Reg(Bool()) init(True)
  io.ledPanel.latch <> latch

  val blank = Reg(Bool()) init(True)
  io.ledPanel.blank <> blank

  val clockEnabled = Reg(Bool) init(False)
  io.ledPanel.clock <> (ClockDomain.current.readClockWire && clockEnabled)

  val column = Reg(UInt(6 bits)) init(0)
  val frameCount = Reg(UInt(32 bits)) init(0)

  io.readColumn <> column
  io.readRow <> row

  val topPainter = ColorToPwm()
  topPainter.io.frameCount := frameCount(7 downto 0)
  topPainter.io.inColor <> io.topColor
  topShift <> topPainter.io.outColor

  val bottomPainter = ColorToPwm()
  bottomPainter.io.frameCount <> frameCount(7 downto 0)
  bottomPainter.io.inColor <> io.bottomColor
  bottomShift <> bottomPainter.io.outColor

  val stateMachine = new StateMachine {
    val blankState = new State
    val latchState = new State
    val unlatchState = new State
    val unblankState = new State
    val shiftState = new State with EntryPoint
    val finishShiftState = new State

    blankState.whenIsActive {
      blank := True
      goto(latchState)
    }

    latchState.whenIsActive {
      latch := True
      row := row + 1
      goto(unlatchState)
    }

    unlatchState.whenIsActive {
      latch := False
      goto(unblankState)
    }

    unblankState.whenIsActive {
      blank := False
      when (row === 31) {
        frameCount := frameCount + 1
      }
      goto(shiftState)
    }

    shiftState
      .onEntry(column := 1)
      .whenIsActive {
        clockEnabled := True
        column := column + 1
        when (column === 0)  {
          column := 0
          goto(finishShiftState)
        }
      }

    finishShiftState.whenIsActive {
      clockEnabled := False
      goto(blankState)
    }
  }
}

case class BufferedLedPanelController() extends Component {
  val io = new Bundle {
    val ledPanel = LedPanel()
    val colorStream = slave Stream(Rgb(RgbConfig(8, 8, 8)))
  }

  val panelCtrl = LedPanelController()
  panelCtrl.io.ledPanel <> io.ledPanel

  val frame = Frame()
  frame.io.inColor <> io.colorStream
  frame.io.read.row := panelCtrl.io.readRow
  frame.io.read.column := panelCtrl.io.readColumn
  panelCtrl.io.bottomColor <> frame.io.read.bottom
  panelCtrl.io.topColor <> frame.io.read.top

}