package esc

import spinal.core._
import spinal.lib._
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

case class ColorPair() extends Bundle {
  val top = Rgb(RgbConfig(5, 6, 5))
  val bottom = Rgb(RgbConfig(5, 6, 5))
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

case class LedPanelController() extends Component {
  val io = new Bundle {
    val ledPanel = LedPanel()
    val colorStream = slave Stream(ColorPair())
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

  val column = Reg(UInt(6 bits)) init(0)
  val frameCount = Reg(UInt(32 bits)) init(0)

  val topPainter = ColorToPwm()
  val topColor = Reg(Rgb(RgbConfig(5, 6, 5)))
  topPainter.io.frameCount := frameCount(7 downto 0)
  topPainter.io.inColor <> topColor
  topShift <> topPainter.io.outColor

  val bottomPainter = ColorToPwm()
  val bottomColor = Reg(Rgb(RgbConfig(5, 6, 5)))
  bottomPainter.io.frameCount <> frameCount(7 downto 0)
  bottomPainter.io.inColor <> bottomColor
  bottomShift <> bottomPainter.io.outColor

  val slowClock = Reg(Bool) init(False)
  slowClock := ~slowClock

  val stateMachine = new StateMachine {
    val shiftState = new State
    val finishShiftState = new State
    val blankState = new State
    val latchState = new State
    val unlatchState = new State
    val unblankState = new State with EntryPoint

    val consumeStream = Reg(Bool) init(False)

    io.colorStream.ready := consumeStream
    io.ledPanel.clock := ~consumeStream

    shiftState
      .onEntry {
        column := 0
      }
      .whenIsActive {
        when (slowClock) {
          bottomColor := io.colorStream.payload.bottom
          topColor := io.colorStream.payload.top
          consumeStream := True
        } otherwise {
          consumeStream := False
          column := column + 1
          when(column === 63) {
            goto(finishShiftState)
          }
        }
      }

    finishShiftState.whenIsActive {
      when (~slowClock) {
        goto(blankState)
      }
    }
    blankState.whenIsActive {
      blank := True
      when (~slowClock) {
        goto(latchState)
      }
    }

    latchState.whenIsActive {
      latch := True
      when (~slowClock) {
        goto(unlatchState)
      } otherwise {
        row := row + 1
      }
    }

    unlatchState.whenIsActive {
      latch := False
      when (~slowClock) {
        goto(unblankState)
      }
    }

    unblankState.whenIsActive {
      blank := False
      when (slowClock) {
        when(row === 31) {
          frameCount := frameCount + 1
        }
      } otherwise {
        goto(shiftState)
      }
    }
  }
}

case class FrameStreamer() extends Component {
  val io = new Bundle {
    val read = new Bundle() {
      val row = out UInt(6 bits)
      val column = out UInt(6 bits)
      val color = in(Rgb(RgbConfig(5, 6, 5)))
    }

    val colorStream = master Stream(ColorPair())
  }

  val colorStream = new Stream(ColorPair())
  io.colorStream <> colorStream

  val address = Reg(UInt(11 bits)) init(0)
  val segment = Reg(UInt(1 bits)) init(0)

  val fullAddress = segment @@ address
  io.read.row := fullAddress(11 downto 6)
  io.read.column := fullAddress(5 downto 0)

  val streamValid = Reg(Bool) init(False)
  colorStream.valid := streamValid

  val colors = Reg(ColorPair())
  colorStream.payload := colors

  val stateMachine = new StateMachine {
    val initState = new State with EntryPoint
    val loadTop = new State
    val loadBottom = new State

    val top = Reg(Rgb(RgbConfig(5, 6, 5)))

    initState.whenIsActive {
      goto(loadTop)
    }

    loadTop.whenIsActive {
      top.r := fullAddress(4 downto 0)
      top.g := fullAddress(10 downto 5)
      top.b := U(fullAddress(11)) << 4
      segment := 1

      when (colorStream.ready) {
        streamValid := False
      }
      goto(loadBottom)
    }

    loadBottom.whenIsActive {
      when (colorStream.ready | !streamValid) {
        colors.top := top
        colors.bottom.r := fullAddress(4 downto 0)
        colors.bottom.g := fullAddress(10 downto 5)
        colors.bottom.b := U(fullAddress(11)) << 4

        streamValid := True
        segment := 0
        address := address + 1

        goto(loadTop)
      }
    }
  }
}

case class BufferedLedPanelController() extends Component {
  val io = new Bundle {
    val ledPanel = LedPanel()
    val colorStream = slave Stream(Rgb(RgbConfig(8, 8, 8)))
  }

  val frame = Frame()
  frame.io.inColor <> io.colorStream

  val frameStreamer = FrameStreamer()
  frame.io.read.row := frameStreamer.io.read.row
  frame.io.read.column := frameStreamer.io.read.column
  frameStreamer.io.read.color := frame.io.read.color

  val panelCtrl = LedPanelController()
  panelCtrl.io.ledPanel <> io.ledPanel
  panelCtrl.io.colorStream << frameStreamer.io.colorStream
}