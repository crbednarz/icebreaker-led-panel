package esc

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.graphic._

case class LedPanel() extends Bundle {
  val address = out UInt(5 bits)
  val top = out(Rgb(1, 1, 1))
  val bottom = out(Rgb(1, 1, 1))
  val clock = out Bool
  val latch = out Bool
  val blank = out Bool
}

case class ColorPair() extends Bundle {
  val top = Rgb(5, 6, 5)
  val bottom = Rgb(5, 6, 5)
}

case class ColorToPwm() extends Component {
  val io = new Bundle {
    val frameCount = in UInt(8 bits)
    val inColor = in(Rgb(5, 6, 5))
    val outColor = out(Rgb(1, 1, 1))
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
  io.ledPanel.address <> RegNext(row - 1)

  val topShift = Reg(Rgb(1, 1, 1))
  io.ledPanel.top <> topShift

  val bottomShift = Reg(Rgb(1, 1, 1))
  io.ledPanel.bottom <> bottomShift

  val latch = Reg(Bool()) init(True)
  io.ledPanel.latch <> latch

  val blank = Reg(Bool()) init(True)
  io.ledPanel.blank <> blank

  val column = Reg(UInt(6 bits)) init(0)
  val frameCount = Reg(UInt(32 bits)) init(0)

  val topPainter = ColorToPwm()
  val topColor = Reg(Rgb(5, 6, 5))
  topPainter.io.frameCount := frameCount(7 downto 0)
  topPainter.io.inColor <> topColor
  topShift <> topPainter.io.outColor

  val bottomPainter = ColorToPwm()
  val bottomColor = Reg(Rgb(5, 6, 5))
  bottomPainter.io.frameCount <> frameCount(7 downto 0)
  bottomPainter.io.inColor <> bottomColor
  bottomShift <> bottomPainter.io.outColor

  val slowClock = Reg(Bool) init(False)
  slowClock := ~slowClock

  val stateMachine = new StateMachine {
    val consumeStream = Reg(Bool) init(False)

    io.colorStream.ready := consumeStream
    io.ledPanel.clock := ~consumeStream

    val shiftState : State = new State {
      whenIsActive {
        when(slowClock) {
          bottomColor := io.colorStream.payload.bottom
          topColor := io.colorStream.payload.top
          consumeStream := True
        } otherwise {
          consumeStream := False
          column := column + 1
          when(column === 63) {
            goto(blankState)
          }
        }
      }
    }
    val blankState : State = new State {
      whenIsActive {
        blank := True
        when (~slowClock) {
          goto(latchState)
        }
      }
    }

    val latchState : State = new State() {
      whenIsActive {
        latch := True
        when (~slowClock) {
          goto(unlatchState)
        } otherwise {
          row := row + 1
          when(row === 31) {
            frameCount := frameCount + 1
          }
        }
      }
    }

    val unlatchState : State = new State {
      whenIsActive {
        latch := False
        when (~slowClock) {
          goto(unblankState)
        }
      }
    }

    val unblankState : State = new State with EntryPoint {
      whenIsActive {
        blank := False
        when (slowClock) {
        } otherwise {
          goto(shiftState)
        }
      }
    }
  }
}

case class FrameStreamer() extends Component {
  val io = new Bundle {
    val read = new Bundle() {
      val address = out UInt(12 bits)
      val color = in(Rgb(5, 6, 5))
    }

    val colorStream = master Stream(ColorPair())
  }

  val colorStream = new Stream(ColorPair())
  io.colorStream <> colorStream

  val address = Reg(UInt(12 bits)) init(0)
  val lastAddress = address + 1

  io.read.address := lastAddress.rotateRight(1)

  val streamValid = Reg(Bool) init(False)
  colorStream.valid := streamValid

  val colors = Reg(ColorPair())
  colorStream.payload := colors

  val stateMachine = new StateMachine {
    val loadTop = new State with EntryPoint
    val loadBottom = new State
    val waitState = new State

    val top = Reg(Rgb(5, 6, 5))
    val bottom = Reg(Rgb(5, 6, 5))

    loadTop.whenIsActive {
      top := io.read.color
      address := address + 1

      when (colorStream.ready) {
        streamValid := False
      }
      goto(loadBottom)
    }

    loadBottom.whenIsActive {
      bottom := io.read.color
      when (colorStream.ready | !streamValid) {
        colors.top := top
        colors.bottom := io.read.color
        streamValid := True
        address := address + 1

        goto(loadTop)
      } otherwise {
        goto(waitState)
      }
    }

    waitState.whenIsActive {
      when (colorStream.ready | !streamValid) {
        colors.top := top
        colors.bottom := bottom
        streamValid := True
        address := address + 1

        goto(loadTop)
      }
    }
  }
}

case class BufferedLedPanelController() extends Component {
  val io = new Bundle {
    val ledPanel = LedPanel()
    val write = slave Flow(FrameWrite())
    val present = in Bool
  }

  val frame = Frame()
  frame.io.write <> io.write
  frame.io.present <> io.present

  val frameStreamer = FrameStreamer()
  frame.io.read.address := frameStreamer.io.read.address
  frameStreamer.io.read.color := frame.io.read.color

  val panelCtrl = LedPanelController()
  panelCtrl.io.ledPanel <> io.ledPanel
  panelCtrl.io.colorStream << frameStreamer.io.colorStream
}