package esc

import spinal.core._
import spinal.lib.{Reverse, slave}
import spinal.lib.bus.amba3.apb._
import spinal.lib.com.uart.Apb3UartCtrl
import spinal.lib.fsm._
import spinal.lib.graphic.{Rgb, RgbConfig}


case class Painter() extends Component {
    val io = new Bundle {
    val x = in UInt(6 bits)
    val y = in UInt(6 bits)
    val color = out(Rgb(RgbConfig(8, 8, 8)))
  }

  io.color.r := io.x << 2
  io.color.g := 0
  io.color.b := io.y << 2
}

case class PwmPainter() extends Component {
  val io = new Bundle {
    val frameCount = in UInt(8 bits)
    val x = in UInt(6 bits)
    val y = in UInt(6 bits)
    val color = out(RgbShift())
  }

  val colorPainter = Painter()
  colorPainter.io.x <> io.x
  colorPainter.io.y <> io.y

  io.color.r := (colorPainter.io.color.r > Reverse(io.frameCount))
  io.color.g := (colorPainter.io.color.g > Reverse(io.frameCount))
  io.color.b := (colorPainter.io.color.b > Reverse(io.frameCount))

}

case class LedPanelController() extends Component {
  val io = new Bundle {
    val led = LedPanel()
    val bus =  slave(Apb3(Apb3Config(addressWidth = 16, dataWidth = 32)))
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

  val frame = Mem(Rgb(RgbConfig(8, 8, 8)), 8 * 8)
  val busControl = Apb3SlaveFactory(io.bus)
  busControl.createAndDriveFlow(frame.writePort, 0)

  val row = Reg(UInt(5 bits)) init(0)
  val column = Reg(UInt(7 bits))
  val clockEnabled = Reg(Bool) init(False)
  val frameCount = Reg(UInt(32 bits)) init(0)

  val topPainter = PwmPainter()
  topPainter.io.frameCount <> frameCount(7 downto 0)
  topPainter.io.x <> column(5 downto 0)
  topPainter.io.y <> (U(0, 1 bits)  @@ row)
  topPainter.io.color <> led.top

  val bottomPainter = PwmPainter()
  bottomPainter.io.frameCount <> frameCount(7 downto 0)
  bottomPainter.io.x <> column(5 downto 0)
  bottomPainter.io.y <> (U(1, 1 bits)  @@ row)
  bottomPainter.io.color <> led.bottom

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