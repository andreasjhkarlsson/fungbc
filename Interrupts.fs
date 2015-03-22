module Interrupts

open BitLogic
open MemoryCell
open IORegisters

type Interrupt = |TIMER_OVERFLOW of TIMARegister*TMARegister*IFRegister

let handler = function |TIMER_OVERFLOW _ -> 0x0050us

let execute = function
    |TIMER_OVERFLOW (tima, tma, _if) -> 
        tima.MemoryValue <- tma.MemoryValue
        _if.SetTimerOverflow ()

(*
[<AbstractClass>]
type Interrupt () = 
    abstract Handler: uint16 with get
    abstract member Execute: unit -> unit
    abstract member Check: unit -> bool
    abstract member Enabled: InterruptEnableRegister -> bool

type TimerInterrupt (ioRegisters: IORegisters) =
    inherit Interrupt()

    let tima = ioRegisters.TIMA
    let tma = ioRegisters.TMA

    override this.Handler with get () = 0x0050us

    override this.Execute () = tima.MemoryValue <- tma.MemoryValue

    override this.Check () = tima.Overflowed ()

    override this.Enabled interruptEnabled = interruptEnabled.TimerOverflow

    *)