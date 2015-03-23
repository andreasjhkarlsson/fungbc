module Interrupts

open BitLogic
open MemoryCell
open IORegisters
open Timer

// Interrupt enabled register. Controls if specific interrupts are enabled.
type InterruptEnableRegister (init) =
    inherit ValueBackedIORegister(init)
    
    member this.VBlank           =  isBitSet 0 this.MemoryValue 
    member this.LCDC             =  isBitSet 1 this.MemoryValue
    member this.TimerOverflow    =  isBitSet 2 this.MemoryValue 
    member this.SerialIOTransfer =  isBitSet 3 this.MemoryValue
    member this.P10P13Flip       =  isBitSet 4 this.MemoryValue

// Interrupt flag register. I set according to the current interrupt
type IFRegister(init) as this =
    inherit ValueBackedIORegister(init)

    let setBit bit = this.Value <- 0uy |> BitLogic.setBit bit
    
    member this.SetVBlank ()            = setBit 0
    member this.SetLCDC ()              = setBit 1
    member this.SetTimerOverflow ()     = setBit 2
    member this.SetSerialIOTransfer ()  = setBit 3
    member this.P10P13Flip ()           = setBit 4

    member this.Clear = this.Value <- 0uy

type InterruptRegisters () =
    let ie = InterruptEnableRegister(0x1Fuy)   
    let _if = IFRegister(0uy)
    
    member this.IE = ie
    member this.IF = _if    

type InterruptHandler = |InterruptHandler of uint16

[<AbstractClass>]
type Interrupt () =
    abstract Execute: unit -> InterruptHandler


type TimerInterrupt (timers: Timers, interruptRegisters: InterruptRegisters) =
    inherit Interrupt()

    override this.Execute () =
        timers.TIMA.MemoryValue <- timers.TMA.MemoryValue
        interruptRegisters.IF.SetTimerOverflow ()
        InterruptHandler 0x0050us

    member this.Check () =
        interruptRegisters.IE.TimerOverflow && timers.TIMA.Overflowed ()


