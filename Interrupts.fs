module Interrupts

open BitLogic
open Register

type InterruptEnableRegister (init) =
    inherit MemoryRegister<uint8>(init)

    member this.P10P13Flip = isBitSet 4 this.Value
    member this.SerialIOTransfer = isBitSet 3 this.Value
    member this.TimerOverflow = isBitSet 2 this.Value 
    member this.LCDC = isBitSet 1 this.Value
    member this.VBlank = isBitSet 0 this.Value 

type InterruptManager () =
    member val InteruptEnabled = InterruptEnableRegister(0uy)
