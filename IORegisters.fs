module IORegisters

open MemoryCell
open BitLogic
open Timer
open Units
open Clock

[<AbstractClass>]
type IORegister () =
    abstract MemoryValue: uint8 with get, set

type ValueBackedIORegister(init) as this =
    inherit IORegister ()

    // We seperate the concept of register value and memory value
    // as writes for example may be disabled from memory, but enabled overall for the register.
    member val Value = init with get, set

    member val MemoryCell = VirtualCell((fun () -> this.MemoryValue), (fun newValue -> this.MemoryValue <- newValue))

    default this.MemoryValue
        with get () = this.Value
        and set newValue = this.Value <- newValue
    

type InterruptEnableRegister (init) =
    inherit ValueBackedIORegister(0uy)
    
    member this.VBlank           =  isBitSet 0 this.MemoryValue 
    member this.LCDC             =  isBitSet 1 this.MemoryValue
    member this.TimerOverflow    =  isBitSet 2 this.MemoryValue 
    member this.SerialIOTransfer =  isBitSet 3 this.MemoryValue
    member this.P10P13Flip       =  isBitSet 4 this.MemoryValue

type DIVRegister(clock: Clock) =
    inherit IORegister ()

    let frequency = 16384<Hz>

    let startCount () = timedCount frequency clock

    let mutable count = startCount ()

    override this.MemoryValue
        with get () = count() |> uint8
        and set _ = count <- startCount() // Writing any value to memory will clear register (count)

type IORegisters (clock) =
    
    let ie = InterruptEnableRegister(0uy)   
    let div = DIVRegister(clock)

    member this.IE = ie
    member this.DIV = div