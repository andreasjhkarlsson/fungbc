module IORegisters

open MemoryCell
open BitLogic
open Units
open Clock

// IORegister = Memory register
[<AbstractClass>]
type IORegister () as this =
    abstract MemoryValue: uint8 with get, set
    member val MemoryCell = VirtualCell((fun () -> this.MemoryValue), (fun newValue -> this.MemoryValue <- newValue))

// A memory register that is fundamentally a regular byte value
type ValueBackedIORegister(init) =
    inherit IORegister ()

    // We seperate the concept of register value and memory value
    // as writes for example may be disabled from memory, but enabled overall for the register.
    member val Value = init with get, set

    override this.MemoryValue
        with get () = this.Value
        and set newValue = this.Value <- newValue 

// Interrupt enabled register. Controls if specific interrupts are enabled.
type InterruptEnableRegister (init) =
    inherit ValueBackedIORegister(init)
    
    member this.VBlank           =  isBitSet 0 this.MemoryValue 
    member this.LCDC             =  isBitSet 1 this.MemoryValue
    member this.TimerOverflow    =  isBitSet 2 this.MemoryValue 
    member this.SerialIOTransfer =  isBitSet 3 this.MemoryValue
    member this.P10P13Flip       =  isBitSet 4 this.MemoryValue

// Divider register. Simply increments 16384 times per second
type DIVRegister(clock: Clock) =
    inherit IORegister ()

    let startClock () = derive clock 16384<Hz>

    let mutable clock = startClock ()

    override this.MemoryValue
        with get () = clock.Ticks |> uint8 // Converting to uint8 == % 256
        and set _ = clock <- startClock () // Writing any vlaue clears register (i.e. restart clock)

// Timer counter. Increments at a frequency controlled by TAC register
type TIMARegister (systemClock: Clock, startFrequency: int<Hz>) =
    inherit IORegister ()

    let startClock frequency = derive systemClock frequency
    
    let mutable clock = startClock startFrequency 
    let mutable paused = false
    let mutable baseValue = 0uy

    member this.Frequency
        with set frequency =
            clock <- startClock frequency
            if paused then clock <- freeze clock

    member this.ResetClock () = this.Frequency <- clock.Frequency 

    member this.Paused
        with set state =
            match state with
            | true -> clock <- freeze clock
            | false -> clock <- unfreeze clock systemClock
            paused <- state

    override this.MemoryValue
        with get () =
            clock.Ticks + uint64 baseValue |> uint8
        and set value =
            baseValue <- value
            this.ResetClock ()

    member this.Overflowed () = clock.Ticks + uint64 baseValue > 0xFFUL


// Timer control register. Controls the TIMA register.
type TACRegister (tima: TIMARegister,init) =
    inherit ValueBackedIORegister(init)

    override this.MemoryValue
        with set newValue =
            
            let oldValue = this.MemoryValue

            match (oldValue, newValue) with
            // Timer stop bit changed!
            | BitChanged 2 newState ->
                match newState with
                | SET -> tima.Paused <- false
                | CLEAR -> tima.Paused <- true
            | _ -> ()

            match (oldValue, newValue) with
            // Frequency selection changed!
            | BitsChanged 0x3uy value ->
                match value with
                | 0x0uy -> tima.Frequency <- 4096<Hz>
                | 0x1uy -> tima.Frequency <- 262144<Hz>
                | 0x2uy -> tima.Frequency <- 65536<Hz>
                | 0x3uy -> tima.Frequency <- 16384<Hz>
                | _ -> () // Will never happen (since value is newValue &&& 0x3) 
            | _ -> () 

            base.MemoryValue <- newValue

// Timer modulo register. Is loaded into TIMA when TIMA overflows.
type TMARegister (init) = inherit ValueBackedIORegister(init)

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

type IORegisters (clock) =
    
    let ie = InterruptEnableRegister(0x1Fuy)   
    let div = DIVRegister(clock)
    let tima = TIMARegister(clock,4096<Hz>)
    let tma = TMARegister(0uy)
    let tac = TACRegister(tima,0x4uy)
    let _if = IFRegister(0uy)
    
    member this.IE = ie
    member this.IF = _if
    member this.DIV = div
    member this.TIMA = tima
    member this.TMA = tma
    member this.TAC = tac

    