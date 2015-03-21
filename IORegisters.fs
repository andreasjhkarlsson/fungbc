module IORegisters

open MemoryCell
open BitLogic
open Units
open Clock

[<AbstractClass>]
type IORegister () as this =
    abstract MemoryValue: uint8 with get, set
    member val MemoryCell = VirtualCell((fun () -> this.MemoryValue), (fun newValue -> this.MemoryValue <- newValue))

type ValueBackedIORegister(init) =
    inherit IORegister ()

    // We seperate the concept of register value and memory value
    // as writes for example may be disabled from memory, but enabled overall for the register.
    member val Value = init with get, set

    override this.MemoryValue
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

    let startClock () = derive clock 16384<Hz>

    let mutable clock = startClock ()

    override this.MemoryValue
        with get () = clock.Ticks |> uint8
        and set _ = clock <- startClock ()

type TIMARegister (systemClock: Clock, startFrequency: int<Hz>) =
    inherit IORegister ()

    let startClock frequency = derive systemClock frequency
    
    let mutable clock = startClock startFrequency 
    let mutable paused = false

    member this.Frequency
        with set frequency =
            clock <- startClock frequency
            if paused then clock <- freeze clock

    member this.Paused
        with set state =
            match state with
            | true -> clock <- freeze clock
            | false -> clock <- unfreeze clock systemClock
            paused <- state

    override this.MemoryValue
        with get () = clock.Ticks |> uint8
        and set _ = () // This is probably wrong. TODO. 

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

type TMARegister (init) = inherit ValueBackedIORegister(init)

type IORegisters (clock) =
    
    let ie = InterruptEnableRegister(0uy)   
    let div = DIVRegister(clock)
    let tima = TIMARegister(clock,4096<Hz>)
    let tma = TMARegister(0uy)
    let tac = TACRegister(tima,0x4uy)
    
    member this.IE = ie
    member this.DIV = div
    member this.TIMA = tima
    member this.TMA = tma
    member this.TAC = tac
    