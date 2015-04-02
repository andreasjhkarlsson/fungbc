module Timer

open IORegisters
open Clock
open BitLogic
open Units
open Interrupts

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

type Timers (systemClock, interrupts: InterruptManager) =
    let div = DIVRegister(systemClock)
    let tima = TIMARegister(systemClock,4096<Hz>)
    let tma = TMARegister(0uy)
    let tac = TACRegister(tima,0x4uy)

    member this.DIV = div
    member this.TIMA = tima
    member this.TMA = tma
    member this.TAC = tac
    
    member this.Update () =
        if tima.Overflowed () then
            tima.MemoryValue <- tma.Value
            interrupts.Interrupt.Current <- Some TimerOverflow