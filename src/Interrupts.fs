module Interrupts

open BitLogic
open MemoryCell
open IORegisters

type Interrupt =
    | TimerOverflow
    | VBlank
    | LCDC
    | SerialIO
    | P10P13Flip

let address =
    function
    | VBlank ->         0x0040us
    | LCDC ->           0x0048us
    | TimerOverflow ->  0x0050us
    | SerialIO ->       0x0058us
    | P10P13Flip ->     0x0060us


// Interrupt enabled register. Controls if specific interrupts are enabled.
type InterruptEnableRegister (init) =
    inherit ValueBackedIORegister(init)

    member this.Enabled interrupt =
        let bit =
            match interrupt with
            | VBlank ->         0
            | LCDC ->           1
            | TimerOverflow ->  2
            | SerialIO ->       3
            | P10P13Flip ->     4
        this.Value |> isBitSet bit 


// Interrupt flag register. Set according to the current interrupt
type InterruptFlagRegister(init) =
    inherit ValueBackedIORegister(init)

    let bitOf = function
                | VBlank ->         0
                | LCDC ->           1
                | TimerOverflow ->  2
                | SerialIO ->       3
                | P10P13Flip ->     4

    member this.Item
        with get interrupt = this.Value |> isBitSet (bitOf interrupt)
        and set interrupt state = this.Value <- this.Value |> controlBit (bitOf interrupt) (setIfTrue state)

    member this.Set with set interrupt = this.[interrupt] <- true

    member this.Clear with set interrupt = this.[interrupt] <- false
            

type InterruptManager() =
    
    // Global enable flag (controlled by EI/DI instructions)
    member val Enable = true with get, set

    // The current requested interrupt
    member val Current = InterruptFlagRegister(0uy)

    // Enables/disables specific interrupts
    member val InterruptEnable = InterruptEnableRegister(0uy)

    // Calls handler function if an interrupt is set.
    member this.Handle fn =
        // Global enable
        if this.Enable && this.Current.Value > 0uy then
            
            // Active pattern for detecting if specific interrupt is set
            let (|InterruptRaised|_|) interrupt (interruptRegister: InterruptFlagRegister) =
                if interruptRegister.[interrupt] then Some interrupt else None

            let enabled = this.InterruptEnable.Enabled

            // Match cases sorted by priority
            match this.Current with
            | InterruptRaised VBlank interrupt
            | InterruptRaised LCDC interrupt
            | InterruptRaised TimerOverflow interrupt
            | InterruptRaised SerialIO interrupt
            | InterruptRaised P10P13Flip interrupt
                // Match any of the above as long as it's enabled in IE register
                when enabled interrupt ->
                    fn interrupt
            | _ -> () // No interrupt happened

