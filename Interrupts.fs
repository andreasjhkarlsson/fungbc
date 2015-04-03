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
type IFRegister(init) =
    inherit ValueBackedIORegister(init)

    member this.Current
        with get () =
            match this.Value with
            | BitSet 0 _ -> Some VBlank
            | BitSet 1 _ -> Some LCDC
            | BitSet 2 _ -> Some TimerOverflow
            | BitSet 3 _ -> Some SerialIO
            | BitSet 4 _ -> Some P10P13Flip
            | _ -> None

        and set interrupt =
            this.Value <- 
                match interrupt with
                | Some interrupt ->
                    let bit =
                        match interrupt with
                        | VBlank ->         0
                        | LCDC ->           1
                        | TimerOverflow ->  2
                        | SerialIO ->       3
                        | P10P13Flip ->     4
                    0uy |> setBit bit
                | None ->
                    0uy

type InterruptManager() =
    
    // Global enable flag (controlled by EI/DI instructions)
    member val Enable = true with get, set

    // The current requested interrupt
    member val Interrupt = IFRegister(0uy)

    // Enables/disables specific interrupts
    member val InterruptEnable = InterruptEnableRegister(0uy)

    // Calls handler function if an interrupt is set.
    member this.Handle fn =
        if this.Enable then
            match this.Interrupt.Current with
            | Some interrupt ->
                if this.InterruptEnable.Enabled interrupt then
                    fn interrupt
            | None -> ()
