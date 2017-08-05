module IORegisters

open Memory
open BitLogic

// IORegister = Memory register
[<AbstractClass>]
type IORegister () =
    abstract Value: uint8 with get, set

    // Convenience method so clients doesn't have to cast
    member x.MemoryCell = x :> IMemoryCell

    interface IMemoryCell with
        member x.Value
            with get () = x.Value
            and set newValue = x.Value <- newValue


// A memory register that is fundamentally a regular byte value
type ValueBackedIORegister(init: uint8) =
    inherit IORegister ()

    let mutable value = init

    // We seperate the concept of register value and memory value
    // as writes for example may be disabled from memory, but enabled overall for the register.
    override x.Value 
        with get () = value
        and set newValue = value <- newValue

    member this.Update fn = this.Value <- fn this.Value

    member this.GetBit bit = this.Value |> bitStateOf bit

    member this.SetBit bit state = this.Value <- controlBit bit state this.Value

    member this.GetBits from ``to`` = BitLogic.bitsValue (int this.Value) from ``to`` |> uint8


    