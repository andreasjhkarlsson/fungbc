module Input

open BitLogic
open Memory
open IORegisters
open Interrupts

type Key =
    |Left
    |Right
    |Up
    |Down
    |A
    |B
    |Select
    |Start

type KeyState =
    |Pressed
    |Released

type PinColumn = |First |Second

type Keypad (interrupts: InterruptManager) =
    inherit ValueBackedIORegister(0uy)

    let mutable keyStates = 
        [Left,      Released
         Right,     Released
         Up,        Released
         Down,      Released
         A,         Released
         B,         Released
         Select,    Released
         Start,     Released] |> Map.ofList
    
    // Pin columns
    let pins4 = (A, B, Select, Start)
    let pins5 = (Right, Left, Up, Down)
    
    member this.Item
        with get key = keyStates |> Map.find key
        and set key state =
            if state = Pressed && this.Released key then
                interrupts.Current.Set <- P10P13Flip
            keyStates <- keyStates |> Map.add key state


    member this.Pressed key = this.[key] = Pressed

    member this.Released key = this.[key] = Released

    interface IMemoryCell with
        member x.Value
            with get () =
                let column (p0, p1, p2, p3) =
                    let bit = function
                              |B0 -> setIfFalse <| x.Pressed p0
                              |B1 -> setIfFalse <| x.Pressed p1
                              |B2 -> setIfFalse <| x.Pressed p2
                              |B3 -> setIfFalse <| x.Pressed p3
                              |_ -> CLEAR
                    mapByte bit

                match base.Value &&& 0xF0uy with
                | BitSet 4 value ->
                    value ||| (column pins4)
                | BitSet 5 value ->
                    value ||| (column pins5)
                | _ as value -> value