module Input

open BitLogic
open MemoryCell
open IORegisters

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

type Keypad () =
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
    let pins5 = (Right, Left, Down, Up)
    
    member this.Item
        with get key = keyStates |> Map.find key
        and set key state = keyStates <- keyStates |> Map.add key state

    member this.Pressed key = this.[key] = Pressed

    member this.Released key = this.[key] = Released

    override this.MemoryValue
        with get () =
            let column (p0, p1, p2, p3) =
                let bit = function
                          |0 -> setIfFalse <| this.Pressed p0
                          |1 -> setIfFalse <| this.Pressed p1
                          |2 -> setIfFalse <| this.Pressed p2
                          |3 -> setIfFalse <| this.Pressed p3
                          |_ -> CLEAR
                mapByte bit

            match base.MemoryValue &&& 0xF0uy with
            | BitSet 4 value ->
                value ||| (column pins4)
            | BitSet 5 value ->
                value ||| (column pins5)
            | value -> value