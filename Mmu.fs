module Mmu

open Constants

type MemoryAddress = uint16

type MMU () =

    let memory =
        
        let ram = Array.create (pown 2 16) 0uy

        let map address =
            match address with
            | echoed when echoed >= 0xE000 && echoed <= 0xFE00 ->
                (fun () -> Array.get ram (echoed - 0x2000)), Array.set ram (echoed - 0x2000)
            | index ->
                (fun () -> Array.get ram address), Array.set ram address

        Array.init ADDRESS_SPACE_SIZE map |> Array.get

    member this.Read8 address =
        let get, _ = memory (int address)
        get ()


    member this.Write8 address value =
        let _, set = memory (int address)
        set value

    member this.Update8 address fn = this.Read8 address |> fn |> this.Write8 address

    member this.Read16 address =
        (this.Read8 address |> uint16) |||
        ((this.Read8 (address + 1us) |> uint16) <<< 8)

    member this.Write16 address (value: uint16) =
        this.Write8 address (uint8 value)
        this.Write8 (address + 1us) (value >>> 8 |> uint8)

    member this.LoadBlob address (blob: array<uint8>) =
        blob |> Array.iteri (fun index b -> this.Write8 (address + (uint16 index)) b)

    member this.PrintDump fromAddress toAddress =
        
        printfn "Memory dump (0x%04X - 0x%04X):\n\n        0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F\n" (fromAddress &&& 0xFFF0) (toAddress ||| 0xF)
        [(fromAddress >>> 4)..(toAddress >>> 4)] |> Seq.iter (fun x ->
            printf "0x%03Xx " x
            [0..15] |> Seq.iter (fun y ->
                printf "%02X " (this.Read8 (x <<< 4 ||| y |> uint16))
                )
            printfn ""
            )
        

    