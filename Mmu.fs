module Mmu


type MemoryAddress = uint16

type MemoryLocation = | RAM of array<uint8>*int

type MMU () =

    let ram = Array.create (pown 2 16) 0uy

    // Map memory address to different memory types (ram, vram, special registers etc.)
    let translate (address: MemoryAddress) =
        let newAddress =
            match int address with
            | echoed when echoed >= 0xE000 && echoed <= 0xFE00 ->
                echoed - 0x2000
            | address -> address
        RAM (ram, newAddress)


    member this.read8  address =
        match translate address with
        | RAM (ram,address) ->
            Array.get ram address

    member this.write8 address value =
        match translate address with
        | RAM (ram,address) ->
            Array.set ram address value

    member this.update8 address fn = this.read8 address |> fn |> this.write8 address

    member this.read16 address =
        (this.read8 address |> uint16) |||
        ((this.read8 (address + 1us) |> uint16) <<< 8)

    member this.write16 address (value: uint16) =
        this.write8 address (uint8 value)
        this.write8 (address + 1us) (value >>> 8 |> uint8)

    member this.printDump fromAddress toAddress =
        
        printfn "Memory dump (0x%04X - 0x%04X):\n\n        0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F\n" (fromAddress &&& 0xFFF0) (toAddress ||| 0xF)
        [(fromAddress >>> 4)..(toAddress >>> 4)] |> Seq.iter (fun x ->
            printf "0x%03Xx " x
            [0..15] |> Seq.iter (fun y ->
                printf "%02X " (this.read8 (x <<< 4 ||| y |> uint16))
                )
            printfn ""
            )
        

    