module Mmu

open Constants
open MemoryCell
open Rom
open Ram
open IORegisters

type MemoryAddress = uint16

type MMU (rom: ROM, ram: GBCRam, ioRegisters: IORegisters) =
    
    let memory = blankMemoryBlock ADDRESS_SPACE_SIZE

    let mapAddress (address: MemoryAddress) cell = Array.set memory (int address) cell

    let mapBlock (start: MemoryAddress) (stop: MemoryAddress) block =
        {start..stop} |> Seq.iter (fun address -> 
            let cell = Array.get block (address - start |> int)
            mapAddress address cell
        )

    do
        // Map ROM
        mapBlock 0x0000us 0x07FFFus rom.Code
        mapBlock 0x0000us 0x07FFFus rom.Code

        // Map RAM
        mapBlock 0xC000us 0xDFFFus ram.Working
        mapBlock 0xE000us 0xFDFFus ram.Working 
        mapBlock 0xFF80us 0xFFFEus ram.Stack

        // Map I/O Registers
        mapAddress 0xFFFFus ioRegisters.IE.MemoryCell
        mapAddress 0xFF04us ioRegisters.DIV.MemoryCell

    member this.Read8 (address: MemoryAddress) = (Array.get memory (int address)).Value
        
    member this.Write8 address value = (Array.get memory (int address)).Value <- value

    member this.Update8 address fn = this.Read8 address |> fn |> this.Write8 address

    member this.Read16 address =
        (this.Read8 address |> uint16) |||
        ((this.Read8 (address + 1us) |> uint16) <<< 8)

    member this.Write16 address (value: uint16) =
        this.Write8 address (uint8 value)
        this.Write8 (address + 1us) (value >>> 8 |> uint8)

    member this.PrintDump fromAddress toAddress =
        
        printfn "Memory dump (0x%04X - 0x%04X):\n\n        0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F\n" (fromAddress &&& 0xFFF0) (toAddress ||| 0xF)
        [(fromAddress >>> 4)..(toAddress >>> 4)] |> Seq.iter (fun x ->
            printf "0x%03Xx " x
            [0..15] |> Seq.iter (fun y ->
                printf "%02X " (this.Read8 (x <<< 4 ||| y |> uint16))
                )
            printfn ""
            )
        

    