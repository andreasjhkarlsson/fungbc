module Mmu

open Constants
open MemoryCell
open Rom
open Ram
open IORegisters
open Gpu

type MemoryAddress = uint16

// Maps the address space of the GBC into different parts (rom, ram, ioregisters, etc.)
type MMU (gpu: GPU, rom: ROM, ram: GBCRam, ioRegisters: IORegisters) =
    
    // Initially make all memory blank.
    let memory = blankMemoryBlock ADDRESS_SPACE_SIZE

    // Set cell at address
    let mapAddress (address: MemoryAddress) cell = Array.set memory (int address) cell

    // Set cells inside range
    let mapBlock (start: MemoryAddress) (stop: MemoryAddress) block =
        {start..stop} |> Seq.iter (fun address -> 
            let cell = Array.get block (address - start |> int)
            mapAddress address cell
        )
    
    // Map memory!!!
    do
        // Map ROM
        mapBlock 0x0000us 0x07FFFus rom.MemoryBlock

        // Map VRAM
        mapBlock 0x8000us 0x9FFFus gpu.VRAM.MemoryBlock

        // Map RAM
        mapBlock 0xC000us 0xDFFFus ram.Working
        mapBlock 0xE000us 0xFDFFus ram.Working 
        mapBlock 0xFF80us 0xFFFEus ram.Stack

        // Map I/O Registers
        mapAddress 0xFF04us ioRegisters.DIV.MemoryCell
        mapAddress 0xFF05us ioRegisters.TIMA.MemoryCell
        mapAddress 0xFF06us ioRegisters.TMA.MemoryCell
        mapAddress 0xFF07us ioRegisters.TAC.MemoryCell
        mapAddress 0xFF0Fus ioRegisters.IF.MemoryCell
        mapAddress 0xFFFFus ioRegisters.IE.MemoryCell

    // Read mapped byte
    member this.Read8 (address: MemoryAddress) = (Array.get memory (int address)).Value
    
    // Write mapped byte     
    member this.Write8 address value = (Array.get memory (int address)).Value <- value

    // Read and write mapped byte after passing through a supplied map function.
    member this.Update8 address fn = this.Read8 address |> fn |> this.Write8 address

    // Read mapped 16 bit value
    member this.Read16 address =
        (this.Read8 address |> uint16) |||
        ((this.Read8 (address + 1us) |> uint16) <<< 8)

    // Write mapped 16 bit value
    member this.Write16 address (value: uint16) =
        this.Write8 address (uint8 value)
        this.Write8 (address + 1us) (value >>> 8 |> uint8)
    
    // Print contents of memory inside range
    member this.PrintDump fromAddress toAddress =
        
        printfn "Memory dump (0x%04X - 0x%04X):\n\n        0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F\n" (fromAddress &&& 0xFFF0) (toAddress ||| 0xF)
        [(fromAddress >>> 4)..(toAddress >>> 4)] |> Seq.iter (fun x ->
            printf "0x%03Xx " x
            [0..15] |> Seq.iter (fun y ->
                printf "%02X " (this.Read8 (x <<< 4 ||| y |> uint16))
                )
            printfn ""
            )
        

    