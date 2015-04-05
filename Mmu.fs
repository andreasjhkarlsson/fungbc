module Mmu

open Constants
open MemoryCell
open Rom
open Ram
open IORegisters
open Gpu
open Timer
open Interrupts

type MemoryAddress = uint16

// Maps the address space of the GBC into different parts (rom, ram, ioregisters, etc.)
type MMU (gpu: GPU, rom: ROM, ram: GBCRam, interrupts: InterruptManager, timers: Timers) =
    
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
        mapAddress 0xFF04us timers.DIV.MemoryCell
        mapAddress 0xFF05us timers.TIMA.MemoryCell
        mapAddress 0xFF06us timers.TMA.MemoryCell
        mapAddress 0xFF07us timers.TAC.MemoryCell
        mapAddress 0xFF40us gpu.Registers.LCDC.MemoryCell
        mapAddress 0xFF41us gpu.Registers.LCDS.MemoryCell
        mapAddress 0xFF42us gpu.Registers.SCY.MemoryCell
        mapAddress 0xFF43us gpu.Registers.SCX.MemoryCell
        mapAddress 0xFF44us gpu.Registers.LY.MemoryCell
        mapAddress 0xFF47us gpu.Registers.BGP.MemoryCell

        mapAddress 0xFF0Fus interrupts.Current.MemoryCell
        mapAddress 0xFFFFus interrupts.InterruptEnable.MemoryCell

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

    