module Mmu

open Constants
open MemoryCell
open Rom
open Ram
open IORegisters
open Gpu
open Timer
open Interrupts
open Input

type MemoryAddress = uint16

// Maps the address space of the GBC into different parts (rom, ram, ioregisters, etc.)
type MMU (gpu: GPU, rom: ROM, ram: GBCRam,gbs: Sound.GBS,keypad: Keypad, interrupts: InterruptManager, timers: Timers) as this =
    
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
    
    let copyMemory fromAddress toAddress size =
        {0us..(size - 1us)} |> Seq.iter (fun offset ->
            this.Read8 (fromAddress + offset) |> this.Write8 (toAddress + offset) 
        )

    let oamDMACell =
        let onWrite source =
            copyMemory ((uint16 source) * 0x100us) 0xFE00us 160us
            source
        readWriteCell 0uy |> hookWrite onWrite

    // Map memory!!!
    do
        // Map ROM
        mapBlock 0x0000us 0x07FFFus rom.ROMBlock

        // Map VRAM
        mapBlock 0x8000us 0x9FFFus gpu.VRAM.MemoryBlock

        // Map external RAM (if any)
        match rom.RAMBlock with
        | Some ram -> mapBlock 0xA000us 0xBFFFus ram
        | _ -> ()

        // Map RAM
        mapBlock 0xC000us 0xDFFFus ram.Working
        mapBlock 0xE000us 0xFDFFus ram.Working 

        // Map OAM
        mapBlock 0xFE00us 0xFE9Fus gpu.VRAM.OAM

        // Map default stack
        mapBlock 0xFF80us 0xFFFEus ram.Stack

        // Map I/O Registers
        mapAddress 0xFF00us keypad.MemoryCell
        mapAddress 0xFF04us timers.DIV.MemoryCell
        mapAddress 0xFF05us timers.TIMA.MemoryCell
        mapAddress 0xFF06us timers.TMA.MemoryCell
        mapAddress 0xFF07us timers.TAC.MemoryCell
        mapAddress 0xFF0Fus interrupts.Current.MemoryCell
        mapAddress 0xFF16us gbs.Square2.NR21.MemoryCell
        mapAddress 0xFF17us gbs.Square2.NR22.MemoryCell
        mapAddress 0xFF18us gbs.Square2.NR23.MemoryCell
        mapAddress 0xFF19us gbs.Square2.NR24.MemoryCell
        mapAddress 0xFF24us gbs.NR50.MemoryCell
        mapAddress 0xFF25us gbs.NR51.MemoryCell
        mapAddress 0xFF26us gbs.NR52.MemoryCell
        mapAddress 0xFF40us gpu.Registers.LCDC.MemoryCell
        mapAddress 0xFF41us gpu.Registers.LCDS.MemoryCell
        mapAddress 0xFF42us gpu.Registers.SCY.MemoryCell
        mapAddress 0xFF43us gpu.Registers.SCX.MemoryCell
        mapAddress 0xFF44us gpu.Registers.LY.MemoryCell
        mapAddress 0xFF45us gpu.Registers.LYC.MemoryCell
        mapAddress 0xFF46us oamDMACell
        mapAddress 0xFF47us gpu.Registers.BGP.MemoryCell
        mapAddress 0xFF48us gpu.Registers.OBP0.MemoryCell
        mapAddress 0xFF49us gpu.Registers.OBP1.MemoryCell
        mapAddress 0xFF4Aus gpu.Registers.WY.MemoryCell
        mapAddress 0xFF4Bus gpu.Registers.WX.MemoryCell
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

    member this.InitDefaults () =
        let setDefault address value = this.Write8 (uint16 address) value

        setDefault 0xFF05 0x00uy
        setDefault 0xFF06 0x00uy
        setDefault 0xFF07 0x00uy

        setDefault 0xFF10 0x80uy
        setDefault 0xFF11 0xBFuy
        setDefault 0xFF12 0xF3uy
        setDefault 0xFF14 0xBFuy
        setDefault 0xFF16 0x3Fuy
        setDefault 0xFF17 0x00uy
        setDefault 0xFF19 0xBFuy
        setDefault 0xFF1A 0x7Fuy
        setDefault 0xFF1B 0xFFuy
        setDefault 0xFF1C 0x9Fuy
        setDefault 0xFF1E 0xBFuy
        setDefault 0xFF20 0xFFuy
        setDefault 0xFF21 0x00uy
        setDefault 0xFF22 0x00uy
        setDefault 0xFF23 0xBFuy
        setDefault 0xFF24 0x77uy
        setDefault 0xFF25 0xF3uy
        setDefault 0xFF26 0xF1uy

        setDefault 0xFF40 0x91uy
        setDefault 0xFF42 0x00uy
        setDefault 0xFF43 0x00uy
        setDefault 0xFF45 0x00uy
        setDefault 0xFF47 0xFCuy
        setDefault 0xFF48 0xFFuy
        setDefault 0xFF49 0xFFuy
        setDefault 0xFF4A 0x00uy
        setDefault 0xFF4B 0x00uy
        setDefault 0xFFFF 0x00uy
        
        
    