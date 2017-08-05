module Mmu

open Constants
open Memory
open Rom
open Ram
open IORegisters
open Gpu
open Timer
open Interrupts
open Input
open Misc


type Mapping =
    | MemoryBlock of MemoryBlock * uint16
    | MemoryCell of IMemoryCell
    | Unmapped


// Maps the address space of the GBC into different parts (rom, ram, ioregisters, etc.)
type MMU (gpu: GPU, rom: ROM, ram: GBCRam,gbs: Sound.GBS,keypad: Keypad, interrupts: InterruptManager, timers: Timers) as this =
    

    
    let copyMemory fromAddress toAddress size =
        {0us..(size - 1us)} |> Seq.iter (fun offset ->
            this.Read8 (fromAddress + offset) |> this.Write8 (toAddress + offset) 
        )

    let oamDMARegister =
        {
            new ValueBackedIORegister(0uy) with
                override x.Value
                    with set source =
                        do copyMemory ((uint16 source) * 0x100us) 0xFE00us 160us
                        base.Value <- source
        } :> IORegister


    let map =
        {0us..((uint16 ADDRESS_SPACE_SIZE)-1us)}
        |> Seq.map (
            function
            | Range 0x0000us 0x7FFFus _ -> MemoryBlock (rom.ROMBlock, 0x0000us)
            | Range 0x8000us 0x9FFFus _ -> MemoryBlock (gpu.VRAM.Memory, 0x8000us)
            | Range 0xA000us 0xBFFFus _ ->
                match rom.RAMBlock with
                | Some ram -> MemoryBlock (ram, 0xA000us)
                | None -> Unmapped
            | Range 0xC000us 0xDFFFus _ -> MemoryBlock (ram.Working, 0xC000us) 
            | Range 0xE000us 0xFDFFus _ -> MemoryBlock (ram.Working, 0xE000us)
            | Range 0xFE00us 0xFE9Fus _ -> MemoryBlock (gpu.VRAM.OAM, 0xFE00us) 
            | 0xFF00us -> MemoryCell keypad
            | 0xFF04us -> MemoryCell timers.DIV
            | 0xFF05us -> MemoryCell timers.TIMA
            | 0xFF06us -> MemoryCell timers.TMA
            | 0xFF07us -> MemoryCell timers.TAC
            | 0xFF0Fus -> MemoryCell interrupts.Current
            | 0xFF10us -> MemoryCell gbs.Square1.NR10
            | 0xFF11us -> MemoryCell gbs.Square1.NR11
            | 0xFF12us -> MemoryCell gbs.Square1.NR12
            | 0xFF13us -> MemoryCell gbs.Square1.NR13
            | 0xFF14us -> MemoryCell gbs.Square1.NR14
            | 0xFF16us -> MemoryCell gbs.Square2.NR21
            | 0xFF17us -> MemoryCell gbs.Square2.NR22
            | 0xFF18us -> MemoryCell gbs.Square2.NR23
            | 0xFF19us -> MemoryCell gbs.Square2.NR24
            | 0xFF24us -> MemoryCell gbs.NR50
            | 0xFF25us -> MemoryCell gbs.NR51
            | 0xFF26us -> MemoryCell gbs.NR52
            | 0xFF40us -> MemoryCell gpu.Registers.LCDC
            | 0xFF41us -> MemoryCell gpu.Registers.LCDS
            | 0xFF42us -> MemoryCell gpu.Registers.SCY
            | 0xFF43us -> MemoryCell gpu.Registers.SCX
            | 0xFF44us -> MemoryCell gpu.Registers.LY
            | 0xFF45us -> MemoryCell gpu.Registers.LYC
            | 0xFF46us -> MemoryCell oamDMARegister
            | 0xFF47us -> MemoryCell gpu.Registers.BGP
            | 0xFF48us -> MemoryCell gpu.Registers.OBP0
            | 0xFF49us -> MemoryCell gpu.Registers.OBP1
            | 0xFF4Aus -> MemoryCell gpu.Registers.WY
            | 0xFF4Bus -> MemoryCell gpu.Registers.WX
            | Range 0xFF80us 0xFFFEus _ -> MemoryBlock (ram.Stack, 0xFF80us) 
            | 0xFFFFus -> MemoryCell interrupts.InterruptEnable
            | _ -> Unmapped
        )
        |> Seq.toArray

    // Read mapped byte
    member this.Read8 (address: MemoryAddress) =
        match map.[int address] with
        | MemoryBlock (block, ``base``) ->
            block.Read (address - ``base``)
        | MemoryCell cell ->
            cell.Value
        | Unmapped ->
            0uy
    
    // Write mapped byte     
    member this.Write8 address value =
        match map.[int address] with
        | MemoryBlock (block, ``base``) ->
            do block.Write (address - ``base``) value
        | MemoryCell cell ->
            do cell.Value <- value
        | Unmapped ->
            ()

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
        
        
    