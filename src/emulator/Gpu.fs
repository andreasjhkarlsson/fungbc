﻿module Gpu

open System
open System.Runtime.InteropServices
open System.Diagnostics
open System.Threading
open IORegisters
open Tile
open Memory
open Clock
open Units
open Constants
open BitLogic
open Interrupts
open Palette
open Configuration
open Sound
open Types

type OBJBGPriority = |Above |Behind
type SpritePalette = |Palette0 |Palette1

type SpriteAttribute (oam: uint8 array, index) =


    member private this.c0 = oam.[4*index]
    member private this.c1 = oam.[4*index+1]
    member private this.c2 = oam.[4*index+2]
    member private this.c3 = oam.[4*index+3]

    member this.Y = this.c0
    member this.X = this.c1

    // Normalize coordinates (not sure why 1 & 9 instead of 0 & 8)
    member this.TX = (int this.c1) - 1
    member this.TY = (int this.c0) - 9

    member this.Tile = this.c2

    // Flags
    member this.Priority = match this.c3 |> bitStateOf 7 with |CLEAR -> Above |SET -> Behind
    member this.YFlip = this.c3 |> isBitSet 6 |> not
    member this.XFlip = this.c3 |> isBitSet 5 |> not
    member this.Palette = match this.c3 |> bitStateOf 4 with |CLEAR -> Palette0 |SET -> Palette1

type TileMapSelect = |Map1 |Map0

type TileMapMode = Signed | Unsigned

type TileDataSelect = |Tiles0 |Tiles1

type LCDControl (init) =
    inherit ValueBackedIORegister(init)

    member this.BGDisplay = this.Value |> isBitSet 0
    member this.SpriteEnable = this.Value |> isBitSet 1
    member this.SpriteSize = if this.Value |> isBitSet 2 then (8,16) else (8,8)
    member this.BGTilemapSelect = if this.Value |> isBitSet 3 then Map1 else Map0
    member this.BGAndWindowTileDataSelect = if this.Value |> isBitSet 4 then Tiles1 else Tiles0
    member this.WindowEnabled = this.Value |> isBitSet 5
    member this.WindowTileMapSelect = if this.Value |> isBitSet 6 then Map1 else Map0
    member this.DisplayEnable = this.Value |> isBitSet 7

type Coincidence = |LYC_NE_LY |LYC_E_LY

type Mode = |HBlank |VBlank |SearchingOAMRAM |LCDDriverDataTransfer

type LCDStatus (init) =
    inherit ValueBackedIORegister(init)
    member this.LYCLYCoincidenceInterrupt
        with get () = this.GetBit 6 |> isSet
        and set value = setIfTrue value |> this.SetBit 6 
    member this.OAMInterrupt 
        with get () = this.GetBit 5 |> isSet
        and set value = setIfTrue value |> this.SetBit 5
    member this.VBlankInterrupt
        with get () = this.GetBit 4 |> isSet
        and set value = setIfTrue value |> this.SetBit 4
    member this.HBlankInterrupt
        with get () = this.GetBit 3 |> isSet
        and set value = setIfTrue value |> this.SetBit 3
    member this.Coincidence
        with get () = match this.GetBit 2 with |SET -> LYC_E_LY |CLEAR -> LYC_NE_LY
        and set value = this.SetBit 2 <| match value with |LYC_E_LY -> SET |LYC_NE_LY -> CLEAR
    member this.Mode
        with get () = match this.Value &&& 0x3uy with 
                        |0uy -> HBlank
                        |1uy -> VBlank
                        |2uy -> SearchingOAMRAM
                        |3uy -> LCDDriverDataTransfer
                        |_ -> failwith "Cannot happen (uint8&0x3 <= 3)"
        and set value = this.Value <- (this.Value &&& (~~~0x3uy)) |||
                        (match value with
                        |HBlank -> 0uy
                        |VBlank -> 1uy
                        |SearchingOAMRAM -> 2uy
                        |LCDDriverDataTransfer -> 3uy)

type LY(init) =
    inherit ValueBackedIORegister(init)

    interface IMemoryCell with
        member x.Value
            with set _ = x.Value <- 0x0uy // Writing from memory resets the register


type PaletteMapRegister(init) =
    inherit ValueBackedIORegister(init)

    member this.Transparent (palette: Palette) = palette.[0]

    member this.Color (palette: Palette) index = palette.[(int this.Value >>> (index * 2)) &&& 0x3]

type TileDataBlock(memory: uint8 array,offset,mode) =

    let data =
        let createTile index =
            let segment = ArraySegment<uint8>(memory, offset + index * 16, 16)
            TileData segment

        Array.init 256 createTile 
       
    member this.Item index = data.[if mode = Signed then int (int8 index) + 128 else index]


type TileMap(memory: uint8 array,offset) =
    member this.Item (x,y) = memory.[offset + x + y * 32]

type VRAM () =
    let memory = ReadWriteMemoryBlock(8*kB)

    let oam = ReadWriteMemoryBlock(160<b>)

    let objectAttributeTable = [|0..39|] |> Array.map (fun index ->
            SpriteAttribute(oam.Array, index)
        )

    let tiles1 = TileDataBlock(memory.Array,0,Unsigned)

    let tiles0 = TileDataBlock(memory.Array,2048,Signed)

    let tileMap0 = TileMap(memory.Array,0x1800)
    let tileMap1 = TileMap(memory.Array,0x1C00)

    member this.Tiles1 = tiles1

    member this.Tiles0 = tiles0

    member this.TileMap0 = tileMap0

    member this.TileMap1 = tileMap1

    member this.ObjectAttribute index = objectAttributeTable.[index]

    member this.ObjectAttributes = objectAttributeTable

    member this.Memory = memory

    member this.OAM = oam
        
type GPURegisters () =
    let lcdc = LCDControl(0uy)
    let lcds = LCDStatus(0uy)
    let scx = ValueBackedIORegister(0uy)
    let scy = ValueBackedIORegister(0uy)
    let wx = ValueBackedIORegister(0uy)
    let wy = ValueBackedIORegister(0uy)
    let bgp = PaletteMapRegister(0uy)
    let obp0 = PaletteMapRegister(0uy)
    let obp1 = PaletteMapRegister(0uy)
    let ly = LY(0uy)
    let lyc = ValueBackedIORegister(0uy)

    member this.LCDC = lcdc
    member this.LCDS = lcds
    member this.SCX = scx
    member this.SCY = scy
    member this.WX = wx
    member this.WY = wy
    member this.BGP = bgp
    member this.OBP0 = obp0
    member this.OBP1 = obp1
    member this.LY = ly
    member this.LYC = lyc


type RenderStage = |ScanOAM of int |ScanVRAM of int |HBlank of int |VBlank of int

type GPU (sound: Sound.GBS,systemClock, interrupts: InterruptManager,config: Configuration ref) =

    let clock = Clock.derive systemClock systemClock.Frequency :?> DerivedClock

    let vram = VRAM()

    let registers = GPURegisters()

    let stopWatch = Stopwatch.StartNew()

    let drawLine y =
        
        let lineWidth = RESOLUTION.Width
        let screenHeight = RESOLUTION.Height
        let lcdc = registers.LCDC

        let drawTileLayer (tileMap: TileMap) (tileData: TileDataBlock) xOffset yOffset scroll = 
            let adjustedY = y + yOffset

            let palette = (!config).Palette

            let rec drawPixel x =
                if x >= 0 then
                    let adjustedX = x + xOffset
                    if scroll || (adjustedX >= 0 && adjustedX < lineWidth) then
                        let tile = tileData.[tileMap.[((adjustedX % 256) / 8),((adjustedY % 256) / 8)] |> int]
                        do
                            Tile.decode8x8 tile (adjustedX % 8) (adjustedY % 8)
                            |> registers.BGP.Color palette
                            |> (!config).Renderer.SetPixel x y
                    do drawPixel (x - 1)
            if scroll || (adjustedY >= 0 && adjustedY < screenHeight) then do drawPixel (lineWidth - 1)

        let tileData = match registers.LCDC.BGAndWindowTileDataSelect with |Tiles0 -> vram.Tiles0 |Tiles1 -> vram.Tiles1

        // Draw background
        if lcdc.BGDisplay then do
            let map = match registers.LCDC.BGTilemapSelect with |Map0 -> vram.TileMap0 |Map1 -> vram.TileMap1
            drawTileLayer map tileData (int registers.SCX.Value) (int registers.SCY.Value) true
        
        // Draw window
        if lcdc.WindowEnabled then do
            let map = match registers.LCDC.WindowTileMapSelect with |Map0 -> vram.TileMap0 |Map1 -> vram.TileMap1
            drawTileLayer map tileData (-(int registers.WX.Value - 7)) (-(int registers.WY.Value)) false

        // Draw sprites
        if lcdc.SpriteEnable then do
            
            let (_, spriteHeight) = lcdc.SpriteSize

            let drawSprite (sprite: SpriteAttribute) = 
                // Find palette for this sprite

                let palette = (!config).Palette

                let paletteReg = match sprite.Palette with |Palette0 -> registers.OBP0 |Palette1 -> registers.OBP1

                // Calculate y coordinate in tile for this sprite (NOTE: May be larger than 7!)
                let tileY = if sprite.YFlip then (spriteHeight-1) - (sprite.TY - y) else (sprite.TY - y)

                // Find tile to be decoded
                let tile = if tileY > 7 then vram.Tiles1.[sprite.Tile+1uy |> int] else vram.Tiles1.[sprite.Tile |> int]

                // Wrap y
                let tileY = tileY % 8 

                let rec drawTileLine x =
                    if x >= 0 then
                        // Calculate destination x value
                        let screenX = sprite.TX - x

                        // Flip x if flag is set
                        let tileX = if sprite.XFlip then 7 - x else x

                        if screenX >= 0 && screenX < lineWidth then
                            let colorIndex = Tile.decode8x8 tile tileX tileY
                            // Apparently index 0 is always transparent (regardless of palette??????)
                            if colorIndex <> 0 then 
                                // Draw pixel if sprite is above background or if background is transparent
                                if sprite.Priority = Above || (((!config).Renderer.GetPixel screenX y) = (registers.BGP.Transparent palette)) then
                                   do (paletteReg.Color palette colorIndex) |> (!config).Renderer.SetPixel screenX y    

                        // Next column
                        do drawTileLine (x - 1)
                                
                do drawTileLine 7
            
            do
                vram.ObjectAttributes
                |> Array.filter (fun s -> s.TY >= y && s.TY <= (y + (spriteHeight-1)))
                |> Array.sortBy (fun s -> -(int s.X)) // Lower value X should overlap (draw after) larger x (maybe sacrifice this for performance?).
                |> Array.iter drawSprite


    let drawScreen = (!config).Renderer.Flush

    let isVBlank = function |VBlank _ -> true |_ -> false

    let stageAt time =
        // Timing constants
        let oamLineCycles = 80UL
        let vramLineCycles = 172UL
        let hBlankCycles = 204UL
        let lineCycles = oamLineCycles + vramLineCycles + hBlankCycles
        let allLinesCycles = lineCycles * uint64 RESOLUTION.Height 
        let vBlankCycles = 4560UL
        let frameCycles = allLinesCycles + vBlankCycles

        match time % frameCycles with
        | cycles when cycles < allLinesCycles ->
            let line = cycles / lineCycles |> int
            match cycles % lineCycles with
            | cycles when cycles > (oamLineCycles + vramLineCycles) ->
                HBlank(line)
            | cycles when cycles > oamLineCycles ->
                ScanVRAM(line)
            | cycles ->
                ScanOAM(line)
        | cycles ->
            VBlank ((cycles - allLinesCycles) / (vBlankCycles / 10UL) |> int)  
            
    let mutable lastStage = VBlank 0 

    member this.VRAM = vram

    member this.Registers = registers

    member this.ForceRedraw () =
        do
            {0..143} |> Seq.iter drawLine   
            drawScreen ()

    member this.Reset () =
        lastStage <- VBlank 0
        clock.Reset ()

    member this.Update () =
        // Extract some registers
        let lcds = registers.LCDS
        let lcdc = registers.LCDC
        let ly = registers.LY
        let lyc = registers.LYC

        if lcdc.DisplayEnable then

            let stage = stageAt clock.Ticks

            if stage <> lastStage then
            
                match stage with
                | HBlank line ->
                    lcds.Mode <- Mode.HBlank

                    // Generate LCDC interrupt from HBlank?
                    if lcds.HBlankInterrupt then
                        interrupts.Current.Set <- LCDC

                    do drawLine line
                | VBlank t  ->
                    if not (lastStage |> isVBlank) then do

                        lcds.Mode <- Mode.VBlank

                        // Should VBlank generate LCDC interrupt?
                        if lcds.VBlankInterrupt then
                            interrupts.Current.Set <- LCDC

                        // Generate VBlank interrupt regardless of the above
                        if interrupts.Enable then
                            interrupts.Current.Set <- Interrupts.VBlank

                        if (!config).EnableAudio then sound.Mixer |> Mixer.flush

                        do drawScreen ()

                        match (!config).Speed with
                        | Speed factor ->
                            
                            let rec limit () =

                                let ticksLeft = ((float Stopwatch.Frequency) / (factor * 60.0) - (float stopWatch.ElapsedTicks))

                                if ticksLeft >= 0.0 then
                                    do
                                        ((ticksLeft / (float) Stopwatch.Frequency))
                                        |> (*) (10.0**6.0)
                                        |> int
                                        |> (!config).Idle

                                    limit ()

                            if (!config).EnableAudio then do sound.Mixer |> Mixer.sync
                            do limit ()

                        stopWatch.Restart()
                                
                    ly.Value <- uint8 <| RESOLUTION.Height + t

                | ScanOAM line ->
                    lcds.Mode <- Mode.SearchingOAMRAM // Correct?
                    ly.Value <- uint8 <| line 

                    // Generate LCDC interrupt from OAM?
                    if lcds.OAMInterrupt then
                        interrupts.Current.Set <- LCDC

                | ScanVRAM _ ->
                    lcds.Mode <- Mode.LCDDriverDataTransfer // Correct?

            if lcds.LYCLYCoincidenceInterrupt then
                if ly.Value = lyc.Value then
                    interrupts.Current.Set <- LCDC
                    lcds.Coincidence <- LYC_E_LY
                else
                    lcds.Coincidence <- LYC_NE_LY

            lastStage <- stage
        else
            clock.Reset ()
            ly.Value <- 0uy
            lcds.Mode <- Mode.HBlank
