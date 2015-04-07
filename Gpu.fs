module Gpu

open System.Drawing
open IORegisters
open Tile
open MemoryCell
open Clock
open Units
open Constants
open BitLogic
open Interrupts

type OBJBGPriority = |Above |Behind
type TileFlip = |Normal |Flipped
type SpritePalette = |Palette0 |Palette1

type SpriteAttribute (c0: MemoryCell,c1: MemoryCell,c2: MemoryCell,c3: MemoryCell) =
    member this.Y = c0.Value
    member this.X = c1.Value
    member this.Tile = c2.Value

    // Flags
    member this.Priority = match c3.Value |> bitStateOf 7 with |CLEAR -> Above |SET -> Behind
    member this.YFlip = match c3.Value |> bitStateOf 6 with |CLEAR -> Normal |SET -> Flipped
    member this.XFlip = match c3.Value |> bitStateOf 5 with |CLEAR -> Normal |SET -> Flipped
    member this.Palette = match c3.Value |> bitStateOf 4 with |CLEAR -> Palette0 |SET -> Palette1


type FrameReceiver =
    |FrameReceiver of (Bitmap -> unit)
    member x.Value = let (FrameReceiver v) = x in v // How does this syntax work??

type TileMapSelect = |Map1 |Map0

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
                        |_ -> raise <| System.Exception("Cannot happen (uint8&0x3 <= 3)")
        and set value = this.Value <- (this.Value &&& (~~~0x3uy)) |||
                        (match value with
                        |HBlank -> 0uy
                        |VBlank -> 1uy
                        |SearchingOAMRAM -> 2uy
                        |LCDDriverDataTransfer -> 3uy)

type LY(init) =
    inherit ValueBackedIORegister(init)

    // Writing from memory resets the register
    override this.MemoryValue with set value = base.MemoryValue <- 0x0uy


type BGPalette(init) =
    inherit ValueBackedIORegister(init)

    let colors = [|Color.White; Color.LightGray; Color.DarkGray; Color.Black|]

    member this.Color index = colors.[(int this.Value >>> (index * 2)) &&& 0x3]


type VRAM () =
    let memory = 8*kB |> readWriteMemoryBlock

    let oam = 160<byte> |> readWriteMemoryBlock

    let objectAttributeTable = [|0..39|] |> Array.map (fun index ->
            SpriteAttribute(oam.[4*index],
                            oam.[4*index+1],
                            oam.[4*index+2],
                            oam.[4*index+3])
        )

    let mapTiles offset count =
        let createTile index = Array.sub memory (offset + index * 16) 16 |> TileData
        Array.init 256 createTile

    let tiles1 = mapTiles 0 256
    let tiles0 = mapTiles 2048 256

    let tileMap0 = Array.sub memory 0x1800 1024
    let tileMap1 = Array.sub memory  0x1C00 1024

    // Use useless constraints on the index to clarify that
    // tiles1 are indexes as 0 - 255 and tiles0 as -128 - 127
    member this.Tile1 (index: uint8) =
        tiles1.[index |> int]
    member this.Tile0 (index: int8) =
        tiles0.[int index + 128]

    member this.TileMap0 x y = (Array.get tileMap0 (x + y * 32)).Value
    member this.TileMap1 x y = (Array.get tileMap1 (x + y * 32)).Value

    member this.ObjectAttribute index = objectAttributeTable.[index]

    member this.MemoryBlock = memory

    member this.OAM = oam
        
type GPURegisters () =
    let lcdc = LCDControl(0uy)
    let lcds = LCDStatus(0uy)
    let scx = ValueBackedIORegister(0uy)
    let scy = ValueBackedIORegister(0uy)
    let bgp = BGPalette(0uy)
    let ly = LY(0uy)
    let lyc = ValueBackedIORegister(0uy)

    member this.LCDC = lcdc
    member this.LCDS = lcds
    member this.SCX = scx
    member this.SCY = scy
    member this.BGP = bgp
    member this.LY = ly
    member this.LYC = lyc


type RenderStage = |ScanOAM of int |ScanVRAM of int |HBlank of int |VBlank of int

type GPU (systemClock: Clock,interrupts: InterruptManager,frameReceiver: FrameReceiver) =

    let vram = VRAM()

    let registers = GPURegisters()

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    let screenBuffer = new System.Drawing.Bitmap(RESOLUTION.Width,RESOLUTION.Height)

    let drawLine y =

        let bgMap = match registers.LCDC.BGTilemapSelect with |Map0 -> vram.TileMap0 |Map1 -> vram.TileMap1

        let tileData = match registers.LCDC.BGAndWindowTileDataSelect with |Tiles0 -> int8 >> vram.Tile0|Tiles1 -> vram.Tile1

        let cy = int registers.SCY.Value + y

        let drawPixel x =
            let cx = int registers.SCX.Value + x
            let tileIndex = bgMap ((cx % 256) / 8) ((cy % 256) / 8)
            let tile = tileData tileIndex
            let color = Tile.decode8x8 tile (cx % 8) (cy % 8) |> registers.BGP.Color
            screenBuffer.SetPixel(x,y,color)

        {0..(RESOLUTION.Width - 1)} |> Seq.iter drawPixel

    let drawScreen (FrameReceiver receiver) = receiver screenBuffer

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

    member this.Update () =
        // Extract some registers
        let lcds = registers.LCDS
        let lcdc = registers.LCDC
        let ly = registers.LY
        let lyc = registers.LYC

        let stage = stageAt systemClock.Ticks

        if stage <> lastStage then
            
            match stage with
            | HBlank line ->
                lcds.Mode <- Mode.HBlank

                // Generate LCDC interrupt from HBlank?
                if lcds.HBlankInterrupt then
                    interrupts.Current.Set <- LCDC

                drawLine line
            | VBlank t  ->
                if not (lastStage |> isVBlank) then
                    let fps = 1000.0 / float stopWatch.ElapsedMilliseconds
                    //printfn "FPS: %.2f" fps
                    stopWatch.Restart()
                    lcds.Mode <- Mode.VBlank

                    // Should VBlank generate LCDC interrupt?
                    if lcds.VBlankInterrupt then
                        interrupts.Current.Set <- LCDC

                    // Generate VBlank interrupt regardless of the above
                    interrupts.Current.Set <- Interrupts.VBlank

                    drawScreen frameReceiver

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




