module Gpu

open System.Drawing
open IORegisters
open Tile
open MemoryCell
open Clock
open Units
open Constants
open BitLogic

type FrameReceiver =
    |FrameReceiver of (Bitmap -> unit)
    member x.Value = let (FrameReceiver v) = x in v // How does this syntax work??

type BGTileMapSelect = |Map1 |Map0

type TileDataSelect = |Tiles0 |Tiles1

type LCDControl (init) =
    inherit ValueBackedIORegister(init)

    member this.BGDisplay = this.Value |> isBitSet 0
    member this.SpriteEnable = this.Value |> isBitSet 1
    member this.SpriteSize = this.Value |> isBitSet 2
    member this.BGTilemapSelect = if this.Value |> isBitSet 3 then Map1 else Map0
    member this.BGAndWindowTileDataSelect = if this.Value |> isBitSet 4 then Tiles1 else Tiles0
    member this.WindowEnabled = this.Value |> isBitSet 5
    member this.WIndowTileMapSelect = this.Value |> isBitSet 6
    member this.DisplayEnable = this.Value |> isBitSet 7

type BGPalette(init) =
    inherit ValueBackedIORegister(init)

    let colors = [|Color.White; Color.LightGray; Color.DarkGray; Color.Black|]

    member this.Color index = colors.[(int this.Value >>> (index * 2)) &&& 0x3]


type VRAM () =
    let memory = readWriteMemoryBlock 8192

    let mapTiles offset count =
        let createTile index = Tile8x8(Array.sub memory (offset + index * 16) 16)
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
        tiles0.[index |> uint8 |> int]

    member this.TileMap0 x y = (Array.get tileMap0 (x + y * 32)).Value
    member this.TileMap1 x y = (Array.get tileMap1 (x + y * 32)).Value
    member this.MemoryBlock = memory


type RenderStage = |ScanOAM of int |ScanVRAM of int |HBlank of int |VBlank

type RenderAction = |Wait |RenderLine of int |RenderScreen

type RenderTimer (systemClock: Clock) =

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let oamLineCycles = 80UL

    let vramLineCycles = 172UL

    let hBlankCycles = 204UL

    let lineCycles = oamLineCycles + vramLineCycles + hBlankCycles

    let allLinesCycles = lineCycles * uint64 RESOLUTION.Height 

    let vBlankCycles = 4560UL

    let frameCycles = allLinesCycles + vBlankCycles

    let stage time =
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
        | _ ->
            VBlank

    let mutable lastStage = VBlank

    member this.Action =
        let stage = stage systemClock.Ticks
        if stage <> lastStage then
            lastStage <- stage
            match stage with
            | HBlank line ->
                RenderLine(line)
            | VBlank ->
                let fps = 1000.0 / float stopWatch.ElapsedMilliseconds
                printfn "FPS: %.2f" fps
                stopWatch.Restart()
                RenderScreen
            | _ ->
                Wait
        else
            Wait
        
type GPURegisters () =
    let lcdc = LCDControl(0uy)
    let scx = ValueBackedIORegister(0uy)
    let scy = ValueBackedIORegister(0uy)
    let bgp = BGPalette(0uy)

    member this.LCDC = lcdc
    member this.SCX = scx
    member this.SCY = scy
    member this.BGP = bgp


type GPU (systemClock, frameReceiver: FrameReceiver) =

    let vram = VRAM()

    let registers = GPURegisters()
    
    let renderTimer = RenderTimer(systemClock)

    let screenBuffer = new System.Drawing.Bitmap(RESOLUTION.Width,RESOLUTION.Height)

    let drawLine y =

        let bgMap = match registers.LCDC.BGTilemapSelect with |Map0 -> vram.TileMap0 |Map1 -> vram.TileMap1

        let tileData = match registers.LCDC.BGAndWindowTileDataSelect with |Tiles0 -> int8 >> vram.Tile0|Tiles1 -> vram.Tile1

        let cy = int registers.SCY.Value + y

        let drawPixel x =
            let cx = int registers.SCX.Value + x
            let tileIndex = bgMap ((cx % 256) / 8) ((cy % 256) / 8)
            let tile = tileData tileIndex
            let color = tile.[cx % 8, cy % 8] |> registers.BGP.Color
            screenBuffer.SetPixel(x,y,color)

        {0..(RESOLUTION.Width - 1)} |> Seq.iter drawPixel

    let drawScreen (FrameReceiver receiver) = receiver screenBuffer

    member this.VRAM = vram

    member this.Registers = registers

    member this.Update () =
        match renderTimer.Action with
        | RenderLine line ->
            drawLine line
        | RenderScreen ->
            drawScreen frameReceiver
        | Wait -> 
            ()




