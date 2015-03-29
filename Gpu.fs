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

type LCDControl (init) =
    inherit ValueBackedIORegister(init)

    member this.BGDisplay = this.Value |> isBitSet 0
    member this.SpriteEnable = this.Value |> isBitSet 1
    member this.SpriteSize = this.Value |> isBitSet 2
    member this.BGTilemapSelect = this.Value |> isBitSet 3
    member this.BGAndWindowTileDataSelect = this.Value |> isBitSet 4
    member this.WindowEnabled = this.Value |> isBitSet 5
    member this.WIndowTileMapSelect = this.Value |> isBitSet 6
    member this.DisplayEnable = this.Value |> isBitSet 7

type VRAM () =
    let memory = readWriteMemoryBlock 8192

    let mapTiles offset count =
        let createTile index = Tile8x8(Array.sub memory (offset + index * 16) 16)
        Array.init 255 createTile

    let tiles1 = mapTiles 0 255
    let tiles0 = mapTiles 2048 255

    // Use useless constraints on the index to clarify that
    // tiles1 are indexes as 0 - 255 and tiles0 as -128 - 127
    let tile1 (index: uint8) = tiles1.[index |> int]
    let tile0 (index: int8) = tiles0.[index |> uint8 |> int]

    member this.MemoryBlock = memory


type RenderStage = |ScanOAM of int |ScanVRAM of int |HBlank of int |VBlank

type RenderAction = |Wait |RenderLine of int |RenderScreen

type RenderTimer (systemClock: Clock) =
    
    let oamLineCycles = 80UL

    let vramLineCycles = 172UL

    let hBlankCycles = 204UL

    let lineCycles = oamLineCycles + vramLineCycles + hBlankCycles

    let allLinesCycles = lineCycles * uint64 RESOLUTION.Height 

    let vBlankCycles = 4560UL

    let frameCycles = allLinesCycles + vBlankCycles

    let vSyncClock = FlipClock(Clock.derive systemClock 60<Hz>)

    let stage time =
        match time % frameCycles with
        | cycles when cycles <= allLinesCycles ->
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
            | HBlank line -> RenderLine(line)
            | VBlank -> RenderScreen
            | _ -> Wait
        else
            Wait
        


type GPU (systemClock, frameReceiver: FrameReceiver) =

    let vram = VRAM()

    let lcdc = LCDControl(0uy)
    
    let renderTimer = RenderTimer(systemClock)

    let screenBuffer = new System.Drawing.Bitmap(RESOLUTION.Width,RESOLUTION.Height)

    let drawLine n = {1..(RESOLUTION.Width)} |> Seq.iter (fun x -> screenBuffer.SetPixel(x-1,n,Color.GreenYellow))

    let drawScreen (FrameReceiver receiver) = receiver screenBuffer

    member this.VRAM = vram

    member this.LCDC = lcdc

    member this.Update () =
        match renderTimer.Action with
        | RenderLine line -> drawLine line
        | RenderScreen -> drawScreen frameReceiver
        | Wait -> ()




