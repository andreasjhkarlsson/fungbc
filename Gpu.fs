module Gpu

open System.Drawing
open IORegisters
open Tile
open MemoryCell
open Clock
open Units
open Constants


type FrameReceiver = |FrameReceiver of (Bitmap -> unit)

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


type GPU (systemClock, frameReciver: FrameReceiver) =
    
    let vSyncClock = FlipClock(Clock.derive systemClock 60<Hz>)

    let frame = System.Drawing.Bitmap(RESOLUTION.Width,RESOLUTION.Height)

    let startVSync () =
        vSyncClock.Reset ()
    
    let vram = VRAM()

    member this.VRAM = vram

    member this.Update () =
        if vSyncClock.Flipped then
            startVSync ()

