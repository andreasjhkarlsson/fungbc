module Gpu

open IORegisters
open Tile
open MemoryCell


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


type GPU () =
    let vram = VRAM()

    member this.VRAM = vram


