module Tile

open MemoryCell
open BitLogic

// Would be GREAT if you could somehow enforce the length of the array to be 16
type TileData = |TileData of MemoryBlock

let inline decode8x8 (TileData data) x y =
    (*
        == Pixels are stored like this (one row) ==

                    x = 0   1   2   3   4   5   6   7 
        byte 0          b7  b6  b5  b4  b3  b2  b1  b0
                        +   +   +   +   +   +   +   +
        byte 1          b7  b6  b5  b4  b3  b2  b1  b0

        Rows are stored sequentially (so y0 = byte0 & byte1, y1 = byte2 & byte3, etc) 
        for a total of 16 bytes
    *)

    let b0 = data.[y * 2].Value
    let b1 = data.[y * 2 + 1].Value

    (bitStateOf (7 - x) b0 |> bitStateToValue) |||
    (bitStateOf (7 - x) b1 |> bitStateToValue <<< 1) 