module Tile

open MemoryCell
open BitLogic

type Tile8x8 (pixelData: MemoryBlock) =

    member this.Item
        with get (x,y) =
            let b1 = pixelData.[y * 2].Value
            let b2 = pixelData.[y * 2 + 1].Value

            (bitStateOf (7 - x) b1 |> bitStateToValue) |||
            (bitStateOf (7 - x) b2 |> bitStateToValue <<< 1)



