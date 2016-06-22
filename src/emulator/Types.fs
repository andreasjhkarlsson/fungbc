
module Types

open Units

type AudioSample = byte

type Color = int

type Speed = Speed of float
    with
        member x.Normal = x = Speed 1.0
        member x.Multiplier = let (Speed speed) = x in speed