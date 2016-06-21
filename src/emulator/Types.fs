
module Types

open Units

type AudioSample = byte

type Color = int

type Speed = |Unlimited |Limit of int<Hz>