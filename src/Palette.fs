module Palette

open System.Drawing

type Palette = Color*Color*Color*Color

let create c0 c1 c2 c3 = c0, c1, c2, c3

let toArray (c0,c1,c2,c3) = [|c0;c1;c2;c3|]

let ofArray (a: array<Color>) = a.[0], a.[1], a.[2], a.[3]

let grayscale = create Color.White Color.LightGray Color.DarkGray Color.Black

let greenTerminal = create Color.LightGreen Color.Gray Color.DarkGray Color.Black