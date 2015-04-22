module Palette

open System.Drawing

type Palette = Color*Color*Color*Color

let create c0 c1 c2 c3 = c0, c1, c2, c3

let toArray (c0,c1,c2,c3) = [|c0;c1;c2;c3|]

let ofArray (a: array<Color>) = a.[0], a.[1], a.[2], a.[3]

module Predefined =
    let grayscale = create Color.White Color.LightGray Color.DarkGray Color.Black

    let terminalGreen = create Color.LightGreen Color.Green Color.DarkGreen Color.Black

    let fabolousPink = create Color.PapayaWhip Color.MistyRose Color.Pink Color.DarkMagenta

    let dreamyBlue = create Color.MintCream Color.PaleTurquoise Color.RoyalBlue Color.MidnightBlue 

    let horrorRed = create Color.WhiteSmoke Color.Red Color.DarkRed Color.Black

    let summerGreen = create Color.MintCream Color.Khaki Color.YellowGreen Color.OliveDrab