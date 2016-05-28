module Palette

type Color = int

type Palette = Color*Color*Color*Color

let create c0 c1 c2 c3 = c0, c1, c2, c3

let toArray (c0,c1,c2,c3) = [|c0;c1;c2;c3|]

let ofArray (a: array<Color>) = a.[0], a.[1], a.[2], a.[3]

module Predefined =
    let grayscale = create (0xFFFFFFFF) (0xFFD3D3D3) (0xFFA9A9A9) (0xFF000000)

    let terminalGreen = create (0xff90ee90) (0xFF008000) (0xFF006400) (0xFF000000)

    let fabolousPink = create (0xFFFFEFD5) (0xFFFFE4E1) (0xFFFFC0CB) (0xFF8B008B) 

    let dreamyBlue = create (0xFFF5FFFA) (0xFFAFEEEE) (0xFF4169E1) (0xFF191970) 

    let horrorRed = create (0xFFF5F5F5) (0xFFFF0000) (0xFF8B0000) (0xFF000000)

    let summerGreen = create (0xFFF5FFFA) (0xFFF0E68C) (0xFF9ACD32) (0xFF6B8E23)