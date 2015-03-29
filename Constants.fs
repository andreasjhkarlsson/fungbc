module Constants

open Units

// CPU Speed.
[<Literal>]
let GBC_SYSTEM_CLOCK_FREQUENCY = 4194000<Hz>

// Size of GBC memory
[<Literal>]
let ADDRESS_SPACE_SIZE = 65536

type Dimension = {Width: int; Height: int}

let RESOLUTION = {Width = 160; Height = 144}

let APPLICATION_TITLE = "FunGBC"