module Constants

open Units

// CPU Speed.
[<Literal>]
let GBC_SYSTEM_CLOCK_FREQUENCY = 4194000<Hz>

// Size of GBC memory
[<Literal>]
let ADDRESS_SPACE_SIZE = 65536

// [<Literal>] Tuples cannot be literals (probably because they are heap allocated?)
let RESOLUTION = (160, 144)

let VBLANK = 59.7<Hz>