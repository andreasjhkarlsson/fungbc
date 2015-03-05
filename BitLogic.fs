module BitLogic

type BitState = |SET |CLEAR

let bitStateToValue state = match state with | SET -> 1uy | CLEAR -> 0uy

let bitStateOf bit value  = if ((value >>> bit) &&& 1uy) = 1uy then SET else CLEAR

let bitStateInvert = function |SET -> CLEAR |CLEAR -> SET

let setBit bit value = value ||| (1uy <<< bit)

let clearBit bit value = value &&& (~~~(1uy <<< bit))