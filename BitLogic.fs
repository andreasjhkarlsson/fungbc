module BitLogic

type BitState = |SET |CLEAR

let inline bitStateToValue state: ^T = match state with |SET -> LanguagePrimitives.GenericOne |CLEAR -> LanguagePrimitives.GenericZero

let inline isBitSet bit value = ((value >>> bit) &&& LanguagePrimitives.GenericOne) = LanguagePrimitives.GenericOne

let inline bitStateOf bit value = if isBitSet bit value then SET else CLEAR

let bitStateInvert = function |SET -> CLEAR |CLEAR -> SET

let inline setBit bit value = value ||| (LanguagePrimitives.GenericOne <<< bit)

let inline clearBit bit value = value &&& (~~~(LanguagePrimitives.GenericOne <<< bit))

let inline setIfZero value = if value = LanguagePrimitives.GenericZero then SET else CLEAR

let inline setIfNotZero value = value |> setIfZero |> bitStateInvert

let setIfTrue = function |true -> SET |false -> CLEAR

let setIfFalse = function |false -> SET |true -> CLEAR

let swapNibbles value = ((value >>> 4) &&& 0xFuy) ||| (value <<< 4)

let nibbles value = ((value &&& 0xF0uy) >>> 4, value &&& 0xFuy)

let highNibble value = fst (nibbles value)

let lowNibble value = snd (nibbles value)