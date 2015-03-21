module BitLogic

type BitState = |SET |CLEAR

let inline bitStateToValue state: ^T = match state with |SET -> LanguagePrimitives.GenericOne |CLEAR -> LanguagePrimitives.GenericZero

let inline isBitSet bit value = ((value >>> bit) &&& LanguagePrimitives.GenericOne) = LanguagePrimitives.GenericOne

let inline bitStateOf bit value = if isBitSet bit value then SET else CLEAR

let inline bitChanged bit oldValue newValue = oldValue ^^^ newValue |> isBitSet bit

let inline bitsChanged bitPattern oldValue newValue = ((oldValue ^^^ newValue) &&& bitPattern) > LanguagePrimitives.GenericZero

let inline (|BitChanged|_|) bit (oldValue,newValue) =
    match bitChanged bit oldValue newValue with
    | true -> Some (bitStateOf bit newValue)
    | false -> None

let inline (|BitsChanged|_|) bitPattern (oldValue, newValue) =
    match bitsChanged bitPattern oldValue newValue with
    | true -> Some (newValue &&& bitPattern)
    | false -> None

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