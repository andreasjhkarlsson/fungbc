module BitLogic

type BitState = |SET |CLEAR

let inline bitStateToValue state: ^T = match state with |SET -> LanguagePrimitives.GenericOne |CLEAR -> LanguagePrimitives.GenericZero

let inline isBitSet bit value = ((value >>> bit) &&& LanguagePrimitives.GenericOne) = LanguagePrimitives.GenericOne

let inline isSet state = state = SET

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

let inline bitStateInvert state = match state with |SET -> CLEAR |CLEAR -> SET

let inline setBit bit value = value ||| (LanguagePrimitives.GenericOne <<< bit)

let inline clearBit bit value = value &&& (~~~(LanguagePrimitives.GenericOne <<< bit))

let inline controlBit bit state = if state = SET then setBit bit else clearBit bit

let inline setIfZero value = if value = LanguagePrimitives.GenericZero then SET else CLEAR

let inline setIfNotZero value = value |> setIfZero |> bitStateInvert

let inline setIfTrue boolean = match boolean with |true -> SET |false -> CLEAR

let inline setIfFalse boolean = match boolean with |false -> SET |true -> CLEAR

let inline swapNibbles value = ((value >>> 4) &&& 0xFuy) ||| (value <<< 4)

let inline nibbles value = ((value &&& 0xF0uy) >>> 4, value &&& 0xFuy)

let inline highNibble value = fst (nibbles value)

let inline lowNibble value = snd (nibbles value)