module Rom

open System.IO
open MemoryCell
open BitLogic
open Units
open Misc

[<AbstractClass>]
type ROM () =
    abstract ROMBlock: array<MemoryCell>
    abstract RAMBlock: array<MemoryCell> option

type GBCFlag = |GB |GBC |GBC_ONLY

type SGBFlag = |SGB |NO_SGB

type Ram = |Ram

type Battery = |Battery

type Timer = |Timer

type Rumble = |Rumble


type CartridgeType =
    | ROM of (Ram option)*(Battery option)
    | MBC1 of (Ram option)*(Battery option)
    | MBC2 of (Battery option)
    | MMM01 of (Ram option)*(Battery option)
    | MBC3 of (Timer option)*(Ram option)*(Battery option)
    | MBC4 of (Ram option)*(Battery option)
    | MBC5 of (Ram option)*(Battery option)*(Rumble option)
    | PocketCamera
    | Bandai_TAMA5
    | HuC3
    | HuC1 of Ram*Battery
    | Unknown
    with
        member this.Ram =
            match this with
                |ROM (Some ram, _)
                |MBC1 (Some ram, _)
                |MBC3 (_,Some ram, _)
                |MBC4 (Some ram, _) 
                |MBC5 (Some ram, _, _) ->
                    Some ram
                |_ ->
                    None

type Region = |Japan |NotJapan |Unknown

type CartHeader(bytes: array<uint8>) =
    
    let subBytes fromIndex toIndex = Array.sub bytes fromIndex (toIndex - fromIndex + 1)

    let toText = System.Text.Encoding.ASCII.GetString

    let parseError message = System.Exception("Error parsing rom header. " + message) |> raise

    member this.NintendoLogo = subBytes 0x4 0x33

    member this.Title =
        subBytes 0x34 0x43
        |> Array.toSeq
        |> Seq.takeWhile (not << (=) 0uy)
        |> Seq.toArray
        |> toText

    member this.ManufacturerCode = subBytes 0x3F 0x42 |> toText

    member this.GBCFlag = match int bytes.[0x43] with 
                          | 0x80 -> GBC
                          | 0xC0 -> GBC_ONLY
                          | _ -> GB

    member this.LicenseeCode = subBytes 0x44 0x45 |> toText

    member this.SGBFlag = match int bytes.[0x46] with
                          | 0x03 -> SGB
                          | 0x0 |_ -> NO_SGB

    member this.CartridgeType = match int bytes.[0x47] with
                                | 0x00 -> ROM (None, None)
                                | 0x01 -> MBC1 (None, None)
                                | 0x02 -> MBC1 (Some Ram, None)
                                | 0x03 -> MBC1 (Some Ram, Some Battery)
                                | 0x05 -> MBC2 (None)
                                | 0x06 -> MBC2 (Some Battery)
                                | 0x08 -> ROM (Some Ram, None)
                                | 0x09 -> ROM (Some Ram, Some Battery)
                                | 0x0B -> MMM01 (None, None)
                                | 0x0C -> MMM01 (Some Ram, None)
                                | 0x0D -> MMM01 (Some Ram, Some Battery)
                                | 0x0F -> MBC3 (Some Timer, None, Some Battery)
                                | 0x10 -> MBC3 (Some Timer, Some Ram, Some Battery)
                                | 0x11 -> MBC3 (None, None, None)
                                | 0x12 -> MBC3 (None, Some Ram, None)
                                | 0x13 -> MBC3 (None, Some Ram, Some Battery)
                                | 0x15 -> MBC4 (None, None)
                                | 0x16 -> MBC4 (Some Ram, None)
                                | 0x17 -> MBC4 (Some Ram, Some Battery)
                                | 0x19 -> MBC5 (None, None, None)
                                | 0x1A -> MBC5 (Some Ram, None, None)
                                | 0x1B -> MBC5 (Some Ram, Some Battery, None)
                                | 0x1C -> MBC5 (None, None, Some Rumble)
                                | 0x1D -> MBC5 (Some Ram, None, Some Rumble)
                                | 0x1E -> MBC5 (Some Ram, Some Battery, Some Rumble)
                                | 0xFC -> PocketCamera
                                | 0xFD -> Bandai_TAMA5
                                | 0xFE -> HuC3
                                | 0xFF -> HuC1 (Ram, Battery)
                                | _ -> CartridgeType.Unknown

        member this.Size = ((int <| 32 * kB) <<< (int bytes.[0x48])) * 1<byte>

        member this.RAMSize = match int bytes.[0x49] with |0x1 -> 2 * kB |0x2 -> 8 * kB |0x3 -> 32 * kB |_ -> 0<byte>

        member this.Region = match int bytes.[0x4A] with |0x0 -> Japan |0x1 -> NotJapan |_ -> Unknown

        member this.LegacyLicenseeCode = bytes.[0x4B]

        member this.VersionNumber = bytes.[0x4C]

        member this.HeaderChecksum = bytes.[0x4D]

        member this.GlobalChecksum = (bytes.[0x4E] <<< 8) ||| (bytes.[0x4F]) // Big endian

[<AbstractClass>]
type CartROM (header,data) =
    inherit ROM ()

    let header = CartHeader(Array.sub data 0x100 0x50)


    let romBanks =
        [|0..(header.Size / (16*kB) - 1)|]
        |> Array.map (fun n ->
            Array.sub data (n * (16 * kB |> int)) (16 * kB |> int)
        )

    let ramBanks =
        [|0..(header.RAMSize / (8 * kB) - 1)|]
        |> Array.map (fun _ ->
            Array.create (8*kB |> int) 0uy
        )


    abstract RAMBank: unit -> int

    abstract HighROMBank: unit -> int

    abstract ROMWrite: uint16 -> uint8 -> unit

    member this.LowROMBank () = 0

    member this.Header = header

    default this.ROMWrite address value = ()

    override this.RAMBlock =
        if header.CartridgeType.Ram.IsSome then
            Some <| readWriteMemoryBlock (8*kB)
        else
            None

    override this.ROMBlock =

        let switchableCell bank address =
            let get () = romBanks.[bank ()].[address]
            let set value = this.ROMWrite (uint16 address) value
            VirtualCell(get,set) :> MemoryCell

        initMemoryBlock (32 * kB) (fun address ->
            match address with
            | Range 0x0000 0x3FFF offset ->
                switchableCell this.LowROMBank offset
            | Range 0x4000 0x7FFF offset ->
                switchableCell this.HighROMBank offset
            | _ -> failwith "Unmapped"
        )


type StaticCartROM(header,data) =
    inherit CartROM(header,data)

    override this.HighROMBank () = 1

    override this.RAMBank () = 0

type Mode = |Rom |Ram

type MBC1CartROM(header,data) =
    inherit CartROM(header,data)

    let mutable activeROMBank = 1uy

    let mutable activeRAMBank = 0uy

    let mutable mode = Rom

    override this.HighROMBank () = if mode = Rom then int activeROMBank else activeROMBank &&& 0x1Fuy |> int

    override this.RAMBank () = if mode = Ram then int activeRAMBank else 0

    override this.ROMWrite address value =

        match address with
        | Range 0x0000us 0x1FFFus _ ->
            ()
        | Range 0x2000us 0x3FFFus _ ->
            // Only the low 5 bits are valid for this regsiter
            let value = value &&& 0x1Fuy
            activeROMBank <- (activeROMBank &&& 0x60uy) ||| (value)
            // Setting bank 0 really means set bank 1
            if value = 0uy then
                activeROMBank <- activeROMBank |> setBit 0

        | Range 0x4000us 0x5FFFus _ ->
            // Only the low 2 bits are valid
            let value = value &&& 0x3uy
            activeRAMBank <- value
            activeROMBank <- (activeROMBank &&& 0x1Fuy) ||| (value <<< 5)
        | Range 0x6000us 0x7FFFus _ ->
            mode <- if value = 0uy then Rom else Ram
        | _ ->
            ()


let loadFromCartDump path =
    let data = File.ReadAllBytes path
    let header = CartHeader(Array.sub data 0x100 0x50)

    match header.CartridgeType with
    | CartridgeType.ROM _ ->
        StaticCartROM(header,data) :> CartROM
    | CartridgeType.MBC1 _ ->
        MBC1CartROM(header,data) :> CartROM
    | _ ->
        raise <| System.Exception("Cart type unsupported")