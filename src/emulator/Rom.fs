module Rom

open System.IO
open MemoryCell
open BitLogic
open Units
open Misc

type SaveFile =
    abstract Load: unit -> uint8[] option
    abstract Save: uint8[] -> unit

[<AbstractClass>]
type ROM () =
    abstract ROMBlock: array<IMemoryCell>
    abstract RAMBlock: array<IMemoryCell> option

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

    let toText (chars: uint8[]) = chars |> Array.map (char >> string) |> String.concat ""

    let parseError message = System.Exception("Error parsing rom header. " + message) |> raise

    member this.NintendoLogo = subBytes 0x4 0x33

    member this.Title =
        subBytes 0x34 0x43
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

        member this.Size = ((int <| 32 * kB) <<< (int bytes.[0x48])) * 1<b>

        member this.RAMSize = match int bytes.[0x49] with |0x1 -> 2 * kB |0x2 -> 8 * kB |0x3 -> 32 * kB |_ -> 0<b>

        member this.Region = match int bytes.[0x4A] with |0x0 -> Japan |0x1 -> NotJapan |_ -> Unknown

        member this.LegacyLicenseeCode = bytes.[0x4B]

        member this.VersionNumber = bytes.[0x4C]

        member this.HeaderChecksum = bytes.[0x4D]

        member this.GlobalChecksum = (bytes.[0x4E] <<< 8) ||| (bytes.[0x4F]) // Big endian

[<AbstractClass>]
type CartROM (header,data,saveFile: SaveFile) =
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

    do
        match saveFile.Load () with
        | Some saveFile ->
            // Load RAM contents from savefile
            saveFile |> Array.iteri (fun address value ->
                let bank = int (address / (8 *kB))
                let bankAddress = address % (8*kB |> int)
                ramBanks.[bank].[bankAddress] <- value
            ) 
        | None ->
            ()


    member this.GetRAMData () = Array.concat ramBanks

    abstract RAMBank: unit -> int

    abstract HighROMBank: unit -> int

    abstract ROMWrite: uint16 -> uint8 -> unit

    member this.LowROMBank () = 0

    member this.Header = header

    default this.ROMWrite address value = ()

    override this.RAMBlock =
        if header.CartridgeType.Ram.IsSome then
            let switchableCell bank address =
                {
                    new MemoryCell() with
                        member x.Value
                            with get () = ramBanks.[(bank ())].[address]
                            and set value = ramBanks.[(bank ())].[address] <- value
                } :> IMemoryCell

            initMemoryBlock (8 * kB) (fun address ->
                switchableCell this.RAMBank address
            ) |> Some
        else
            None

    override this.ROMBlock =

        let switchableCell bank baseAddress offset =
                {
                    new MemoryCell() with
                        member x.Value
                            with get () =
                                try
                                    romBanks.[(bank ())].[offset]
                                with error ->
                                    failwith (sprintf "error reading address %04X from rom bank %d" (baseAddress + offset) (bank()))
                            and set value = this.ROMWrite (baseAddress + offset |> uint16) value
                } :> IMemoryCell

        initMemoryBlock (32 * kB) (fun address ->
            match address with
            | Range 0x0000 0x3FFF offset ->
                switchableCell this.LowROMBank 0x0000 offset
            | Range 0x4000 0x7FFF offset ->
                switchableCell this.HighROMBank 0x4000 offset
            | _ -> failwith "Unmapped"
        )

type StaticCartROM(header,data,savefile) =
    inherit CartROM(header,data,savefile)

    override this.HighROMBank () = 1

    override this.RAMBank () = 0

[<AbstractClass>]
type MBCCartROM(header,data,savefile) =
    inherit CartROM(header,data,savefile)

    abstract Reg0: uint8 with set

    abstract Reg1: uint8 with set

    abstract Reg2: uint8 with set

    abstract Reg3: uint8 with set

    override this.ROMWrite address value =
        match address with
        | Range 0x0000us 0x1FFFus _ ->
            this.Reg0 <- value
        | Range 0x2000us 0x3FFFus _ ->
            this.Reg1 <- value
        | Range 0x4000us 0x5FFFus _ ->
            this.Reg2 <- value
        | Range 0x6000us 0x7FFFus _ ->
            this.Reg3 <- value
        | _ ->
            ()

type Mode = |Rom |Ram

type MBC1CartROM(header,data,savefile) =
    inherit MBCCartROM(header,data,savefile)

    let mutable activeROMBank = 1uy

    let mutable activeRAMBank = 0uy

    let mutable mode = Rom

    override this.HighROMBank () = if mode = Rom then int activeROMBank else activeROMBank &&& 0x1Fuy |> int

    override this.RAMBank () = if mode = Ram then int activeRAMBank else 0

    override this.Reg0 with set value = ()

    override this.Reg1
        with set value =
            // Only the low 5 bits are valid for this regsiter
            let value = value &&& 0x1Fuy
            activeROMBank <- (activeROMBank &&& 0x60uy) ||| (value)
            // Setting bank 0 really means set bank 1
            if value = 0uy then
                activeROMBank <- activeROMBank |> setBit 0

    override this.Reg2
        with set value =
            // Only the low 2 bits are valid
            let value = value &&& 0x3uy
            activeRAMBank <- value
            activeROMBank <- (activeROMBank &&& 0x1Fuy) ||| (value <<< 5)

    override this.Reg3
        with set value =
            mode <- if value = 0uy then Rom else Ram

type MBC3CartROM(header,data,savefile) =
    inherit MBCCartROM(header,data,savefile)

    let mutable activeROMBank = 1uy

    let mutable activeRAMBank = 0uy

    override this.HighROMBank () = activeROMBank |> int
    
    override this.RAMBank () = activeRAMBank |> int

    override this.Reg0
        with set value =
            match value &&& 0xFuy with
            | 0xAuy ->
                do Log.log "Enable RAM"
            | _ ->
                do Log.log "Disable RAM"
                do savefile.Save <| this.GetRAMData ()

    override this.Reg1
        with set value =
            let value = value &&& 0x7Fuy
            activeROMBank <- if value = 0uy then 1uy else value

    override this.Reg2
        with set value =
            match value with
            |ramBank when value >= 0uy && value <= 3uy ->
                activeRAMBank <- ramBank
                do Log.logf "Load RAM bank: %d" activeRAMBank
            |rtc when value >= 0x8uy && value <= 0xCuy ->
                do Log.log "Load RTC"
            |_ ->
                ()

    override this.Reg3
        with set value =
            if value = 1uy then
                do Log.log "Latch RTC"

let load rom saveFile =

    let header = CartHeader(Array.sub rom 0x100 0x50)

    match header.CartridgeType with
    | CartridgeType.ROM _ ->
        StaticCartROM(header,rom,saveFile) :> CartROM
    | CartridgeType.MBC1 _ ->
        MBC1CartROM(header,rom,saveFile) :> CartROM
    | CartridgeType.MBC3 _ ->
        MBC3CartROM(header,rom,saveFile) :> CartROM
    | _ ->
        raise <| System.Exception("Cart type unsupported")