module Rom

open System.IO
open MemoryCell
open BitLogic

[<AbstractClass>]
type ROM () =
    
    let ram = readWriteMemoryBlock 8192

    abstract getCell: int -> MemoryCell
    member this.MemoryBlock = Array.init (pown 2 15) this.getCell

    member this.Ram = ram

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

        member this.Size = (pown 2 15) <<< (int bytes.[0x48]) // 32 kB << n

        member this.RAMSize = match int bytes.[0x49] with |0x1 -> 2048 |0x2 -> 8192 |0x3 -> pown 2 15 |_ -> 0

        member this.Region = match int bytes.[0x4A] with |0x0 -> Japan |0x1 -> NotJapan |_ -> Unknown

        member this.LegacyLicenseeCode = bytes.[0x4B]

        member this.VersionNumber = bytes.[0x4C]

        member this.HeaderChecksum = bytes.[0x4D]

        member this.GlobalChecksum = (bytes.[0x4E] <<< 8) ||| (bytes.[0x4F]) // Big endian


type CartROM (data) =
    inherit ROM ()

    let header = CartHeader(Array.sub data 0x100 0x50)
    
    override this.getCell index =
        match index with
        | bank0Index when index <= 0x3FFF ->
            readOnlyCell (data.[index])
        | _ -> blankCell

let loadFromCartDump path = CartROM(File.ReadAllBytes path) :> ROM