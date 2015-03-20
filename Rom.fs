module Rom

open System.IO
open MemoryCell


type ROM (code) =
    let l = Array.length code
    member val Code = Array.init (pown 2 15) (fun i ->
        readOnlyCell(if i < l then code.[i] else 0uy)
    )

let LoadROMFromFGBC path =
    let lineToCode (line: string) = 
        
        let decodeByte (str: string) =
            System.Byte.Parse(str,System.Globalization.NumberStyles.HexNumber)

        line.Split [|' '; '\t'|] |>
            Array.toSeq |>
            Seq.map (fun x -> x.Trim()) |>
            Seq.takeWhile (fun x ->
                (not <| x.Contains "//") && x <> "" ) |>
            Seq.map decodeByte

    let code = (File.ReadAllLines path) |> Array.map lineToCode |> Seq.concat |> Seq.toArray

    ROM(code)