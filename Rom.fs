module Rom

open System.IO

type ROM (code) =
    member this.Code = code

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