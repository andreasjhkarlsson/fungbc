
open System.Windows.Forms
open Gpu
open Debugger
open Rom
open Gameboy
open GameboyWindow

[<EntryPoint>]
let main argv = 

    if argv.Length <> 1 then raise <| System.Exception("Usage: fgbc <fgbc-file>")

    let romPath = argv.[0]

    let rom =
        
        let ext = (System.IO.Path.GetExtension romPath).ToLower ()
        match ext with
        | ".gb" ->  Rom.loadFromCartDump romPath
        | _ -> raise <| System.Exception(sprintf "Unsupported file extension: %s" ext)

    let mapInfo = 
        let mapPath = System.IO.Path.GetDirectoryName romPath
                        + @"\"
                        + System.IO.Path.GetFileNameWithoutExtension romPath
                        + ".map" 

        match System.IO.File.Exists mapPath with
        | true -> MapInfo(mapPath)
        | false -> MapInfo()

    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault false

    use gbWindow = new GameboyWindow()
    
    async {
        let gb = Gameboy(rom, FrameReceiver gbWindow.PresentFrame)
        
        gb.StartWithDebugger mapInfo false
        //gb.Start()

    } |> Async.Start

    Application.Run(gbWindow)

    0
