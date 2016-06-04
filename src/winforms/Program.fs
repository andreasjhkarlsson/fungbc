
open System
open System.Windows.Forms
open Gpu
open Debugger
open Rom
open Gameboy
open GameboyWindow
open Constants
open Log
open NAudio
open NAudio.Wave

[<EntryPoint>]
[<STAThread>]
let main argv = 

    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault false

    do printfn "%s" |> Log.subscribe

    use gbWindow = new GameboyWindow()
    if argv.Length > 0 then
        let romPath = argv.[0] 
        gbWindow.Shown.Add(fun _ -> gbWindow.LoadROM romPath)

    Application.Run(gbWindow)

    0
