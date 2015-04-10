
open System
open System.Windows.Forms
open Gpu
open Debugger
open Rom
open Gameboy
open GameboyWindow
open Constants

[<EntryPoint>]
[<STAThread>]
let main argv = 

    if argv.Length <> 1 then raise <| System.Exception("Usage: fgbc <fgbc-file>")

    let romPath = argv.[0]

    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault false

    use gbWindow = new GameboyWindow()
    
    gbWindow.LoadROM romPath

    Application.Run(gbWindow)

    0
