
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

    try

    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault false

    do printfn "%s" |> Log.subscribe

    use gbWindow = new GameboyWindow()
    if argv.Length > 0 then
        let romPath = argv.[0] 
        gbWindow.Shown.Add(fun _ -> gbWindow.LoadROM romPath)

    Application.Run(gbWindow)

    with ex ->
        let answer = MessageBox.Show(String.Format("{0}{1}{1}Would you like to restart?", ex.Message, Environment.NewLine), String.Format("{0} - Error", Application.ProductName), MessageBoxButtons.YesNo, MessageBoxIcon.Error)
        match answer with
            | DialogResult.Yes -> Application.Restart()
            | _ -> ()

    0