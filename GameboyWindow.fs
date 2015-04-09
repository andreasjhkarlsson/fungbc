module GameboyWindow

open System.Windows.Forms
open System.Drawing
open Constants
open Gameboy
open Rom
open Debugger
open Gpu

type GameboyWindow () as this =

    inherit Form()

    let scale = 3

    let width = RESOLUTION.Width

    let height = RESOLUTION.Height

    let framebuffer = new Bitmap(width, height)

    let mutable gameboy = None

    do

        this.ClientSize <- Size(width*scale, height*scale)

        this.Text <- APPLICATION_TITLE

        this.DoubleBuffered <- true

        this.FormBorderStyle <- FormBorderStyle.FixedSingle

    member this.LoadROM path =
        let rom =
        
            let ext = (System.IO.Path.GetExtension path).ToLower ()
            match ext with
            | ".gb" ->  Rom.loadFromCartDump path
            | _ -> raise <| System.Exception(sprintf "Unsupported file extension: %s" ext)

        let mapInfo = 
            let mapPath = System.IO.Path.GetDirectoryName path
                            + @"\"
                            + System.IO.Path.GetFileNameWithoutExtension path
                            + ".map" 

            match System.IO.File.Exists mapPath with
            | true -> MapInfo(mapPath)
            | false -> MapInfo()   


        let gb = Gameboy.create rom (FrameReceiver this.PresentFrame)

        let debugger = Debugger.attach gb mapInfo

        Debugger.breakExecution debugger
        Debugger.start debugger

        //Gameboy.run gb

        gameboy <- Some gb

   

    member this.PresentFrame bitmap =
        lock framebuffer (fun () -> Graphics.FromImage(framebuffer).DrawImage(bitmap,0,0,width,height))
        this.BeginInvoke(new System.Action(fun _ -> this.Invalidate ())) |> ignore

    override this.OnPaint args =
        base.OnPaint args
        lock framebuffer (fun () -> args.Graphics.DrawImage(framebuffer,0,0,width*scale,height*scale))

    override this.OnFormClosing args =
        match gameboy with
        | Some gameboy ->
            Gameboy.kill gameboy
        | None -> ()
        base.OnFormClosing args

        


    