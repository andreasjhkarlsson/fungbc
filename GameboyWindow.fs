module GameboyWindow

open System.Windows.Forms
open System.Drawing
open System.Timers
open Constants
open Gameboy
open Rom
open Debugger
open Gpu

type GameboyScreen (scale) as this =
    inherit Panel()

    let width = RESOLUTION.Width * scale

    let height = RESOLUTION.Height * scale

    let framebuffer = new Bitmap(RESOLUTION.Width, RESOLUTION.Height)

    do
        this.DoubleBuffered <- true
        this.ClientSize <- Size(width,height)

    member this.PresentFrame bitmap =
        lock framebuffer (fun () -> Graphics.FromImage(framebuffer).DrawImage(bitmap,0,0,framebuffer.Width,framebuffer.Height))
        this.BeginInvoke(new System.Action(fun _ -> this.Invalidate ())) |> ignore

    override this.OnPaint args =
        base.OnPaint args
        lock framebuffer (fun () -> args.Graphics.DrawImage(framebuffer,0,0,width,height))

type GameboyWindow () as this =

    inherit Form()

    let statusStrip = new StatusStrip()

    let statusFPS = new ToolStripStatusLabel()

    let screen = new GameboyScreen(3)   

    let statusUpdater = new System.Timers.Timer(250.0)

    let mutable gameboy = None

    let keycodeToKeypad =
        function
        | Keys.Up -> Some Input.Up
        | Keys.Down -> Some Input.Down
        | Keys.Left -> Some Input.Left
        | Keys.Right -> Some Input.Right
        | Keys.X -> Some Input.A
        | Keys.Z -> Some Input.B
        | Keys.Return -> Some Input.Start
        | Keys.Space -> Some Input.Select
        | _ -> None 

    do

        this.Text <- APPLICATION_TITLE

        this.FormBorderStyle <- FormBorderStyle.FixedSingle

        this.ClientSize <- Size(screen.Width,screen.Height + statusStrip.Height)

        screen.Dock <- DockStyle.Top
        this.Controls.Add(screen)

        statusStrip.Items.Add(statusFPS :> ToolStripItem) |> ignore
        statusStrip.BackColor <- Color.Honeydew
        statusStrip.Dock <- DockStyle.Bottom
        this.Controls.Add(statusStrip)

        statusUpdater.Elapsed.Add this.UpdateStatus


    member this.UpdateStatus args =
        
        match gameboy with
        | Some gameboy ->
            let fps = Gameboy.fps gameboy
            statusFPS.Text <- sprintf "%.2f fps" fps
        | None -> ()

        statusStrip.Refresh ()

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


        let gb = Gameboy.create rom (FrameReceiver screen.PresentFrame)

        //let debugger = Debugger.attach gb mapInfo

        //Debugger.breakExecution debugger
        //Debugger.start debugger

        Gameboy.run gb

        gameboy <- Some gb        

    override this.OnFormClosing args =
        match gameboy with
        | Some gameboy ->
            Gameboy.kill gameboy
        | None -> ()
        base.OnFormClosing args

    override this.OnShown args =
        statusUpdater.Enabled <- true

        
    override this.OnKeyDown args =
        match gameboy with
        | Some gameboy ->
            match keycodeToKeypad args.KeyCode with
            | Some key ->
                (Gameboy.keypad gameboy).[key] <- Input.Pressed
            | None ->
                ()
        | None -> 
            ()

    override this.OnKeyUp args =
        match gameboy with
        | Some gameboy ->
            match keycodeToKeypad args.KeyCode with
            | Some key ->
                (Gameboy.keypad gameboy).[key] <- Input.Released
            | None ->
                ()
        | None -> 
            ()
    