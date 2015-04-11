module GameboyWindow

open System.Windows.Forms
open System.Drawing
open System.Drawing.Imaging
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

    member this.Capture () = lock framebuffer (fun () -> framebuffer.Clone () :?> Bitmap)

    override this.OnPaint args =
        base.OnPaint args
        lock framebuffer (fun () -> args.Graphics.DrawImage(framebuffer,0,0,width,height))

type GameboyWindow () as this =

    inherit Form()

    let statusStrip = new StatusStrip()

    let statusLabel = new ToolStripStatusLabel()

    let screen = new GameboyScreen(3)   

    let statusUpdater = new System.Timers.Timer(250.0)

    let contextMenu = new ContextMenuStrip()

    let executionMenu = new ToolStripMenuItem("Control execution...")

    let screenCapMenuItem = new ToolStripMenuItem("Save screen capture...") 

    let resetMenuItem = new ToolStripMenuItem("Reset")

    let resumeMenuItem = new ToolStripMenuItem("Resume")

    let pauseMenuItem = new ToolStripMenuItem("Pause")

    let stopMenuItem = new ToolStripMenuItem("Stop")

    let loadRomItem = new ToolStripMenuItem("Load ROM")

    let helpAndAboutMenuItem = new ToolStripMenuItem("Help && About")

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

    let runInUIContext fn = this.BeginInvoke(new System.Action(fn)) |> ignore

    do

        this.Text <- APPLICATION_TITLE

        this.FormBorderStyle <- FormBorderStyle.FixedSingle
       
        this.ClientSize <- Size(screen.Width,screen.Height + statusStrip.Height)

        // Setup context menu
        do 

            loadRomItem.Click.Add this.OpenROM
            contextMenu.Items.Add(loadRomItem) |> ignore

            resumeMenuItem.Click.Add this.Resume
            executionMenu.DropDownItems.Add(resumeMenuItem) |> ignore

            pauseMenuItem.Click.Add this.Pause
            executionMenu.DropDownItems.Add(pauseMenuItem) |> ignore

            stopMenuItem.Click.Add this.Stop
            executionMenu.DropDownItems.Add(stopMenuItem) |> ignore

            resetMenuItem.Click.Add this.Reset
            executionMenu.DropDownItems.Add(resetMenuItem) |> ignore

            contextMenu.Items.Add(executionMenu) |> ignore
        
            screenCapMenuItem.Click.Add this.ScreenCap
            contextMenu.Items.Add(screenCapMenuItem) |> ignore

            helpAndAboutMenuItem.Click.Add this.HelpAndAbout
            contextMenu.Items.Add(helpAndAboutMenuItem) |> ignore
        
            this.ContextMenuStrip <- contextMenu

            contextMenu.Opening.Add this.UpdateContextMenu 

        screen.Dock <- DockStyle.Top
        this.Controls.Add(screen)

        statusStrip.Items.Add(statusLabel :> ToolStripItem) |> ignore
        statusStrip.BackColor <- Color.Honeydew
        statusStrip.Dock <- DockStyle.Bottom
        this.Controls.Add(statusStrip)

        statusUpdater.Elapsed.Add this.UpdateStatus

    member this.Reset _ = gameboy |> Option.iter Gameboy.reset
    
    member this.ScreenCap args =
        let screenCap = screen.Capture ()
        use saveDialog = new SaveFileDialog()
        saveDialog.AddExtension <- true
        saveDialog.Title <- "Select destination"
        saveDialog.Filter <- "Image file (*.png)|*.png"
        if saveDialog.ShowDialog () = DialogResult.OK then
            screenCap.Save(saveDialog.FileName,ImageFormat.Png) 


    member this.UpdateStatus args =
        
        let text =
            match gameboy with
            | Some gameboy ->
                let fps = Gameboy.fps gameboy
                let state = Gameboy.state gameboy
                sprintf "%s | %.2f fps"
                    (match state with |Running -> "Running" |Paused -> "Paused")
                    fps   
            | None ->
                "Idle"
        runInUIContext (fun _ ->
            statusLabel.Text <- text
            statusStrip.Refresh ()
        )

    member this.OpenROM _ =
        use dialog = new OpenFileDialog()
        dialog.Title <- "Select ROM"
        dialog.Filter <- "Gameboy ROMs (*.gb)|*.gb"
        if dialog.ShowDialog () = DialogResult.OK then
            try
                this.LoadROM dialog.FileName
            with
            | error ->
                MessageBox.Show(this,
                                error.Message,
                                "Error loading ROM",
                                MessageBoxButtons.OK,
                                MessageBoxIcon.Error) |> ignore

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
        
        // Is there an gameboy already loaded? Kill!
        match gameboy with
        | Some gameboy ->
            Gameboy.kill gameboy
        | None ->
            ()

        let gb = Gameboy.create rom (FrameReceiver screen.PresentFrame)

        Gameboy.start gb

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

    member this.Resume _ =
        match gameboy with
        | Some gameboy ->
            Gameboy.start gameboy
        | None ->
            ()
    
    member this.Pause _ =
        match gameboy with
        | Some gameboy ->
            Gameboy.pause gameboy
        | None ->
            ()

    member this.Stop _ =
        match gameboy with
        | Some instance ->
            Gameboy.kill instance
            gameboy <- None
        | None ->
            ()

    member this.UpdateContextMenu _ =
        match gameboy with
        | Some gameboy ->
            executionMenu.Enabled <- true
            screenCapMenuItem.Enabled <- true
            let executionState = Gameboy.state gameboy
            match executionState with
            | Running ->
                resumeMenuItem.Enabled <- false
                pauseMenuItem.Enabled <- true
            | Paused ->
                resumeMenuItem.Enabled <- true
                pauseMenuItem.Enabled <- false
        | None ->
            executionMenu.Enabled <- false
            screenCapMenuItem.Enabled <- false

    member this.HelpAndAbout _ =
        MessageBox.Show(
            @"A massive thanks for everyone making this emulator possible. Including accatyyc & cliffords.
            Controls:
                Joypad: arrow keys
                A button: x
                B button: z
                Start: enter
                Select: space
            "
        ) |> ignore