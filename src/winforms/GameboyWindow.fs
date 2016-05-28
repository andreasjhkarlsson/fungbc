module GameboyWindow

open System.Windows.Forms
open System.Drawing
open System.Drawing.Imaging
open System.IO
open System.Timers
open Constants
open Gameboy
open Rom
open Debugger
open Gpu
open Resource
open Units
open System.Runtime.InteropServices

type GameboyScreen () as this =

    inherit Panel ()

    let screen = new Bitmap(RESOLUTION.Width, RESOLUTION.Height)

    let lockScreen () =
        screen.LockBits(Rectangle(0,0,screen.Width,screen.Height),ImageLockMode.ReadWrite,screen.PixelFormat)

    let unlockScreen lockData =
        screen.UnlockBits lockData

    let pixelBuffer, pixelBufferIndex =
        let lockData = lockScreen ()
        let stride = abs lockData.Stride
        let memorySize = (stride * screen.Height) / 4
        unlockScreen lockData
        Array.create memorySize 0, fun x y -> (y * stride + (x * 4)) / 4

    do
        this.DoubleBuffered <- true
 
    member this.Capture () = lock this (fun () -> screen.Clone () :?> Bitmap)

    override this.OnPaint args =
        base.OnPaint args
        lock this (fun () -> args.Graphics.DrawImage(screen,0,0,this.Width,this.Height))

    interface Gpu.Renderer with
        
        member this.SetPixel x y color = Array.set pixelBuffer (pixelBufferIndex x y) color

        member this.GetPixel x y = Array.get pixelBuffer (pixelBufferIndex x y)

        member this.Flush () = 
            lock this (fun () ->
                let lockData = lockScreen ()
                Marshal.Copy(pixelBuffer,0,lockData.Scan0,pixelBuffer.Length)
                unlockScreen lockData
            )

type ScaleToolStripMenuItem(scale) =
    inherit ToolStripMenuItem(sprintf "%dx" scale)

    member this.Scale = scale

type PaletteToolStripMenuItem(name: string,palette) =
    inherit ToolStripMenuItem(name)

    member this.Palette = palette

type GameboyWindow () as this =

    inherit Form()

    let statusStrip = new StatusStrip()

    let statusLabel = new ToolStripStatusLabel()

    let screen = new GameboyScreen()   

    let statusUpdater = new System.Timers.Timer(250.0)

    let redrawTimer = new System.Windows.Forms.Timer ()

    let contextMenu = new ContextMenuStrip()

    let executionMenu = new ToolStripMenuItem("Control execution...")

    let scaleMenu = new ToolStripMenuItem("Scale...")

    let scaleItems =
        [1; 2; 3; 4; 6; 8]
        |> List.map (fun scale -> new ScaleToolStripMenuItem(scale))

    let paletteMenu = new ToolStripMenuItem("Palette...")

    let paletteItems =
        [("Standard Gray", Palette.Predefined.grayscale)
         ("Terminal Green", Palette.Predefined.terminalGreen)
         ("Fabolous Pink", Palette.Predefined.fabolousPink)
         ("Dreamy Blue", Palette.Predefined.dreamyBlue)
         ("Horror Red", Palette.Predefined.horrorRed)
         ("Summer Green", Palette.Predefined.summerGreen)]
        |> List.map (fun (name, palette) -> new PaletteToolStripMenuItem(name,palette))

    let screenCapMenuItem = new ToolStripMenuItem("Save screen capture...") 

    let resetMenuItem = new ToolStripMenuItem("Reset")

    let resumeMenuItem = new ToolStripMenuItem("Resume")

    let pauseMenuItem = new ToolStripMenuItem("Pause")

    let stopMenuItem = new ToolStripMenuItem("Stop")

    let loadRomItem = new ToolStripMenuItem("Load ROM")

    let limitFPSItem = new ToolStripMenuItem("Limit FPS")

    let helpAndAboutMenuItem = new ToolStripMenuItem("About")

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

        this.Text <- Resource.title

        this.Icon <- Resource.icon

        this.FormBorderStyle <- FormBorderStyle.FixedSingle

        this.MaximizeBox <- false
       
        this.Scale 2

        // Setup context menu
        do 

            loadRomItem.Click.Add this.OpenROM
            contextMenu.Items.Add(loadRomItem) |> ignore

            // Setup control execution menu
            do
                resumeMenuItem.Click.Add this.Resume
                executionMenu.DropDownItems.Add(resumeMenuItem) |> ignore

                pauseMenuItem.Click.Add this.Pause
                executionMenu.DropDownItems.Add(pauseMenuItem) |> ignore

                stopMenuItem.Click.Add this.Stop
                executionMenu.DropDownItems.Add(stopMenuItem) |> ignore

                resetMenuItem.Click.Add this.Reset
                executionMenu.DropDownItems.Add(resetMenuItem) |> ignore

                contextMenu.Items.Add(executionMenu) |> ignore

            // Setup scale menu
            do
                scaleItems
                |> List.iter (fun item ->
                    scaleMenu.DropDownItems.Add item |> ignore
                    item.Click.Add (fun _ -> this.Scale item.Scale)
                )
                contextMenu.Items.Add scaleMenu |> ignore

            // Setup palette menu
            do
                paletteItems
                |> List.iter (fun item ->
                    paletteMenu.DropDownItems.Add item |> ignore
                    item.Click.Add (fun _ -> this.Palette item.Palette)
                )
                contextMenu.Items.Add paletteMenu |> ignore

            limitFPSItem.Checked <- true
            limitFPSItem.Click.Add this.ToggleFPSLimit 
            contextMenu.Items.Add(limitFPSItem) |> ignore
        
            screenCapMenuItem.Click.Add this.ScreenCap
            contextMenu.Items.Add(screenCapMenuItem) |> ignore

            helpAndAboutMenuItem.Click.Add this.HelpAndAbout
            contextMenu.Items.Add(helpAndAboutMenuItem) |> ignore
        
            screen.ContextMenuStrip <- contextMenu

            contextMenu.Opening.Add this.UpdateContextMenu 

        screen.Dock <- DockStyle.Fill
   
        this.Controls.Add(screen)

        statusStrip.Items.Add(statusLabel :> ToolStripItem) |> ignore
        statusStrip.BackColor <- Color.Honeydew
        statusStrip.Dock <- DockStyle.Bottom
        this.Controls.Add(statusStrip)

        statusUpdater.Elapsed.Add this.UpdateStatus

        redrawTimer.Interval <- int <| 1000.0 / 60.0
        redrawTimer.Tick.Add (fun _ -> this.Refresh ())

        redrawTimer.Enabled <- true

    member this.Reset _ = gameboy |> Option.iter Gameboy.reset

    member this.ToggleFPSLimit args =
        match gameboy with
        | Some gameboy ->
            let currentSpeed = Gameboy.speed gameboy
            match currentSpeed with
            | Unlimited ->
                gameboy |> Gameboy.setSpeed (Limit 60<Hz>)
            | Limit _ ->
                gameboy |> Gameboy.setSpeed Unlimited 
        |_ ->
            ()

    member this.Scale scale =
        this.ClientSize <- Size(RESOLUTION.Width * scale,RESOLUTION.Height * scale + statusStrip.Height)
        scaleItems |> List.iter (fun item -> item.Checked <- item.Scale = scale)

    member this.Palette palette =
        match gameboy with
        | Some gameboy ->
            gameboy |> Gameboy.setPalette palette
        | None ->
            ()
    
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
            | ".gb" ->
                let romData = path |> File.ReadAllBytes
                let saveFile = path + ".sav"
                Rom.load romData {
                    new SaveFile with
                        member x.Load () = try Some <| File.ReadAllBytes saveFile with _ -> None
                        member x.Save data = do File.WriteAllBytes(saveFile,data)
                    }
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

        let gb = Gameboy.create rom screen

        Gameboy.start gb
        
        Debugger.attachOnCtrlC gb

        gameboy <- Some gb        

    override this.OnFormClosing args =
        match gameboy with
        | Some instance ->
            gameboy <- None
            Gameboy.kill instance
        | None -> ()
        base.OnFormClosing args


    override this.OnShown args =
        statusUpdater.Enabled <- true
        redrawTimer.Start ()
        base.OnShown args
        
    override this.OnKeyDown args =

        match gameboy with
        | Some gameboy ->
            match keycodeToKeypad args.KeyCode with
            | Some key ->
                gameboy |> Gameboy.postInput key Input.Pressed
            | None ->
                base.OnKeyDown args
        | None -> 
            base.OnKeyDown args

    override this.OnKeyUp args =
        match gameboy with
        | Some gameboy ->
            match keycodeToKeypad args.KeyCode with
            | Some key ->
                gameboy |> Gameboy.postInput key Input.Released
            | None ->
                base.OnKeyUp args
        | None -> 
            base.OnKeyUp args

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
            paletteMenu.Enabled <- true
            limitFPSItem.Enabled <- true
            limitFPSItem.Checked <- not ((Gameboy.speed gameboy) = Unlimited)
            let executionState = Gameboy.state gameboy
            match executionState with
            | Running ->
                resumeMenuItem.Enabled <- false
                pauseMenuItem.Enabled <- true
            | Paused ->
                resumeMenuItem.Enabled <- true
                pauseMenuItem.Enabled <- false

            let palette = gameboy |> Gameboy.palette

            paletteItems |> List.iter (fun item -> item.Checked <- item.Palette = palette)

        | None ->
            executionMenu.Enabled <- false
            screenCapMenuItem.Enabled <- false
            paletteMenu.Enabled <- false
            limitFPSItem.Enabled <- false

    member this.HelpAndAbout _ = MessageBox.Show(Resource.about) |> ignore