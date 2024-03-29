﻿module GameboyWindow

open System.Windows.Forms
open System.Drawing
open System.Drawing.Imaging
open System.IO
open System.Timers
open System.Threading
open Constants
open Gameboy
open Rom
open Debugger
open Gpu
open Units
open AudioDevice
open Configuration
open Types
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Diagnostics

#nowarn "9" "51" // Silences native ptr warnings

module HighPrecisionSleep =
    // Super unsupported function but totally cool.
    // Allows you to sleep in 100 ns intervals.
    // Since min windows wait time is 15.6 ms this probably won't yield
    // the thread so its only use is to reduce cpu power usage, which is fair enough.
    [<DllImport(@"ntdll.dll", EntryPoint="ZwDelayExecution")>]
    extern nativeint ZwDelayExecution(unativeint unused,int64* time)

    
    [<DllImport(@"winmm.dll", EntryPoint="timeBeginPeriod")>]
    extern nativeint timeBeginPeriod(unativeint uPeriod)

    let Sleep micros =
        match OS.this with
        | OS.Windows ->
            let mutable nanos = -10L*(int64 micros)
            if nanos < 0L then do ZwDelayExecution(0un,&&nanos) |> ignore
        | _ ->
            // TODO: Call usleep or something
            Thread.Sleep(0)

type FPSTracker (window) =
    let stopwatch = new Stopwatch ()

    do stopwatch.Start ()
    
    let mutable frameCount = 0u

    let mutable fps = 0.0

    member this.Frame () =
        frameCount <- frameCount + 1u

        if frameCount % 5u = 0u then
            fps <- 1000.0 / ((float stopwatch.ElapsedMilliseconds) / (float window))
            stopwatch.Restart ()

    member this.FPS = fps

type GameboyScreen () as this =

    inherit Panel ()

    let fpsTracker = FPSTracker(5)

    let mutable front = new Bitmap(RESOLUTION.Width, RESOLUTION.Height)

    let mutable back = front.Clone () :?> Bitmap

    let lockBuffer (bitmap: Bitmap) =
        bitmap.LockBits(Rectangle(0,0,bitmap.Width,front.Height),ImageLockMode.ReadWrite,bitmap.PixelFormat)    

    let mutable lockData = lockBuffer back

    let flip () =
        
        lock this (fun () ->

            do back.UnlockBits lockData

            let tmp = front
            front <- back
            back <- tmp
            
            lockData <- lockBuffer back

            do fpsTracker.Frame ()
        )


    do
        this.DoubleBuffered <- true
 
    member this.Capture () = lock this (fun () -> front.Clone () :?> Bitmap)

    member this.FPS = fpsTracker.FPS

    override this.OnPaint args =
        base.OnPaint args
        lock this (fun () -> args.Graphics.DrawImage(front,0,0,this.Width,this.Height))

    
    interface Configuration.Renderer with
        
        member this.SetPixel x y color =
            // No need to lock access as lockData is only changed from
            // flip which is only called from Flush (same threads that calls this)
            let bufferPtr = NativePtr.ofNativeInt lockData.Scan0

            let pixelPtr = NativePtr.add bufferPtr ((y * lockData.Stride + (x * 4)) / 4)

            do NativePtr.write pixelPtr color
            

        member this.GetPixel x y = 
            let bufferPtr = NativePtr.ofNativeInt lockData.Scan0

            let pixelPtr = NativePtr.add bufferPtr ((y * lockData.Stride + (x * 4)) / 4)

            NativePtr.read pixelPtr

        member this.Flush () = do flip ()


type DataToolStripMenuItem<'a>(name: string,data: 'a) =
    inherit ToolStripMenuItem(name)

    member this.Data = data

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
        |> List.map (fun scale ->
            new DataToolStripMenuItem<int>(sprintf "%dx" scale,scale)
        )

    let paletteMenu = new ToolStripMenuItem("Palette...")

    let paletteItems =
        [("Standard Gray", Palette.Predefined.grayscale)
         ("Terminal Green", Palette.Predefined.terminalGreen)
         ("Fabolous Pink", Palette.Predefined.fabolousPink)
         ("Dreamy Blue", Palette.Predefined.dreamyBlue)
         ("Horror Red", Palette.Predefined.horrorRed)
         ("Summer Green", Palette.Predefined.summerGreen)
         ("Psychedelic", Palette.Predefined.psychedelic)]
        |> List.map (fun (name, palette) ->
            new DataToolStripMenuItem<Palette.Palette>(name,palette)
        )

    let speedMenu = new ToolStripMenuItem("Speed...")

    let speedItems =
        [
            "50 %", Speed 0.5
            "75 %", Speed 0.75
            "100 %", Speed 1.0
            "125 %", Speed 1.25
            "150 %", Speed 1.5
            "200 %", Speed 2.0
            "300 %", Speed 3.0
        ] |> List.map (fun (label,speed) ->
            new DataToolStripMenuItem<Speed>(label,speed)
        )

    let soundMenu = new ToolStripMenuItem("Sound...")

    let enableSoundMenuItem = new ToolStripMenuItem("Enable")

    let keepPitchMenuItem = new ToolStripMenuItem("Keep pitch")

    let screenCapMenuItem = new ToolStripMenuItem("Save screen capture...") 

    let resetMenuItem = new ToolStripMenuItem("Reset")

    let resumeMenuItem = new ToolStripMenuItem("Resume")

    let pauseMenuItem = new ToolStripMenuItem("Pause")

    let stopMenuItem = new ToolStripMenuItem("Stop")

    let loadRomItem = new ToolStripMenuItem("Load ROM")

    let helpAndAboutMenuItem = new ToolStripMenuItem("About")

    let audioDevice = AudioDevice ()

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

        HighPrecisionSleep.timeBeginPeriod (unativeint 1) |> ignore

        this.Text <- Resource.title

        this.Icon <- Resource.icon

        this.FormBorderStyle <- FormBorderStyle.FixedSingle

        this.MaximizeBox <- false
       
        this.Scale <- 2

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

            // Setup sound menu
            do
                soundMenu.DropDownItems.Add enableSoundMenuItem |> ignore
                enableSoundMenuItem.Click.Add (fun _ -> this.ToggleAudio ())

                soundMenu.DropDownItems.Add keepPitchMenuItem |> ignore
                keepPitchMenuItem.Click.Add (fun _ -> this.ToggleKeepPitch ())

                contextMenu.Items.Add soundMenu |> ignore

            // Setup speed menu
            do
                speedItems
                |> List.iter (fun item ->
                    speedMenu.DropDownItems.Add item |> ignore
                    item.Click.Add (fun _ -> this.Speed <- item.Data)
                )

                contextMenu.Items.Add speedMenu |> ignore

            // Setup scale menu
            do
                scaleItems
                |> List.iter (fun item ->
                    scaleMenu.DropDownItems.Add item |> ignore
                    item.Click.Add (fun _ -> this.Scale <- item.Data)
                )
                contextMenu.Items.Add scaleMenu |> ignore

            // Setup palette menu
            do
                paletteItems
                |> List.iter (fun item ->
                    paletteMenu.DropDownItems.Add item |> ignore
                    item.Click.Add (fun _ -> this.Palette <- item.Data)
                )
                contextMenu.Items.Add paletteMenu |> ignore
        
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

        audioDevice.Init ()

    member this.Reset _ = gameboy |> Option.iter Gameboy.reset

    member this.Scale
        with set scale =
            this.ClientSize <- Size(RESOLUTION.Width * scale,RESOLUTION.Height * scale + statusStrip.Height)
            scaleItems |> List.iter (fun item -> item.Checked <- item.Data = scale)

    member this.Palette
        with set palette =
            match gameboy with
            | Some gameboy ->
                gameboy |> Gameboy.setConfig {
                    Gameboy.getConfig gameboy with
                        Palette = palette
                }
            | None ->
                ()          
                
    member this.Speed
        with set speed =
            match gameboy with
            | Some gameboy ->
                gameboy |> Gameboy.setConfig {
                    Gameboy.getConfig gameboy with
                        Speed = speed
                }  
            | None ->
                ()
    
    member this.ToggleAudio () =
        match gameboy with
        | Some gameboy ->
            let config = gameboy |> Gameboy.getConfig
            gameboy |> Gameboy.setConfig {
                config with
                    EnableAudio = not config.EnableAudio
            }  
        | None ->
            ()

    
    member this.ToggleKeepPitch () =
        match gameboy with
        | Some gameboy ->
            let config = gameboy |> Gameboy.getConfig
            gameboy |> Gameboy.setConfig {
                config with
                    KeepAudioPitch = not config.KeepAudioPitch
            }  
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
                let state = Gameboy.state gameboy
                sprintf "%s | %.2f fps"
                    (match state with |Running -> "Running" |Paused -> "Paused")
                    screen.FPS   
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

        let gb = Gameboy.create rom {
            Renderer = screen :> Configuration.Renderer
            AudioDevice = audioDevice
            ErrorFn = fun e -> runInUIContext (fun () -> this.ShowError e)
            Idle = this.Idle
            Speed = Speed 1.0
            Palette = Palette.Predefined.grayscale
            KeepAudioPitch = true
            EnableAudio = true
        }


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
            speedMenu.Enabled <- true
            soundMenu.Enabled <- true
            let executionState = Gameboy.state gameboy
            match executionState with
            | Running ->
                resumeMenuItem.Enabled <- false
                pauseMenuItem.Enabled <- true
            | Paused ->
                resumeMenuItem.Enabled <- true
                pauseMenuItem.Enabled <- false

            let config = gameboy |> Gameboy.getConfig

            paletteItems |> List.iter (fun item -> item.Checked <- item.Data = config.Palette)

            speedItems |> List.iter (fun item -> item.Checked <- item.Data = config.Speed)

            enableSoundMenuItem.Checked <- config.EnableAudio

            keepPitchMenuItem.Checked <- config.KeepAudioPitch

        | None ->
            executionMenu.Enabled <- false
            screenCapMenuItem.Enabled <- false
            paletteMenu.Enabled <- false
            speedMenu.Enabled <- false
            soundMenu.Enabled <- false

    member this.HelpAndAbout _ = MessageBox.Show(Resource.about,sprintf "%s %s" Resource.title Resource.version) |> ignore

    member this.Idle micros =
        // Simply spinlock if time is < 500 microseconds to get close to perfect fps
        if micros > 500 then do HighPrecisionSleep.Sleep (micros/2)

    member this.ShowError (ex: exn) =
        MessageBox.Show(ex.Message,
                        sprintf "%s encountered an unexpected error" Resource.title, 
                        MessageBoxButtons.OK,
                        MessageBoxIcon.Error) |> ignore