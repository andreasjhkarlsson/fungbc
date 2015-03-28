
open Cpu
open Gpu
open Mmu
open Rom
open Ram
open Interrupts
open Clock
open Constants
open Timer
open Debugger

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

    let ram = GBCRam()

    let systemClock = MutableClock(GBC_SYSTEM_CLOCK_FREQUENCY,0UL)

    let interruptRegisters = InterruptRegisters()

    let timers = Timers(systemClock)

    let timerInterrupt = TimerInterrupt(timers,interruptRegisters)

    let gpu = GPU(systemClock)

    let mmu = MMU(gpu, rom,ram,interruptRegisters,timers)

    let cpu = CPU(mmu,gpu,timerInterrupt,systemClock)

    let debugger = Debugger(cpu,mmu, systemClock, mapInfo)

    debugger.Attach ()

    System.Console.CancelKeyPress.AddHandler
        <| new System.ConsoleCancelEventHandler(fun obj args ->
            if not <| debugger.IsStepping () then
                printfn "\n--- Ctrl+C, invoking debugger ---\n"
                args.Cancel <- true
                debugger.Step ()
            ) 

    debugger.Start ()

    0
