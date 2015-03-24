
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

    let rom =
        let path = argv.[0]
        let ext = (System.IO.Path.GetExtension path).ToLower ()
        match ext with
        | ".fgbc" -> Rom.loadFromFGBC path
        | ".gb" -> Rom.loadFromCartDump path
        | _ -> raise <| System.Exception(sprintf "Unsupported file extension: %s" ext)

    let ram = GBCRam()

    let systemClock = MutableClock(GBC_SYSTEM_CLOCK_FREQUENCY,0UL)

    let interruptRegisters = InterruptRegisters()

    let timers = Timers(systemClock)

    let timerInterrupt = TimerInterrupt(timers,interruptRegisters)

    let gpu = GPU(systemClock)

    let mmu = MMU(gpu, rom,ram,interruptRegisters,timers)

    let cpu = CPU(mmu,timerInterrupt,systemClock)

    let debugger = Debugger(cpu,mmu, systemClock)

    debugger.Attach ()

    debugger.Start ()

    debugger.PrintSummary ()

    0
