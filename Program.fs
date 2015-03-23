
open Cpu
open Gpu
open Mmu
open Rom
open Ram
open Interrupts
open IORegisters
open Clock
open Constants

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

    let ioRegisters = IORegisters(systemClock)

    let gpu = GPU()

    let mmu = MMU(gpu, rom,ram,ioRegisters)

    let cpu = CPU(mmu,ioRegisters,systemClock)

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    cpu.Start ()

    stopWatch.Stop()

    printfn "CPU execution time: %d ms" (int stopWatch.Elapsed.TotalMilliseconds) 

    systemClock.Print ()

    cpu.Registers.Print ()

    mmu.PrintDump 0x0 0xFF

    mmu.PrintDump 0xFF00 0xFFFF



    0
