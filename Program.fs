
open Cpu
open Mmu
open Rom

[<EntryPoint>]
let main argv = 
    
    let mmu = MMU()

    let cpu = CPU(mmu)

    if argv.Length <> 1 then raise (System.Exception("Usage: fgbc <fgbc-file>"))

    let rom = LoadROMFromFGBC argv.[0]

    mmu.LoadBlob 0us rom.Code

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    cpu.Start ()

    stopWatch.Stop()

    printfn "CPU execution time: %d ms" (int stopWatch.Elapsed.TotalMilliseconds) 

    cpu.Registers.Print ()

    mmu.PrintDump 0x0 0xFF


    

    0
