
open Cpu
open Mmu
open Rom
open Ram
open Interrupts
open IORegisters

[<EntryPoint>]
let main argv = 
    
    if argv.Length <> 1 then raise (System.Exception("Usage: fgbc <fgbc-file>"))

    let rom = LoadROMFromFGBC argv.[0]

    let ram = GBCRam()

    let ioRegisters = IORegisters()

    let mmu = MMU()

    mmu.MapRAM ram
    mmu.MapROM rom
    mmu.MapIORegisters ioRegisters

    let cpu = CPU(mmu)

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    cpu.Start ()

    stopWatch.Stop()

    printfn "CPU execution time: %d ms" (int stopWatch.Elapsed.TotalMilliseconds) 

    cpu.Registers.Print ()

    mmu.PrintDump 0x0 0xFF

    0
