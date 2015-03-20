
open Cpu
open Mmu
open Rom
open Ram
open Interrupts
open IORegisters
open Clock

[<EntryPoint>]
let main argv = 
    
    if argv.Length <> 1 then raise (System.Exception("Usage: fgbc <fgbc-file>"))

    let rom = LoadROMFromFGBC argv.[0]

    let ram = GBCRam()

    let clock = Clock()

    let ioRegisters = IORegisters(clock)

    let mmu = MMU()

    mmu.MapRAM ram
    mmu.MapROM rom
    mmu.MapIORegisters ioRegisters

    let cpu = CPU(mmu, clock)

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    cpu.Start ()

    stopWatch.Stop()

    printfn "CPU execution time: %d ms" (int stopWatch.Elapsed.TotalMilliseconds) 

    clock.Print ()

    cpu.Registers.Print ()

    mmu.PrintDump 0x0 0xFF

    mmu.PrintDump 0xFF00 0xFFFF

    0
