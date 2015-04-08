module Gameboy

open System.Drawing
open Rom
open Ram
open Interrupts
open Timer
open Gpu
open Mmu
open Cpu
open Debugger
open Clock
open Constants
open Input


type Gameboy(rom: ROM, frameReceiver: FrameReceiver) =

    let ram = GBCRam()

    let systemClock = MutableClock(GBC_SYSTEM_CLOCK_FREQUENCY,0UL)

    let interrupts = InterruptManager()

    let keypad = Keypad()

    let timers = Timers(systemClock,interrupts)

    let gpu = GPU(systemClock, interrupts, frameReceiver)

    let mmu = MMU(gpu, rom,ram,keypad,interrupts,timers)

    let cpu = CPU(mmu,gpu,interrupts,timers,systemClock)

    member this.Start = cpu.Start 

    member this.StartWithDebugger mapInfo stepOnStart =
        let debugger = Debugger(cpu,gpu,mmu,interrupts,systemClock,mapInfo)
        debugger.Attach ()
        if stepOnStart then debugger.Step ()
        debugger.Start ()
