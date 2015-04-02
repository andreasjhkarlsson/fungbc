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


type Gameboy(rom: ROM, frameReceiver: FrameReceiver) =

    let ram = GBCRam()

    let systemClock = MutableClock(GBC_SYSTEM_CLOCK_FREQUENCY,0UL)

    let interruptRegisters = InterruptRegisters()

    let timers = Timers(systemClock)

    let timerInterrupt = TimerInterrupt(timers,interruptRegisters)

    let gpu = GPU(systemClock, frameReceiver)

    let mmu = MMU(gpu, rom,ram,interruptRegisters,timers)

    let cpu = CPU(mmu,gpu,timerInterrupt,systemClock)

    member this.Start = cpu.Start 

    member this.StartWithDebugger mapInfo stepOnStart =
        let debugger = Debugger(cpu,gpu,mmu,systemClock,mapInfo)
        debugger.Attach ()
        if stepOnStart then debugger.Step ()
        debugger.Start ()
