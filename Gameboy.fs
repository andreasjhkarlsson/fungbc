module Gameboy

open System.Drawing
open Rom
open Ram
open Interrupts
open Timer
open Gpu
open Mmu
open Cpu
open Clock
open Constants
open Input

type Message = |Kill of AsyncReplyChannel<unit> |Run |Step of AsyncReplyChannel<unit> |Input of Input.Key*Input.KeyState

type GameboyAgent = MailboxProcessor<Message>

type GameboyComponents = {
        Ram: GBCRam
        Clock: Clock
        Interrupts: InterruptManager
        Keypad: Keypad
        Timers: Timers
        Gpu: GPU
        Mmu: MMU
        Cpu: CPU
    }


type Gameboy = |Gameboy of GameboyAgent*GameboyComponents

let create (rom: ROM) (frameReceiver: FrameReceiver) =
    let ram = GBCRam()

    let systemClock = MutableClock(GBC_SYSTEM_CLOCK_FREQUENCY,0UL)

    let interrupts = InterruptManager()

    let keypad = Keypad(interrupts)

    let timers = Timers(systemClock,interrupts)

    let gpu = GPU(systemClock, interrupts, frameReceiver)

    let mmu = MMU(gpu, rom,ram,keypad,interrupts,timers)

    let cpu = CPU(mmu,interrupts,systemClock)

    cpu.Reset ()
    
    let components = {Ram = ram; Clock = systemClock; Interrupts = interrupts;
                      Keypad = keypad; Timers = timers; Gpu = gpu; Mmu = mmu; Cpu = cpu}

    let agent = GameboyAgent.Start(fun mailbox ->
            // Run the emulator for a number of iterations
            let rec runEmulation iters =
                if iters > 0 then
                    // Execute a single instruction
                    cpu.Execute ()
                    // Updates gpu state (may draw and raise interrupt)
                    gpu.Update ()
                    // Increment timers (may raise interrupt)
                    timers.Update ()
                    runEmulation (iters - 1)
                else
                    ()
                                    
            let rec handleMessage () = async {
                
                let! message = mailbox.Receive ()

                match message with
                | Run ->
                    // Since agent posting / receiving incurs an overhead
                    // we let the emulator execute several times before processing next message.
                    runEmulation 10
                    mailbox.Post Run
                | Step reply ->
                    runEmulation 1
                    reply.Reply ()
                | Input (key,state) ->
                    keypad.[key] <- state
                | _ ->
                    ()

                match message with
                | Kill reply ->
                    // Stop processing messages and signal back that we have ack'ed the kill
                    reply.Reply ()
                | _ ->
                    return! handleMessage () 

            }

            handleMessage ()

        )  

    Gameboy (agent, components)

let run (Gameboy (agent,_)) = agent.Post Run

let step (Gameboy (agent,_)) = agent.PostAndReply (fun r -> Step r)

let kill (Gameboy (agent,_)) = agent.PostAndReply (fun r -> Kill r)

let postInput (Gameboy (agent,_)) key state = Input(key,state) |> agent.Post

let components (Gameboy (_,components)) = components