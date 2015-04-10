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

type State = |Running |Paused 

type Message = 
    |Kill of AsyncReplyChannel<unit>
    |Run
    |Step of AsyncReplyChannel<unit>
    |Input of Input.Key*Input.KeyState
    |FPS of AsyncReplyChannel<float>
    |Pause of AsyncReplyChannel<unit>
    |Start
    |State of AsyncReplyChannel<State>
    |Reset


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
                                    
            let rec handleMessage state = async {

                let! message = mailbox.Receive ()

                match message with
                | Run when state = Running ->
                    // Since agent posting / receiving incurs an overhead
                    // we let the emulator execute several times before processing next message.
                    try
                        runEmulation 20
                        mailbox.Post Run
                    with
                    | error ->
                        printfn "Runtime errror!\n%s" error.StackTrace
                        mailbox.PostAndReply Kill
                | Reset ->
                    cpu.Reset ()
                | Step reply ->
                    runEmulation 1
                    reply.Reply () 
                | Input (key,state)->
                    keypad.[key] <- state
                | FPS (reply) ->
                    reply.Reply gpu.FPS
                | State reply ->
                    reply.Reply state
                | _ ->
                    ()

                match message with
                | Kill reply ->
                    // Stop processing messages and signal back that we have ack'ed the kill
                    reply.Reply ()
                | Pause reply ->
                    reply.Reply ()
                    return! handleMessage Paused
                | Start ->
                    mailbox.Post Run
                    return! handleMessage Running
                | _ ->
                    return! handleMessage state 

            }

            handleMessage Paused

        )  

    Gameboy (agent, components)

let start (Gameboy (agent,_)) = agent.Post Start

let pause (Gameboy (agent,_)) = agent.PostAndReply Pause

let reset (Gameboy (agent,_)) = agent.Post Reset

let state (Gameboy (agent,_)) = agent.PostAndReply State

let step (Gameboy (agent,_)) = agent.PostAndReply Step

let kill (Gameboy (agent,_)) = agent.PostAndReply Kill

let postInput (Gameboy (agent,_)) key state = Input(key,state) |> agent.Post

let components (Gameboy (_,components)) = components

let keypad gameboy = (components gameboy).Keypad

let fps (Gameboy (agent, _)) = agent.PostAndReply FPS