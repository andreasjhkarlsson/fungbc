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

type Message = |Kill of AsyncReplyChannel<unit> |Run |Step |Input of Input.Key*Input.KeyState

type GameboyAgent = MailboxProcessor<Message>

let create (rom: ROM) (frameReceiver: FrameReceiver) =
    let ram = GBCRam()

    let systemClock = MutableClock(GBC_SYSTEM_CLOCK_FREQUENCY,0UL)

    let interrupts = InterruptManager()

    let keypad = Keypad(interrupts)

    let timers = Timers(systemClock,interrupts)

    let gpu = GPU(systemClock, interrupts, frameReceiver)

    let mmu = MMU(gpu, rom,ram,keypad,interrupts,timers)

    let cpu = CPU(mmu,interrupts,systemClock)
    
    
    GameboyAgent.Start(fun mailbox ->
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
                | Step ->
                    runEmulation 1
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

let run (agent: GameboyAgent) = agent.Post Run

let step (agent: GameboyAgent) = agent.Post Step

let kill (agent: GameboyAgent) = agent.PostAndReply (fun r -> Kill r)

let postInput (agent: GameboyAgent) key state = Input(key,state) |> agent.Post