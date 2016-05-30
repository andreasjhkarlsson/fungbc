module Gameboy

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
open Palette
open Host

type State = |Running |Paused 

type Reply<'a> = AsyncReplyChannel<'a>

type PropertyValue =
    |Speed of Speed
    |Palette of Palette

type PropertyReply =
    |Speed of Reply<Speed>
    |Palette of Reply<Palette>
    |FPS of Reply<float>

type Message = 
    |Kill
    |Run
    |Step of Reply<System.Exception option>
    |Input of Input.Key*Input.KeyState
    |Pause of Reply<unit>
    |Start
    |State of Reply<State>
    |Reset
    |Write of PropertyValue
    |Read of PropertyReply

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
        Host: Host
    }


type Gameboy = |Gameboy of GameboyAgent*GameboyComponents

let create (rom: ROM) renderer host =
    let ram = GBCRam()

    let systemClock = MutableClock(GBC_SYSTEM_CLOCK_FREQUENCY,0UL)

    let interrupts = InterruptManager()

    let keypad = Keypad(interrupts)

    let timers = Timers(systemClock,interrupts)

    let gpu = GPU(systemClock, interrupts, renderer, host)

    let mmu = MMU(gpu, rom,ram,keypad,interrupts,timers)

    let cpu = CPU(mmu,interrupts,systemClock)

    cpu.Reset ()
    gpu.Reset ()
    mmu.InitDefaults ()

    
    let components = {Ram = ram; Clock = systemClock; Interrupts = interrupts;
                      Keypad = keypad; Timers = timers; Gpu = gpu; Mmu = mmu;
                      Cpu = cpu; Host = host}

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
                                    
            let rec handleMessage state = async {

                let! message = mailbox.Receive ()

                // Emulator control
                match message with
                | Run when state = Running ->
                    // Since agent posting / receiving incurs an overhead
                    // we let the emulator execute several times before processing next message.
                    try
                        runEmulation 20
                        mailbox.Post Run
                    with
                    | error ->
                        do Log.logf "Runtime errror!\n%s\n%s" error.Message error.StackTrace
                        do host.Error error
                        mailbox.Post Kill
                | Reset ->
                    cpu.Reset ()
                    gpu.Reset ()
                    systemClock.Reset ()
                    mmu.InitDefaults ()
                | Step reply ->
                    try
                        runEmulation 1
                        reply.Reply None 
                    with error ->
                        do Log.logf "Runtime errror!\n%s\n%s" error.Message error.StackTrace
                        do host.Error error
                        reply.Reply (Some error) 
                        mailbox.Post Kill 
                | Input (key,state)->
                    keypad.[key] <- state
                | State reply ->
                    reply.Reply state
                | Write property ->
                    match property with
                    | PropertyValue.Speed speed ->
                        gpu.Speed <- speed
                    | PropertyValue.Palette palette ->
                        gpu.Palette <- palette
                | Read property ->
                    match property with
                    | Speed reply ->
                        reply.Reply gpu.Speed
                    | Palette reply ->
                        reply.Reply gpu.Palette
                    | FPS reply ->
                        reply.Reply gpu.FPS
                | _ ->
                    ()

                // Agent control
                match message with
                | Kill ->
                    // Stop processing messages (by stopping recursion)
                    ()
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

let post message (Gameboy (agent,_)) = agent.Post message

let postAndReply builder (Gameboy (agent,_)) = agent.PostAndReply builder

let components (Gameboy (_,components)) =  components

let start = post Start

let pause = postAndReply Pause

let reset = post Reset

let state = postAndReply State

let step gameboy =
    match postAndReply Step gameboy with
    | Some error -> raise error
    | None -> ()

let kill = post Kill

let postInput key state = post <| Input(key,state)

let fps = postAndReply <| (Read << FPS)

let speed = postAndReply <| (Read << Speed)

let setSpeed  = post << (Write << PropertyValue.Speed)

let palette = postAndReply <| (Read << Palette)

let setPalette = post << (Write << PropertyValue.Palette)