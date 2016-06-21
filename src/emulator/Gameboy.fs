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
open Configuration

type State = |Running |Paused 

type Reply<'a> = AsyncReplyChannel<'a>


type Message = 
    |Kill
    |Run
    |Step of Reply<exn option>
    |Input of Input.Key*Input.KeyState
    |Pause of unit Reply
    |Start
    |State of State Reply
    |Reset
    |SetConfig of Configuration
    |GetConfig of Configuration Reply

type GameboyAgent = MailboxProcessor<Message>

type GameboyComponents = {
        Ram: GBCRam
        Clock: Clock
        Interrupts: InterruptManager
        Keypad: Keypad
        Timers: Timers
        Gpu: GPU
        Gbs: Sound.GBS
        Mmu: MMU
        Cpu: CPU
    }


type Gameboy = |Gameboy of GameboyAgent*GameboyComponents

let create (rom: ROM) config =

    let config = ref config

    let ram = GBCRam()

    let systemClock = MutableClock(GBC_SYSTEM_CLOCK_FREQUENCY,0UL)

    let interrupts = InterruptManager()

    let keypad = Keypad(interrupts)

    let timers = Timers(systemClock,interrupts)

    let gbs = Sound.GBS(systemClock,config)

    let gpu = GPU(gbs,systemClock, interrupts, config)

    let mmu = MMU(gpu,rom,ram,gbs,keypad,interrupts,timers)

    let cpu = CPU(mmu,interrupts,systemClock)

    cpu.Reset ()
    gpu.Reset ()
    mmu.InitDefaults ()

    
    let components = {Ram = ram; Clock = systemClock; Interrupts = interrupts;
                      Keypad = keypad; Timers = timers; Gpu = gpu; Mmu = mmu;
                      Cpu = cpu; Gbs = gbs}

    let agent = GameboyAgent.Start(fun mailbox ->
            // Run the emulator for a number of iterations
            let rec runEmulation iters =
                if iters > 0 then
                    // Execute a single instruction
                    do cpu.Execute ()
                    // Updates gpu state (may draw and raise interrupt)
                    do gpu.Update ()
                    // Update sound
                    do gbs.Update ()
                    // Increment timers (may raise interrupt)
                    do timers.Update ()

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
                        do (!config).ErrorFn error
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
                        do (!config).ErrorFn error
                        reply.Reply (Some error) 
                        mailbox.Post Kill 
                | Input (key,state)->
                    keypad.[key] <- state
                | State reply ->
                    reply.Reply state
                | SetConfig newConfig ->
                    do config := newConfig
                | GetConfig reply ->
                    do reply.Reply !config
                | _ ->
                    ()

                // Agent control
                match message with
                | Kill ->
                    // Stop processing messages (by stopping recursion)
                    ()
                | Pause reply ->
                    reply.Reply ()
                    do (!config).AudioDevice.Stop ()
                    return! handleMessage Paused
                | Start ->
                    do (!config).AudioDevice.Start ()
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

let getConfig = GetConfig |> postAndReply 

let setConfig = SetConfig >> post