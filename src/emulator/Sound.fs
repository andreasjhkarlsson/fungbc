
module Sound

open System.Diagnostics
open IORegisters
open IORegisters
open BitLogic
open Clock
open Types
open Units
open Host
open System.Collections.Generic

type EnvelopeAddMode = Add | Subtract

type PlayMode = Counter | Continuous

type NR51 (init) =
    inherit ValueBackedIORegister(init)
    member this.Square1Left = this.GetBit 0 = SET
    member this.Square2Left = this.GetBit 1 = SET
    member this.Square1Right = this.GetBit 4 = SET
    member this.Square2Right = this.GetBit 5 = SET


[<AbstractClass>]
type Channel () =
    abstract Amplitude: float with get 
    abstract Update: unit -> unit
    abstract Enable: bool with get, set
    abstract LeftVolume: float
    abstract RightVolume: float

module Mixer =
    
    type Buffer(size) =

        let buffer = Array.zeroCreate size

        let mutable used = 0

        member this.Add item =
            buffer.[used] <- item
            used <- used + 1

        member this.AddMany (items: AudioSample[]) count =
            let count = min this.Available count
            do System.Array.Copy(items,0,buffer,used,count)
            do used <- used + count

        member this.Used = used

        member this.Size = buffer.Length

        member this.Full = used = this.Size

        member this.Clear () = used <- 0

        member this.Available = this.Size - used

        member this.TrimFront count =
            System.Array.Copy(buffer,count,buffer,0,used-count)
            used <- used - count

        member this.TrimBack count = used <- used - count

        member this.Data = buffer

        member this.Move (destination: Buffer) =
            destination.AddMany this.Data this.Used
            this.Clear ()

    // An agent would have been much cleaner, but with unacceptable overhead :(
    module BufferPool =
        let cache = new Dictionary<int,Buffer list> ()
                
        let checkout size = lock cache (fun _ ->
            if not <| cache.ContainsKey(size) then cache.[size] <- []
            match cache.[size] with
            | head::tail ->
                cache.[size] <- tail
                head
            | [] -> Log.log "Created new audio buffer"; Buffer(size)
        )

        let checkin (buffer: Buffer) = lock cache (fun _ ->
            do buffer.Clear ()
            do cache.[buffer.Size] <-  buffer :: cache.[buffer.Size]
        )

    type Message = Process of Buffer

    type Agent = {Processor: MailboxProcessor<Message>; AudioBuffer: Buffer; Host: Host}

    let sync (agent: Agent) =

        let buffered = agent.Host.SoundReceiver.Buffered

        let extraSamples = buffered - ((int  Constants.AudioConfig.SampleRate * Constants.AudioConfig.Channels) / (1000 / Constants.AudioConfig.BufferLength))

        if extraSamples > 500 then

            let microsToWait =
                (float extraSamples)
                / (float Constants.AudioConfig.SampleRate)
                * 1000000.0 // seconds -> microseconds
                |> int

            if microsToWait > 500 then
                agent.Host.Idle (microsToWait / 2) // Don't wait all the calculated time, always time to do it next call anyways
                      

    let flush (agent: Agent) = 
        if agent.Host.SoundReceiver.Buffered < 10 then Log.log "Audio buffer exhausted, there may be audio artifacts"
        let newBuffer = BufferPool.checkout agent.AudioBuffer.Size
        do newBuffer.AddMany agent.AudioBuffer.Data agent.AudioBuffer.Used
        do newBuffer |> Process |> agent.Processor.Post
        do agent.AudioBuffer.Clear ()

    let mix (channels: Channel list) (masterLeftVolume,masterRightVolume) (agent: Agent) =

        let buffer sample =
    
            do agent.AudioBuffer.Add sample

            if agent.AudioBuffer.Full then
                do flush agent

        let left, right =
            channels |> List.fold (fun (left, right) channel ->
                if channel.Enable then
                    left + channel.Amplitude * channel.LeftVolume,
                    right + channel.Amplitude * channel.RightVolume
                else
                    left, right
            ) (0.0, 0.0)

        do buffer (left * masterLeftVolume  |> min 2.0 |> (*) 32.0 |> int |> uint8)
        do buffer (right * masterRightVolume |> min 2.0 |> (*) 32.0 |> int |> uint8)

    let create (host: Host) =
        
        let soundReceiver = host.SoundReceiver

        let audioBuffer = Buffer(((int Constants.AudioConfig.SampleRate * Constants.AudioConfig.Channels) /
                                        (1000 / Constants.AudioConfig.BufferLength)))

        let sendToHost (buffer: Buffer) count =
            do soundReceiver.PlaySamples buffer.Data count
            do buffer.TrimFront count

        let processor = MailboxProcessor.Start (fun mb ->

            do soundReceiver.Start ()

            let rec background () = async {

                let! msg = mb.Receive ()

                match msg with
                | Process buffer ->
                    do
                        Log.logf "Audio playback: device buffer = %f ms"
                            ((float soundReceiver.Buffered) / (float Constants.AudioConfig.SampleRate) |> (*) 500.0)

                    do soundReceiver.PlaySamples buffer.Data buffer.Used

                    do BufferPool.checkin buffer
                    
                return! background ()
            }

            background ()
        )

        { Processor = processor; AudioBuffer = audioBuffer; Host = host}

[<AbstractClass>]
type SquareChannel (soundClock: Clock) as this =
    inherit Channel ()

    let mutable leftInDuty = 0
    let mutable length = 0
    let mutable volume = 0uy
    let mutable waveClock = Clock.derive soundClock 1<Hz>
    let mutable envelopeClock = None
    let mutable lengthClock = Clock.derive soundClock 256<Hz>
    let mutable currentAmp = 0.0
    let mutable enable = false

    override this.Enable
        with get () = enable
        and set state = enable <- state


    member this.Start () =
        this.Enable <- true

        lengthClock <- Clock.derive soundClock 256<Hz>
        waveClock <- Clock.derive soundClock this.Frequency
        envelopeClock <- if this.Period <> 0uy then 64<Hz> / (int this.Period) |> Clock.derive soundClock |> Some else None
        volume <- this.StartVolume

    member this.Stop () =
        this.Enable <- false


    member val NRx1 = {
        new ValueBackedIORegister (0uy) with
            override x.MemoryValue
                with get () = x.Value &&& 0b11000000uy
                and set value =
                     x.Value <- value
                     length <- 64 - (x.GetBits 5 0 |> int)
    }

    member val NRx2 = ValueBackedIORegister(0uy)

    member val NRx3 = {
        new ValueBackedIORegister(0uy) with
            override x.MemoryValue
                with get () = 0uy
    }

    member val NRx4 = {
        new ValueBackedIORegister(0uy) with
            override x.MemoryValue
                with get () = x.Value &&& 0b01000000uy
                and set value =
                    x.Value <- value
                    if value |> isBitSet 7 then do this.Start ()
    }

    member this.Duty =
        match this.NRx1.GetBits 7 6 with
        | 0uy -> 0.125
        | 1uy -> 0.25
        | 2uy -> 0.5
        | 3uy -> 0.75
        | _ -> failwith "Unknown sound duty"


    member this.StartVolume = this.NRx2.GetBits 7 4

    member this.EnvelopeAddMode = if this.NRx2.GetBit 3 = SET then Add else Subtract

    member this.Period = this.NRx2.GetBits 2 0

    member this.PlayMode = if this.NRx4.GetBit 6 = SET then Counter else Continuous

    member this.Frequency =
        let regValue = ((int this.NRx4.Value &&& 0b111) <<< 8) ||| (int this.NRx3.Value)
        4194304<Hz> / (32 * (2048 - regValue))


    override this.Amplitude = currentAmp

    override this.Update () =
        
        if this.Enable then

            do
                match envelopeClock with
                | Some envelopeClock ->
                    envelopeClock.Ticked (fun _ ->
                        volume <-
                            match this.EnvelopeAddMode with
                            | Add ->
                                if volume < 15uy then volume + 1uy else volume
                            | Subtract ->
                                if volume > 0uy then volume - 1uy else volume
                    )
                | None ->
                    ()

            do waveClock.Ticked (fun _ ->
                leftInDuty <- (44100.0 / (float waveClock.Frequency)) * this.Duty |> int
            )

            do lengthClock.Ticked (fun _ ->
                
                match this.PlayMode with
                | Counter ->
                    length <- length - 1
                    if length <= 0 then do this.Stop ()
                | Continuous ->
                    ()
            )

            currentAmp <-
                if leftInDuty > 0 then
                    leftInDuty <- leftInDuty - 1
                    (float volume) / 15.0
                else 0.0
        else
            currentAmp <- 0.0


type Square1 (soundClock: Clock, nr51: NR51) =
    inherit SquareChannel(soundClock)

    member val NR10 = ValueBackedIORegister(0uy)

    member this.NR11 = this.NRx1

    member this.NR12 = this.NRx2

    member this.NR13 = this.NRx3

    member this.NR14 = this.NRx4

    override this.LeftVolume = if nr51.Square1Left then 1.0 else 0.0
    
    override this.RightVolume = if nr51.Square1Right then 1.0 else 0.0

type Square2 (soundClock: Clock, nr51: NR51) =
    inherit SquareChannel(soundClock)

    member this.NR21 = this.NRx1

    member this.NR22 = this.NRx2

    member this.NR23 = this.NRx3

    member this.NR24 = this.NRx4

    override this.LeftVolume = if nr51.Square2Left then 1.0 else 0.0

    override this.RightVolume = if nr51.Square2Right then 1.0 else 0.0

type GBS (systemClock: Clock, host) as this =

    let soundClock = Clock.derive systemClock Constants.AudioConfig.SampleRate

    let mutable n = 0

    let stopwatch = new Stopwatch()

    let mixer = Mixer.create host

    let nr51 = NR51(0uy)

    let square1 = Square1(soundClock, nr51)

    let square2 = Square2(soundClock, nr51)

    let channels: Channel list = [square1; square2]

    member this.Mixer = mixer

    member this.Square1 = square1

    member this.Square2 = square2

    member val MasterEnable = false with get, set

    member val NR50 = ValueBackedIORegister(255uy)
    member val NR51 = nr51
    member val NR52 = {
        new ValueBackedIORegister(0uy) with
            override x.MemoryValue
                with get () =
                    BitLogic.mapByte (
                        function
                        | B7 -> this.MasterEnable |> BitLogic.setIfTrue
                        | B6 | B5 | B4 -> CLEAR
                        | B3 -> CLEAR // Sound 4
                        | B2 -> CLEAR // Sound 3
                        | B1 -> square2.Enable |> BitLogic.setIfTrue
                        | B0 -> CLEAR // Sound 1
                    )
                        
                and set value =
                    this.MasterEnable <- value |> BitLogic.isBitSet 7
                    square2.Enable <- value |> BitLogic.isBitSet 1
    }

    member this.LeftSpeakerEnabled = this.NR50.GetBit 3 = SET
    member this.LeftSpeakerVolume = (float <| this.NR50.GetBits 2 0 + 1uy) / 8.0

    member this.RightSpeakerEnabled = this.NR50.GetBit 7 = SET
    member this.RightSpeakerVolume = (float <| this.NR50.GetBits 6 4 + 1uy) / 8.0

    member this.Square2Enable = this.NR52.GetBit 1 = SET

    member this.Update () =
    
        do soundClock.Ticked (fun ticks ->

            if this.MasterEnable then

                channels |> List.iter (fun channel -> if channel.Enable then do channel.Update ())

                if this.Square2.Enable then do this.Square2.Update ()

                do mixer
                |> Mixer.mix
                    channels
                    ( this.LeftSpeakerVolume,
                      this.RightSpeakerVolume)
            else
                do mixer |> Mixer.mix [] (0.0,0.0)
        )


