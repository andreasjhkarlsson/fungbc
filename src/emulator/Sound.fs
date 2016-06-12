
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

module Mixer =
    
    type Buffer(size) =

        let buffer = Array.zeroCreate size

        let mutable used = 0

        member this.Add item =
            buffer.[used] <- item
            used <- used + 1

        member this.AddMany (items: AudioSample[]) =
            let count = min this.Available items.Length
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
            destination.AddMany this.Data
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

    type Agent = {Processor: MailboxProcessor<Message>; FrontBuffer: Buffer}

    let Add sample (agent: Agent) =
    
        do agent.FrontBuffer.Add sample
        if agent.FrontBuffer.Full then
            let newBuffer = BufferPool.checkout agent.FrontBuffer.Size
            newBuffer.AddMany agent.FrontBuffer.Data
            do newBuffer |> Process |> agent.Processor.Post
            do agent.FrontBuffer.Clear ()

    let create (host: Host) =
        
        let soundReceiver = host.SoundReceiver

        let backBuffer = Buffer(Constants.AudioConfig.SampleRate / 10<Hz>)

        let frontBuffer = Buffer(backBuffer.Size / 10)

        let sendToHost (buffer: Buffer) count =
            do soundReceiver.PlaySamples Speaker.Left buffer.Data count
            do buffer.TrimFront count

        let processor = MailboxProcessor.Start (fun mb ->

            do soundReceiver.Start ()

            let rec background () = async {
                let! msg = mb.Receive ()

                match msg with
                | Process buffer ->
                    do backBuffer.AddMany buffer.Data

                    do BufferPool.checkin buffer

                    if soundReceiver.Buffered < backBuffer.Size*2 then
                        do sendToHost backBuffer (min backBuffer.Used (backBuffer.Size*2-soundReceiver.Buffered))

                    do
                        Log.logf "Audio playback: Backbuffer = %f ms, device buffer = %f ms"
                            ((float backBuffer.Used) / (float Constants.AudioConfig.SampleRate) |> (*) 1000.0)
                            ((float soundReceiver.Buffered) / (float Constants.AudioConfig.SampleRate) |> (*) 1000.0)
                    
                return! background ()
            }

            background ()
        )

        { Processor = processor; FrontBuffer = frontBuffer }


type Channel () =
    inherit obj ()

type Square2 (soundClock: Clock) as this =
    inherit Channel ()

    let mutable leftInDuty = 0
    let mutable length = 0
    let mutable volume = 0uy
    let mutable waveClock = Clock.derive soundClock 1<Hz>
    let mutable envelopeClock = None
    let mutable lengthClock = Clock.derive soundClock 256<Hz>

    member val Enable = false with get, set


    member this.Start () =
        this.Enable <- true

        lengthClock <- Clock.derive soundClock 256<Hz>
        waveClock <- Clock.derive soundClock this.Frequency
        envelopeClock <- if this.Period <> 0uy then (int this.Period) * 64<Hz> |> Clock.derive soundClock |> Some else None
        volume <- this.StartVolume

    member this.Stop () =
        this.Enable <- false


    member val NR21 = {
        new ValueBackedIORegister (0uy) with
            override x.MemoryValue
                with get () = x.Value &&& 0b11000000uy
                and set value =
                     x.Value <- value
                     length <- 64 - (x.GetBits 5 0 |> int)
    }

    member val NR22 = ValueBackedIORegister(0uy)

    member val NR23 = {
        new ValueBackedIORegister(0uy) with
            override x.MemoryValue
                with get () = 0uy
    }

    member val NR24 = {
        new ValueBackedIORegister(0uy) with
            override x.MemoryValue
                with get () = x.Value &&& 0b01000000uy
                and set value =
                    x.Value <- value
                    if value |> isBitSet 7 then do this.Start ()
    }

    member this.Duty =
        match this.NR21.GetBits 7 6 with
        | 0uy -> 0.125
        | 1uy -> 0.25
        | 2uy -> 0.5
        | 3uy -> 0.75
        | _ -> failwith "Unknown sound duty"


    member this.StartVolume = this.NR22.GetBits 7 4

    member this.EnvelopeAddMode = if this.NR22.GetBit 3 = SET then Add else Subtract

    member this.Period = this.NR22.GetBits 2 0

    member this.PlayMode = if this.NR24.GetBit 6 = SET then Counter else Continuous

    member this.Frequency =
        let regValue = ((int this.NR24.Value &&& 0b111) <<< 8) ||| (int this.NR23.Value)
        4194304<Hz> / (32 * (2048 - regValue))


    member val CurrentSample = 0uy with get, set

    member this.Update () =
        
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

            this.CurrentSample <-
                if leftInDuty > 0 then
                    leftInDuty <- leftInDuty - 1
                    volume * 4uy
                else 0uy
        else
            this.CurrentSample <- 0uy


type GBS (systemClock: Clock, host) =

    let soundClock = Clock.derive systemClock Constants.AudioConfig.SampleRate

    let mutable n = 0

    let stopwatch = new Stopwatch()

    let mixer = Mixer.create host

    member val Square2 = Square2 (soundClock)

    member val NR50 = ValueBackedIORegister(0uy)
    member val NR51 = ValueBackedIORegister(0uy)
    member val NR52 = {
        new ValueBackedIORegister(0uy) with
            override x.MemoryValue
                with get () = x.Value
                and set value = x.Value <- value &&& 0x80uy 
    }

    member this.LeftSpeakerEnabled = this.NR50.GetBit 3 = SET
    member this.LeftSpeakerVolume = this.NR50.GetBits 2 0

    member this.RightSpeakerEnabled = this.NR50.GetBit 7 = SET
    member this.RightSpeakerVolumne = this.NR50.GetBits 6 4

    member this.Square2Left = this.NR51.GetBit 1 = SET
    member this.Square2Right = this.NR51.GetBit 5 = SET

    member this.MasterEnable = this.NR52.GetBit 7 = SET
    member this.Square2Enable = this.NR52.GetBit 1 = SET

    member this.Update () =
    
        do soundClock.Ticked (fun ticks ->

            let sample =
                if this.MasterEnable then
                    if this.Square2.Enable then
                        do this.Square2.Update ()
                        this.Square2.CurrentSample
                    else
                        0uy
                else
                    0uy

            mixer |> Mixer.Add sample

        )


