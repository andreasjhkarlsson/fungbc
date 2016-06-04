
module Sound

open System.Diagnostics
open IORegisters
open IORegisters
open BitLogic
open Clock
open Types
open Units
open Host

type EnvelopeAddMode = Add | Subtract

type Channel () =
    inherit obj ()

type Square2 (soundClock: Clock) as this =
    inherit Channel ()

    let mutable leftInDuty = 0
    let mutable length = 0
    let mutable waveClock = Clock.derive soundClock 1<Hz>
    let mutable lengthClock = Clock.derive soundClock 256<Hz>

    member val Enable = false with get, set


    member this.Start () =
        this.Enable <- true

        lengthClock <- Clock.derive soundClock 256<Hz>
        waveClock <- Clock.derive soundClock this.Frequency

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

    member this.Frequency =
        let regValue = ((int this.NR24.Value &&& 0b111) <<< 8) ||| (int this.NR23.Value)
        4194304<Hz> / (32 * (2048 - regValue))


    member val CurrentSample = 0uy with get, set

    member this.Update () =
        
        if this.Enable then

            do waveClock.Ticked (fun _ ->
                leftInDuty <- (44100.0 / (float waveClock.Frequency)) * this.Duty |> int
            )

            do lengthClock.Ticked (fun _ ->
                length <- length - 1
                if length <= 0 then do this.Stop ()
            )

            this.CurrentSample <-
                if leftInDuty > 0 then leftInDuty <- leftInDuty - 1; 32uy else 0uy
        else
            this.CurrentSample <- 0uy


type GBS (systemClock: Clock, host: Host) =

    let soundClock = Clock.derive systemClock Constants.AudioConfig.SampleRate

    let mutable n = 0

    let stopwatch = new Stopwatch()

    let audioBuffer = Array.create (soundClock.Frequency / 10 |> int) 0uy

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
    
        if this.MasterEnable then do soundClock.Ticked (fun ticks ->
            
            let n = (uint32 ticks) % (uint32 audioBuffer.Length)
            
            do 
                this.Square2.Update ()
                audioBuffer.[int n] <- this.Square2.CurrentSample            

            if n = 0u then do
                Log.logf "Host buffer: %d ms" host.SoundReceiver.Buffered
                host.SoundReceiver.PlaySamples Speaker.Left audioBuffer
        )


