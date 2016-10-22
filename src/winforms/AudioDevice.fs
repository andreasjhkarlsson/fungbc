
module AudioDevice

open System.Diagnostics
open Types
open NAudio
open NAudio.Wave

type AudioDevice () =
    
    let format = new WaveFormat(int Constants.AudioConfig.SampleRate,Constants.AudioConfig.BitDepth,Constants.AudioConfig.Channels)
    
    let provider = new BufferedWaveProvider(format)

    let device = new WaveOut()

    do device.DesiredLatency <- Constants.AudioConfig.BufferLength

    member this.Init () = do device.Init provider

    member this.Start () = do device.Play ()

    member this.Pause () = do device.Pause ()

    member this.Stop () =
        do device.Stop ()

    member this.IsPlaying () = device.PlaybackState = PlaybackState.Playing

    interface Configuration.AudioDevice with
        member this.PlaySamples samples count = do provider.AddSamples(samples,0,count)

        member this.Buffered = provider.BufferedBytes

        member this.Playing
            with get () =
                this.IsPlaying ()
            and set state =
                if state then this.Start () else this.Stop ()