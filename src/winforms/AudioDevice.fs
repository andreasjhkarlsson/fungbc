
module AudioDevice

open Types
open NAudio
open NAudio.Wave

type AudioDevice () =
    
    let format = new WaveFormat(int Constants.AudioConfig.SampleRate,Constants.AudioConfig.BitDepth,Constants.AudioConfig.Channels)
    let provider = new BufferedWaveProvider(format)

    let device = new WaveOut ()

    member this.Init () = do device.Init provider

    member this.Start () = do device.Play ()

    member this.Pause () = do device.Pause ()

    interface Host.SoundReceiver with
        member this.PlaySamples samples count = do provider.AddSamples(samples,0,count)

        member this.Buffered = provider.BufferedBytes

        member this.Start () = this.Start ()

        member this.Stop () = this.Pause ()