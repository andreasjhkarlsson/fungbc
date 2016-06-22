
module Configuration

open Types
open Palette

type Renderer =
    abstract member SetPixel: int -> int -> int -> unit
    abstract member GetPixel: int -> int -> int
    abstract member Flush: unit -> unit


type AudioDevice =
    abstract member PlaySamples: AudioSample [] -> int -> unit
    abstract member Buffered: int
    abstract member Start: unit -> unit
    abstract member Stop: unit -> unit

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
type Configuration = {
        Renderer: Renderer
        AudioDevice: AudioDevice
        ErrorFn: exn -> unit
        Idle: int -> unit // Idle for max n microseconds.
        Speed: Types.Speed
        Palette: Palette
        KeepAudioPitch: bool
        EnableAudio: bool
    }
