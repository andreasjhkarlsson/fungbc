
module Host

open Types

type Renderer =
    abstract member SetPixel: int -> int -> int -> unit
    abstract member GetPixel: int -> int -> int
    abstract member Flush: unit -> unit

type SoundReceiver =
    abstract member PlaySamples: Speaker -> AudioSample [] -> int -> unit
    abstract member Buffered: int
    abstract member Start: unit -> unit
    abstract member Stop: unit -> unit

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
type Host =
    abstract Error: exn -> unit 
    abstract Idle: int -> unit // Idle max n microseconds.
    abstract Renderer: Renderer
    abstract SoundReceiver: SoundReceiver
