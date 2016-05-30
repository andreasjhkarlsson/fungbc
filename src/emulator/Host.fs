
module Host

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
type Host =
    abstract Error: exn -> unit 
    abstract Idle: int -> unit // Idle max n microseconds.
