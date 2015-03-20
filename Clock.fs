module Clock

open Units
open Constants

type Clock () =
    let mutable cycles = 0UL

    member this.Tick count = cycles <- cycles + count

    member this.Cycles = cycles

    member this.Seconds = ((float cycles) / (float CLOCK_FREQUENCY) )

    member this.MilliSeconds = this.Seconds * 1000.0

    member this.Print () = printfn "Clock:\n\tCycles: %d,\tTime: %.4f ms" cycles this.MilliSeconds