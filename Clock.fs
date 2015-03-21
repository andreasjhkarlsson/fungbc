module Clock

open Units
open Constants

[<AbstractClass>]
type Clock (frequency: int<Hz>) =
    abstract Ticks: uint64 with get

    member this.Frequency = frequency

    member this.Seconds = ((float this.Ticks) / (float frequency) )

    member this.MilliSeconds = this.Seconds * 1000.0

    member this.Print () = printfn "Clock:\n\tCycles: %d,\tTime: %.4f ms" this.Ticks this.MilliSeconds


type MutableClock (frequency,start) =
    inherit Clock(frequency)
    let mutable ticks = start
    override this.Ticks with get () = ticks

    member this.Tick count = ticks <- ticks + count

type DerivedClock(reference: Clock, start) =
    inherit Clock(reference.Frequency)

    override this.Ticks with get () = reference.Ticks - start

type AbsoluteClock(reference: Clock) =
    inherit Clock(reference.Frequency)

    let ticks = reference.Ticks

    override this.Ticks with get () = ticks

let derive clock = DerivedClock(clock,0UL) :> Clock

let freeze clock = AbsoluteClock(clock) :> Clock

let unfreeze (clock: Clock) reference = DerivedClock(reference,reference.Ticks - clock.Ticks) :> Clock
