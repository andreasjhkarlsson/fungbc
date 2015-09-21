module Clock

open Units
open Constants

// Something that ticks at a certain frequency
[<AbstractClass>]
type Clock (frequency: int<Hz>) =

    abstract Ticks: uint64 with get

    member this.Frequency = frequency

    member this.Seconds = ((float this.Ticks) / (float frequency) )

    member this.MilliSeconds = this.Seconds * 1000.0



// This clock ticks according to a mutable value that clients manipulate
type MutableClock (frequency,start) =
    inherit Clock(frequency)
    let mutable ticks = start
    override this.Ticks with get () = ticks

    member this.Reset () = ticks <- 0UL

    member this.Tick count = ticks <- ticks + count

// This clock ticks after another, reference clock
type DerivedClock(reference: Clock, frequency, start) =
    inherit Clock(frequency)

    let mutable offset = start

    let factor = uint64 <| reference.Frequency / frequency

    member this.Reset () = offset <- reference.Ticks

    override this.Ticks with get () = (reference.Ticks - offset) / factor

// This clock doesn't tick at all.
type AbsoluteClock(reference: Clock) =
    inherit Clock(reference.Frequency)

    let ticks = reference.Ticks

    override this.Ticks with get () = ticks



// A clock that switch between 1 and 0 according to reference clock
type FlipClock(reference: Clock) =
    inherit MutableClock(reference.Frequency, reference.Ticks)

    override this.Ticks with get () = (base.Ticks + reference.Ticks) % (2UL)

    member this.Flipped = this.Ticks = 1UL

    member this.Reset () = if this.Flipped then this.Tick 1UL
        


// Create a new clock from an existing one with specified frequency
let derive clock frequency = DerivedClock(clock,frequency,clock.Ticks) :> Clock

// Create a frozen copy of a clock
let freeze clock = AbsoluteClock(clock) :> Clock

// Create a clock that will begin resume ticking according to a reference
let unfreeze (clock: Clock) reference = DerivedClock(reference,clock.Frequency,reference.Ticks - clock.Ticks) :> Clock
    