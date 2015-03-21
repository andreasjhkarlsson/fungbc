module Timer

open Units
open Constants
open Clock

let timedCount (frequency: int<Hz>) (clock: Clock) =
    let start = clock.Ticks
    fun () ->  (float <| (clock.Ticks - start)) / ((float clock.Frequency) / (float frequency))