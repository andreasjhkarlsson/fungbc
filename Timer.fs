module Timer

open Units
open Constants
open Clock

let timedCount (frequency: int<Hz>) (clock: Clock) =
    let start = clock.Cycles
    fun () ->  (float <| (clock.Cycles - start)) / ((float CLOCK_FREQUENCY) / (float frequency)) 