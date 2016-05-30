
module OS

open System

type OS =
        | OSX            
        | Windows
        | Linux

let this = 
        match int Environment.OSVersion.Platform with
        | 4 | 128 -> Linux
        | 6       -> OSX
        | _       -> Windows