module Ram

open Units
open Memory

type GBCRam () =
    member val Working =  ReadWriteMemoryBlock(8*kB)
    member val Stack = ReadWriteMemoryBlock(127<b>)
