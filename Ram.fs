module Ram

open MemoryCell
open Units

type GBCRam () =
    member val Working = 8*kB |> readWriteMemoryBlock
    member val Stack = 127<byte> |> readWriteMemoryBlock 
