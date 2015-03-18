module Ram

open MemoryCell

type GBCRam () =
    member val Working = readWriteMemoryBlock 8192
    member val Stack = readWriteMemoryBlock 127
