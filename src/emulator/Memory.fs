module Memory

open Units

type MemoryAddress = uint16

type IMemoryCell =
    abstract Value: uint8 with get, set

[<AbstractClass>]
type MemoryBlock () =  
    abstract member Read: MemoryAddress -> uint8
    abstract member Write: MemoryAddress -> uint8 -> unit
    member x.Item address = x.Read address

type ReadWriteMemoryBlock(size: int<b>) =
    
    inherit MemoryBlock ()

    let memory: uint8 array = Array.zeroCreate (int size)

    override x.Read address = memory.[int address]

    override x.Write address value = memory.[int address] <- value

    member x.Array = memory