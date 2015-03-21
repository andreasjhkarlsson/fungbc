module MemoryCell

[<AbstractClass>]
type MemoryCell () =
    abstract Value: uint8 with get, set
   
type ReadWriteCell (init) =
    inherit MemoryCell()

    let mutable value = init

    override this.Value
        with get () = value
        and set newValue = value <- newValue

type ReadOnlyCell (value) =
    inherit MemoryCell ()

    override this.Value
        with get () = value
        and set _ = printfn "Warning: tried to write to read-only cell"

// Memory cell not represented by a concrete value
type VirtualCell (getter, setter) =
    inherit MemoryCell()

    override this.Value
        with get () = getter ()
        and set newValue = setter newValue

type MemoryBlock = array<MemoryCell>

let readOnlyCell value = ReadOnlyCell(value) :> MemoryCell

let readWriteCell value = ReadWriteCell(value) :> MemoryCell

let blankCell = readOnlyCell 0uy

let blankMemoryBlock size: MemoryBlock = Array.create size blankCell

let readWriteMemoryBlock size: MemoryBlock = Array.init size (fun _ -> ReadWriteCell(0uy) :> MemoryCell)

let hookReadWrite (cell: MemoryCell) read write =
    let get () = read cell.Value
    let set value = cell.Value <- write value
    VirtualCell(get,set) :> MemoryCell

let hookRead (cell: MemoryCell) fn = hookReadWrite cell fn (fun v -> v)

let hookWrite fn (cell: MemoryCell) = hookReadWrite cell (fun v -> v) fn

