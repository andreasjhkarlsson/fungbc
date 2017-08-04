module MemoryCell

open Units
open State

type IMemoryCell =
    inherit IEmulationState
    abstract Value: uint8 with get, set

[<AbstractClass>]
type MemoryCell () =

    abstract Value: uint8 with get, set

    interface IEmulationState with
        member x.Persist () = box x.Value
        member x.Load obj = x.Value <- unbox obj
    interface IMemoryCell with
        member x.Value
            with get () = x.Value
            and set newValue = x.Value <- newValue
   
type ReadWriteCell (init) =
    inherit MemoryCell()
    let mutable value = init

    override x.Value
        with get () = value
        and set newValue = value <- newValue

type ReadOnlyCell (value) =
    inherit MemoryCell ()

    override this.Value
        with get () = value
        and set _ = ()

type MemoryBlock = array<IMemoryCell>

let readOnlyCell value = ReadOnlyCell(value) :> IMemoryCell

let readWriteCell value = ReadWriteCell(value) :> IMemoryCell

let blankCell = readOnlyCell 0uy

let blankMemoryBlock (size: int<b>): MemoryBlock = Array.create (size |> int) blankCell

let readWriteMemoryBlock (size: int<b>): MemoryBlock = Array.init (size |> int) (fun _ -> ReadWriteCell(0uy) :> IMemoryCell)

let initMemoryBlock (size: int<b>) fn: MemoryBlock = Array.init (size |> int) fn 

// TODO: Remove as this makes assumptions about persistment
let hookReadWrite (cell: IMemoryCell) read write =
    {
        new IMemoryCell with
            member x.Value
                with get () = read cell.Value
                and set value = cell.Value <- write value
        interface IEmulationState with
            member x.Persist () = cell.Persist ()
            member x.Load obj = cell.Load obj
    }

let hookRead fn cell = hookReadWrite cell fn (fun v -> v)

let hookWrite fn cell = hookReadWrite cell (fun v -> v) fn

