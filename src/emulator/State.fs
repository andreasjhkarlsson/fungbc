module State

type IEmulationState =
    abstract member Persist: unit -> obj
    abstract member Load: obj -> unit

type NameValueState<'a>(name, init: 'a) =
    
    member val Value = init with get, set

    interface IEmulationState with
        member x.Persist () = Map.ofList [name, box x.Value] :> obj
        member x.Load map = x.Value <- map :?> Map<string, obj> |> Map.find name |> unbox