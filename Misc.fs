module Misc

open Microsoft.FSharp.Reflection

///Returns the case name of the object with union type 'ty.
let GetUnionCaseName (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name  

///Returns the case names of union type 'ty.
let GetUnionCaseNames <'ty> () = 
    FSharpType.GetUnionCases(typeof<'ty>) |> Array.map (fun info -> info.Name)