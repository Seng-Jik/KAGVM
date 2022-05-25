namespace KAGVM.Utils

open System.Collections
open System.Collections.Generic


type IDGenerator<'a when 'a: comparison> () = 
    let mutable generated = Map.empty: Map<'a, uint64>
    let mutable nextId = 0UL
    
    member _.Item
        with get index =
            match Map.tryFind index generated with
            | Some x -> x
            | None ->
                let curId = nextId
                nextId <- nextId + 1UL
                generated <- Map.add index curId generated
                curId
    
    interface IEnumerable with
        member _.GetEnumerator () = (Map.toSeq generated).GetEnumerator ()

    interface IEnumerable<'a * uint64> with
        member _.GetEnumerator () = (Map.toSeq generated).GetEnumerator ()

