namespace FulcrumChess.Engine
open System.Collections.Generic

module Seq =
    //Returns elements from the sequence until stopCondition occurs (including the first element for which it happened)
    //Cf. https://stackoverflow.com/a/12564899
    let takeUntilInclusive stopCondition (s:seq<_>) = 
        let rec loop (en:IEnumerator<_>) = seq {
            if en.MoveNext() then
                // Always yield the current element
                yield en.Current
                //Cntinue unless stopCondition occurs
                if not (stopCondition en.Current) then
                    yield! loop en 
            }
        // Get enumerator of the sequence and yield all results
        // (making sure that the enumerator gets disposed)
        seq { use en = s.GetEnumerator()
            yield! loop en  }

module Array =
    // Returns elements of array of options where isSome
    let chooseWhereSome (arr:'a option[]) =
        let res = ResizeArray<'a>(arr.Length)
        for i in 0..arr.Length-1 do
            if arr.[i] |> Option.isSome then res.Add(arr.[i].Value)
        res.ToArray()

module Tuple2 = 
    //Applies function f to each element of a tuple
    let map f (a, b) = (f a, f b)
    let mapFirst f (a, b) = (f a, b)
