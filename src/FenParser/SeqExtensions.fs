namespace FenParser
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