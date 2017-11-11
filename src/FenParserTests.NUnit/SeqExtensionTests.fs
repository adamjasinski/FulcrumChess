namespace FenParserTests.NUnit
open FenParser

module SeqExtensionsTakeUntilInclusiveTests =
    open Swensen.Unquote
    open NUnit.Framework

    [<Test>]
    let ``applying on empty sequence should return empty sequence`` () =
        let seq1 = Seq.empty
        let result = seq1 |> Seq.takeUntilInclusive (fun x -> true) 
        test <@ result |> Seq.isEmpty @>

    [<Test>]
    let ``applying on a finite sequence should return expected sequence`` () =
        let seq1 = seq { for i in 1 .. 20 -> i}
        let result = seq1 |> Seq.takeUntilInclusive (fun x -> x >= 10) 
        test <@ 10 = (result |> Seq.length) @>
        test <@ [|1..10|] = (result |> Array.ofSeq) @>

    [<Test>]
    let ``applying on a finite sequence with no element matching the stop condition should return the whole sequence`` () =
        let seq1 = seq { for i in 1 .. 10 -> i}
        let result = seq1 |> Seq.takeUntilInclusive (fun x -> x >= 30) 
        test <@ 10 = (result |> Seq.length) @>
        test <@ [|1..10|] = (result |> Array.ofSeq) @>
        