namespace FenParserTests.NUnit

open NUnit.Framework
open Swensen.Unquote
open System

module BitboardTests =

    [<TestCase(0, "h1")>]
    [<TestCase(7, "a1")>]
    [<TestCase(8, "h2")>]
    [<TestCase(56, "h8")>]
    [<TestCase(63, "a8")>]
    let ``bitRefToAlgebraicNotation should map to expected notation`` bitRef expectedNotation =
        let result = Bitboards.bitRefToAlgebraicNotation bitRef
        test <@ expectedNotation = result @>

    
    [<Test>]
    let ``quick test of getSetBits (1)`` () =
        let input = 2017
        let expectedResult = [|0;5;6;7;8;9;10|]
        let result = input |> FenParser.BitUtils.getSetBits
        printfn "%s:  %d elements" "GetSetBits found something" result.Length
        printfn "%A" result
        test <@ (result |> Array.length) > 0 @>
        test <@ expectedResult = result  @>

 
 