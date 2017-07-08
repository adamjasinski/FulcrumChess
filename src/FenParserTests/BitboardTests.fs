namespace FenParserTests

open Xunit
open Swensen.Unquote
open System

module BitboardTests =

    // [<Fact>]
    // let ``bitRefToAlgebraicNotation H1`` () =
    //     let result = Bitboards.bitRefToAlgebraicNotation 0
    //     test <@ "h1" = result @>

    // [<Fact>]
    // let ``bitRefToAlgebraicNotation A8`` () =
    //     let result = Bitboards.bitRefToAlgebraicNotation 63
    //     test <@ "a8" = result @>

    [<Theory>]
    [<InlineDataAttribute(0, "h1")>]
    [<InlineDataAttribute(7, "a1")>]
    [<InlineDataAttribute(8, "h2")>]
    [<InlineDataAttribute(56, "h8")>]
    [<InlineDataAttribute(63, "a8")>]
    let ``bitRefToAlgebraicNotation should map to expected notation`` bitRef expectedNotation =
        let result = Bitboards.bitRefToAlgebraicNotation bitRef
        test <@ expectedNotation = result @>

    
    [<Fact>]
    let ``quick test of getSetBits (1)`` () =
        let input = 2017
        let expectedResult = [|0;5;6;7;8;9;10|]
        let result = input |> FenParser.BitUtils.getSetBits
        printfn "%s:  %d elements" "GetSetBits found something" result.Length
        printfn "%A" result
        test <@ (result |> Array.length) > 0 @>
        test <@ expectedResult = result  @>

 
 