namespace FulcrumChess.Engine.Tests
open FulcrumChess.Engine
open Swensen.Unquote
open Xunit
open System

module BitboardTests =

    [<Theory>]
    [<InlineDataEx(0, "h1")>]
    [<InlineDataEx(7, "a1")>]
    [<InlineDataEx(8, "h2")>]
    [<InlineDataEx(56, "h8")>]
    [<InlineDataEx(63, "a8")>]
    let ``bitRefToAlgebraicNotation should map to expected notation`` bitRef expectedNotation =
        let result = Notation.bitRefToAlgebraicNotation bitRef
        test <@ expectedNotation = result @>

    
    [<Fact>]
    let ``quick test of getSetBits (1)`` () =
        let input = 2017
        let expectedResult = [|0;5;6;7;8;9;10|]
        let result = input |> BitUtils.getSetBits_32
        printfn "%s:  %d elements" "GetSetBits found something" result.Length
        printfn "%A" result
        test <@ (result |> Array.length) > 0 @>
        test <@ expectedResult = result  @>

 
 