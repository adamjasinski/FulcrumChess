 namespace FulcrumChess.Engine.Tests

 module BitUtilsTests =
    open System
    open Xunit
    open Swensen.Unquote
    open FulcrumChess.Engine

    [<Theory>]
    [<InlineDataEx(1, [|0|])>]
    [<InlineDataEx(8, [|3|])>]
    [<InlineDataEx(141, [|0;2;3;7|])>]
    [<InlineDataEx(2017, [|0;5;6;7;8;9;10|])>]
    let ``getSetBits (32-bit) returns expected result `` (input:int, expectedResult:int[]) =
        let result = input |> BitUtils.getSetBits_32
        printfn "%s:  %d elements" "GetSetBits found something" result.Length
        printfn "%A" result
        test <@ (result |> Array.length) > 0 @>
        test <@ expectedResult = result  @>

    [<Theory>]
    [<InlineDataEx(1UL, [|0|])>]
    [<InlineDataEx(8UL, [|3|])>]
    [<InlineDataEx(141UL, [|0;2;3;7|])>]
    [<InlineDataEx(2017UL, [|0;5;6;7;8;9;10|])>]
    let ``getSetBits (64-bit) returns expected result `` (input:uint64, expectedResult:int[]) =
        let result = input |> BitUtils.getSetBits_u64
        printfn "%s:  %d elements" "GetSetBits found something" result.Length
        printfn "%A" result
        test <@ (result |> Array.length) > 0 @>
        test <@ expectedResult = result  @>

    [<Fact>]
    let ``getSetBits for zero returns empty array`` () =
        let input = 0
        let expectedResult:int[] = [||]
        let result = input |> BitUtils.getSetBits
        test <@ expectedResult = result  @>

    [<Fact>]
    let ``getSetBits for large number returns expected result`` () =
        let input = 1266637395197952UL
        let expectedResult:int[] = [|47; 50|]
        let result = input |> BitUtils.getSetBits
        test <@ expectedResult = result  @>

    [<Theory>]
    [<InlineDataEx(1UL, 1)>]
    [<InlineDataEx(3UL, 2)>]
    [<InlineDataEx(141UL, 4)>]
    [<InlineDataEx(2017UL, 7)>]
    let ``Hamming.popcount returns expected result`` (input:uint64, expectedResult:int) =
        let result = input |> BitUtils.Hamming.popcount_64
        test <@ expectedResult = result  @>

    [<Theory>]
    [<InlineDataEx(1UL, 1)>]
    [<InlineDataEx(3UL, 2)>]
    [<InlineDataEx(141UL, 4)>]
    [<InlineDataEx(2017UL, 7)>]
    let ``Slow bitcount returns expected result`` (input:uint64, expectedResult:int) =
        let result = input |> BitUtils.countBits_slow
        test <@ expectedResult = result  @>