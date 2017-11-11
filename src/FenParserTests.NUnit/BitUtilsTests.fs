 namespace FenParserTests.NUnit

 module BitUtilsTests =
    open System
    open NUnit.Framework
    open Swensen.Unquote
    open FenParser

    [<TestCase(1, [|0|])>]
    [<TestCase(8, [|3|])>]
    [<TestCase(141, [|0;2;3;7|])>]
    [<TestCase(2017, [|0;5;6;7;8;9;10|])>]
    let ``getSetBits returns expected result `` (input:int, expectedResult:int[]) =
        let result = input |> BitUtils.getSetBits
        printfn "%s:  %d elements" "GetSetBits found something" result.Length
        printfn "%A" result
        test <@ (result |> Array.length) > 0 @>
        test <@ expectedResult = result  @>

    [<Test>]
    let ``getSetBits for zero returns empty array`` () =
        let input = 0
        let expectedResult:int[] = [||]
        let result = input |> BitUtils.getSetBits
        test <@ expectedResult = result  @>

    [<Test>]
    let ``getSetBits for large number returns expected result`` () =
        let input = 1266637395197952UL
        let expectedResult:int[] = [|47; 50|]
        let result = input |> BitUtils.getSetBits
        test <@ expectedResult = result  @>

    [<TestCase(1UL, 1)>]
    [<TestCase(3UL, 2)>]
    [<TestCase(141UL, 4)>]
    [<TestCase(2017UL, 7)>]
    let ``Hamming.popcount returns expected result`` (input:uint64, expectedResult:int) =
        let result = input |> BitUtils.Hamming.popcount_64
        test <@ expectedResult = result  @>

    [<TestCase(1UL, 1)>]
    [<TestCase(3UL, 2)>]
    [<TestCase(141UL, 4)>]
    [<TestCase(2017UL, 7)>]
    let ``Slow bitcount returns expected result`` (input:uint64, expectedResult:int) =
        let result = input |> BitUtils.countBits_slow
        test <@ expectedResult = result  @>