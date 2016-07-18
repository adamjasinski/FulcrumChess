namespace FenParserTests

open Xunit
open Swensen.Unquote
open Puzzles1

module PuzzlesTests1 = 

    [<Fact>]
    let findCommonImperative1 () =
        let A = [|1; 5; 12; 20; 25; 30; 30; 40|]
        let B = [|2; 12; 20; 30; 35; 40; 41|]
        let expected = [|12; 20; 30; 40|]
        let result = findCommonInSortedInputsImperative A B
        test <@ expected = result @>

    [<Fact>]
    let findCommonFunctional1 () =
        let A = [|1; 5; 12; 20; 25; 30; 30; 40|]
        let B = [|2; 12; 20; 30; 35; 40; 41|]
        let expected = [|12; 20; 30; 40|]
        let result = findCommonInSortedInputsFunctional A B
        test <@ expected = result @>

