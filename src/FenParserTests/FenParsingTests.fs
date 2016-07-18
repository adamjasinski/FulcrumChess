namespace FenParserTests

open Xunit
open Swensen.Unquote

module FenParsingTests = 
    //member this.X = "F#"


//    [<Fact>]
//    let T3() =
//        let arr1 = [|1; 5; 100|]
//        let arr2 = [|1; 5; 100|]
//
//        test <@ arr1 = arr2 @>
//
//    [<Fact>]
//    let T4() =
//        let arr1 = [|[|1; 5; 100|]|]
//        let arr2 = [|[|1; 5; 100|]|]
//
//        test <@ arr1 = arr2 @>

    [<Fact>]
    let ``Initial position`` () =
        let blankRow = List.replicate 8 ' '
        let expected  =   [
                ['r';'n';'b';'q';'k';'b';'n';'r'];
                ['p';'p';'p';'p';'p';'p';'p';'p'];
                blankRow;
                blankRow;
                blankRow;
                blankRow;
                ['P';'P';'P';'P';'P';'P';'P';'P'];
                ['R';'N';'B';'Q';'K';'B';'N';'R'];
            ]
       
        let input = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        let result = input |> FenParsing.parse
        test <@ expected = result @>

    [<Fact>]
    let ``Single row - all explicit`` () =
        let input = "rnbqkbnr"
        let expected = ['r';'n';'b';'q';'k';'b';'n';'r']
        
        let result = input |> FenParsing.parseSingleRow
        test <@ expected = result @>

    [<Fact>]
    let ``Single row - some fields empty`` () =
        let input = "rnb3nr"
        let expected = ['r';'n';'b';' ';' ';' ';'n';'r']
        
        let result = input |> FenParsing.parseSingleRow
        test <@ expected = result @>

    [<Fact>]
    let ``Single row - some fields empty (2)`` () =
        let input = "2b3nr"
        let expected = [' ';' ';'b';' ';' ';' ';'n';'r']
        
        let result = input |> FenParsing.parseSingleRow
        test <@ expected = result @>

    [<Fact>]
    let ``Single row - entire row empty`` () =
        let input = "8"
        let expected = [' ';' ';' ';' ';' ';' ';' ';' ']
        
        let result = input |> FenParsing.parseSingleRow
        test <@ expected = result @>

    [<Fact>]
    let ``Single row - row too long`` () =
        let input = "rnbqkbnrr"
        
        let ex = Record.Exception( fun () -> input |> FenParsing.parseSingleRow |> ignore)
        let typedEx = Assert.IsType<System.ArgumentException>(ex)
        test <@ "input" = typedEx.ParamName @>

    [<Fact>]
    let ``Single row - too many empty squares`` () =
        let input = "rnb6q"
        
        let ex = Record.Exception( fun () -> input |> FenParsing.parseSingleRow |> ignore)
        Assert.IsType<System.InvalidOperationException>(ex) |> ignore
