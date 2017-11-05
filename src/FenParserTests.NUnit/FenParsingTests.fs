namespace FenParserTests.NUnit

open System
open NUnit.Framework
open Swensen.Unquote

module FenParsingTests = 

    [<TestCase>]
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
        let result = input |> FenParsing.parseToBoard8x8
        test <@ expected = result @>

    [<TestCase>]
    let ``Single row - all explicit`` () =
        let input = "rnbqkbnr"
        let expected = ['r';'n';'b';'q';'k';'b';'n';'r']
        
        let result = input |> FenParsing.parseSingleRow
        test <@ expected = result @>

    [<TestCase>]
    let ``Single row - some fields empty`` () =
        let input = "rnb3nr"
        let expected = ['r';'n';'b';' ';' ';' ';'n';'r']
        
        let result = input |> FenParsing.parseSingleRow
        test <@ expected = result @>

    [<TestCase>]
    let ``Single row - some fields empty (2)`` () =
        let input = "2b3nr"
        let expected = [' ';' ';'b';' ';' ';' ';'n';'r']
        
        let result = input |> FenParsing.parseSingleRow
        test <@ expected = result @>

    [<TestCase>]
    let ``Single row - entire row empty`` () =
        let input = "8"
        let expected = [' ';' ';' ';' ';' ';' ';' ';' ']
        
        let result = input |> FenParsing.parseSingleRow
        test <@ expected = result @>

    [<TestCase>]
    let ``Single row - row too long`` () =
        let input = "rnbqkbnrr"

        raises<ArgumentException> <@ input |> FenParsing.parseSingleRow @>

    [<TestCase>]
    let ``Single row - too many empty squares`` () =
        let input = "rnb6q"

        raises<InvalidOperationException> <@ input |> FenParsing.parseSingleRow  @>
