namespace FulcrumChess.Engine.Tests

open System
open Xunit
open Swensen.Unquote
open FulcrumChess.Engine

module FenParsingTests = 

    let private blankRow = List.replicate 8 ' '

    [<Fact>]
    let ``Initial position`` () =

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

        raises<ArgumentException> <@ input |> FenParsing.parseSingleRow @>

    [<Fact>]
    let ``Single row - too many empty squares`` () =
        let input = "rnb6q"

        raises<InvalidOperationException> <@ input |> FenParsing.parseSingleRow  @>


    [<Fact>]
    let ``Parse initial position to Position instance`` () =
        let input = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        let result = input |> FenParsing.parseToPosition

        let expectedAsBoard8x8 = [
                ['r';'n';'b';'q';'k';'b';'n';'r'];
                ['p';'p';'p';'p';'p';'p';'p';'p'];
                blankRow;
                blankRow;
                blankRow;
                blankRow;
                ['P';'P';'P';'P';'P';'P';'P';'P'];
                ['R';'N';'B';'Q';'K';'B';'N';'R'];
            ]
        let actualResultAsBoard8x8 = result |> Board8x8Funcs.fromPosition
        printfn "%A" actualResultAsBoard8x8
        test <@ expectedAsBoard8x8 = actualResultAsBoard8x8 @>

    [<Fact>]
    let ``Parse last position to of the Immortal game`` () =
        let input = "r1bk3r/p2pBpNp/n4n2/1p1NP2P/6P1/3P4/P1P1K3/q5b1 b - - 0 22"
        let result = input |> FenParsing.parseToPosition

        let expectedAsBoard8x8 = [
                ['r';' ';'b';'k';' ';' ';' ';'r'];
                ['p';' ';' ';'p';'B';'p';'N';'p'];
                ['n';' ';' ';' ';' ';'n';' ';' '];
                [' ';'p';' ';'N';'P';' ';' ';'P'];
                [' ';' ';' ';' ';' ';' ';'P';' '];
                [' ';' ';' ';'P';' ';' ';' ';' ']
                ['P';' ';'P';' ';'K';' ';' ';' '];
                ['q';' ';' ';' ';' ';' ';'b';' '];
            ]
        let actualResultAsBoard8x8 = result |> Board8x8Funcs.fromPosition
        printfn "%A" actualResultAsBoard8x8
        test <@ expectedAsBoard8x8 = actualResultAsBoard8x8 @>

    [<Fact>]
    [<Category("FenParsing")>]
    [<Category("Zobrist")>]
    let ``Parse specific position with en passant pawn`` () =
        let input = "rnbqkbnr/pp1ppppp/8/8/2pPP3/5N2/PPP2PPP/RNBQKB1R b KQkq d3 0 3"
        let result = input |> FenParsing.parseToPosition

        let expectedAsBoard8x8 = [
                ['r';'n';'b';'q';'k';'b';'n';'r'];
                ['p';'p';' ';'p';'p';'p';'p';'p'];
                [' ';' ';' ';' ';' ';' ';' ';' ']
                [' ';' ';' ';' ';' ';' ';' ';' '];
                [' ';' ';'p';'P';'P';' ';' ';' '];
                [' ';' ';' ';' ';' ';'N';' ';' '];
                ['P';'P';'P';' ';' ';'P';'P';'P'];
                ['R';'N';'B';'Q';'K';'B';' ';'R'];
            ]
        let actualResultAsBoard8x8 = result |> Board8x8Funcs.fromPosition
        printfn "%A" actualResultAsBoard8x8
        test <@ expectedAsBoard8x8 = actualResultAsBoard8x8 @>

        test <@ Side.Black = result.SideToPlay @>
        test <@ CastlingRights.Both = result.WhiteCastlingRights @>
        test <@ CastlingRights.Both = result.BlackCastlingRights @>
        test <@ 20 = result.EnPassantTarget @>
        test <@ 3 = result.FullMoveNumber @>
        let expectedHash = result |> Position.calculateZobristHash
        test <@ expectedHash = result.HashKey @>


