namespace FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open Swensen.Unquote
open FulcrumChess.Engine
open Bitboards
open FulcrumChess.Engine.Tests

type MoveGenPawnTests(magicGenerationSetupFixture:MagicGenerationSetupFixture) =

    let lookups = magicGenerationSetupFixture.Lookups

    let verifyMoves (fen:string, startBitRef:int, expectedSquaresList:string list) =
        let pos = FenParsing.parseToPosition fen

        let result = MoveGenerationLookupFunctions.generatePseudoMovesWithSpecial lookups pos startBitRef

        let algNotations = result |> movesToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = expectedSquaresList |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>

    interface IAssemblyFixture<MagicGenerationSetupFixture>

    static member TestCasesWhite() =
        seq {
            yield ("8/8/8/8/8/8/1P6/8 w - -", 14, ["b3"; "b4" ]); //b2
            yield ("8/8/8/5P2/8/8/8/8 w - -", 34, ["f6" ]);  //f5
            yield ("5P2/8/8/8/8/8/8/8 w - -", 58, List.empty<string>);  //f8
            yield ("8/8/8/2p1b3/3P4/8/8/8 w - -", 28, ["c5"; "d5"; "e5"]);  //f8 - captures
            yield ("8/8/8/2ppb3/3P4/8/8/8 w - -", 28, ["c5"; "e5"]);  //f8 - captures
            yield ("8/8/8/8/8/7N/7P/8 w - -", 8, List.empty<string>);  //h2, Nh3 blocking
        } 

    static member TestCasesBlack() =
        seq {
            yield ("8/7p/8/8/8/8/8/8 b - -", 48, ["h6"; "h5" ]); //h7
            yield ("8/8/8/p7/8/8/8/8 b - -", 39, ["a4" ]); //a5
            yield ("8/8/8/8/8/8/8/p7 b - -", 7, List.empty<string>); //a1
            yield ("8/8/8/3p4/2P1N3/8/8/8 b - -", 36, ["c4"; "d4"; "e4"]); //d5 - captures
            yield ("8/8/8/3p4/2PPN3/8/8/8 b - -", 36, ["c4"; "e4"]); //d5 - captures
            yield ("8/7p/7n/8/8/8/8/8 b - -", 48, List.empty<string>);  //h7, Nh6 blocking
        }

    [<Theory; MemberDataEx("TestCasesWhite")>]
    member __. ``verify moves of White Pawn (data bound)`` (fen:string, startBitRef:int, expectedSquaresList:string list) =
        verifyMoves (fen, startBitRef, expectedSquaresList)

    [<Theory; MemberDataEx("TestCasesBlack")>]
    member __. ``verify moves of Black Pawn (data bound)`` (fen:string, startBitRef:int, expectedSquaresList:string list) =
        verifyMoves (fen, startBitRef, expectedSquaresList)

    static member EnPassantTestCasesWhite() =
        seq {
            yield ("rnbqkbnr/pp2pppp/8/2ppP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3", 35, ["d6"; "e6" ]); //e5
            yield ("rnbqkbnr/pp2pppp/8/2ppP3/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 3", 35, [ "e6" ]); //e5, no en passant privilege
        } 

    static member EnPassantTestCasesBlack() =
        seq {
            yield ("rnbqkbnr/pp1ppppp/8/8/2pPP3/5N2/PPP2PPP/RNBQKB1R b KQkq d3 0 3", 29, ["c3"; "d3" ]); //c4
            yield ("rnbqkbnr/pp1p1ppp/8/4p3/2pPP3/5N2/PPP2PPP/RNBQKB1R w KQkq - 0 4", 29, ["c3"; ]); //c4, no en passant privilege
        } 

    [<Theory; MemberDataEx("EnPassantTestCasesWhite")>]
    member __. ``verify en passant-potential moves of White Pawn`` (fen:string, startBitRef:int, expectedSquaresList:string list) =
        verifyMoves (fen, startBitRef, expectedSquaresList)


    [<Theory; MemberDataEx("EnPassantTestCasesBlack")>]
    member __. ``verify en passant-potential moves of Black Pawn`` (fen:string, startBitRef:int, expectedSquaresList:string list) =
        verifyMoves (fen, startBitRef, expectedSquaresList)

    static member PromotionTestCasesWhite() =
        seq {
            yield ("4k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", 53, ["c8q"; "c8r"; "c8b"; "c8n" ]); //e5
            //yield ("4k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", 53, ["c8"; ]); //e5
            //yield ("rnbqkbnr/pp2pppp/8/2ppP3/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 3", 35, [ "e6" ]); //e5, no en passant privilege
        } 

    [<Theory(Skip="Pending"); MemberDataEx("PromotionTestCasesWhite")>]
    [<Category("Promotion")>]
    member __. ``verify promotion potential moves of White Pawn`` (fen:string, startBitRef:int, expectedSquaresList:string list) =
        verifyMoves (fen, startBitRef, expectedSquaresList)

