namespace FulcrumChess.Engine.Tests.MoveGeneration
open NUnit.Framework
open Swensen.Unquote
open FulcrumChess.Engine
open Bitboards
open FulcrumChess.Engine.Tests

module MoveGenPawnTests =
    open MoveGenTestHelper

    let TestCasesWhite() =
        seq {
            yield TestCaseData("8/8/8/8/8/8/1P6/8 w - -", 14, ["b3"; "b4" ]); //b2
            yield TestCaseData("8/8/8/5P2/8/8/8/8 w - -", 34, ["f6" ]);  //f5
            yield TestCaseData("5P2/8/8/8/8/8/8/8 w - -", 58, List.empty<string>);  //f8
            yield TestCaseData("8/8/8/2p1b3/3P4/8/8/8 w - -", 28, ["c5"; "d5"; "e5"]);  //f8 - captures
            yield TestCaseData("8/8/8/2ppb3/3P4/8/8/8 w - -", 28, ["c5"; "e5"]);  //f8 - captures
        }

    let TestCasesBlack() =
        seq {
            yield TestCaseData("8/7p/8/8/8/8/8/8 b - -", 48, ["h6"; "h5" ]); //h7
            yield TestCaseData("8/8/8/p7/8/8/8/8 b - -", 39, ["a4" ]); //a5
            yield TestCaseData("8/8/8/8/8/8/8/p7 b - -", 7, List.empty<string>); //a1
            yield TestCaseData("8/8/8/3p4/2P1N3/8/8/8 b - -", 36, ["c4"; "d4"; "e4"]); //d5 - captures
            yield TestCaseData("8/8/8/3p4/2PPN3/8/8/8 b - -", 36, ["c4"; "e4"]); //d5 - captures
        }

    let verifyMoves (fen:string, startBitRef:int, expectedSquaresList:string list) =
        let lookups =  MagicGenerationSetupFixture.getCurrentLookups()
        let res = MoveGenerationLookupFunctions.generatePseudoMoves lookups 

        let pos = FenParsing.parseToPosition fen

        let result = MoveGenerationLookupFunctions.generatePseudoMoves lookups pos startBitRef

        let algNotations = result |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = expectedSquaresList |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>


    [<Test; TestCaseSource("TestCasesWhite")>]
    let ``verify moves of White Pawn (data bound)`` (fen:string, startBitRef:int, expectedSquaresList:string list) =
        verifyMoves (fen, startBitRef, expectedSquaresList)

    [<Test; TestCaseSource("TestCasesBlack")>]
    let ``verify moves of Black Pawn (data bound)`` (fen:string, startBitRef:int, expectedSquaresList:string list) =
        verifyMoves (fen, startBitRef, expectedSquaresList)

    
    [<Test>]
    let ``perft 3 level issue repro`` () =
        let lookups =  MagicGenerationSetupFixture.getCurrentLookups()
        let fen = "rnbqkbnr/pppppppp/8/8/8/7N/PPPPPPPP/RNBQKB1R b KQkq -"
        let pos = FenParsing.parseToPosition fen
        let srcBitRefs = pos |> Positions.getBitboardForSideToPlay |> BitUtils.getSetBits
        let pseudoMovesForSide = srcBitRefs |> Array.map (MoveGenerationLookupFunctions.generatePseudoMovesFullInfo lookups pos)
        let pseudoMovesCount = pseudoMovesForSide |> Array.collect id |> Array.length
        test <@ 20 = pseudoMovesCount @>
   
    //TODO - en passant