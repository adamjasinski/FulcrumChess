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

        let result = MoveGenerationLookupFunctions.generatePseudoMoves lookups pos startBitRef

        let algNotations = result |> setBitsToAlgebraicNotations
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
        |> Seq.map( fun (a,b,c) -> [|box a; box b; box c|])

    static member TestCasesBlack() =
        seq {
            yield ("8/7p/8/8/8/8/8/8 b - -", 48, ["h6"; "h5" ]); //h7
            //yield ("8/8/8/p7/8/8/8/8 b - -", 39, ["a4" ]); //a5
            //yield ("8/8/8/8/8/8/8/p7 b - -", 7, List.empty<string>); //a1
            //yield ("8/8/8/3p4/2P1N3/8/8/8 b - -", 36, ["c4"; "d4"; "e4"]); //d5 - captures
            //yield ("8/8/8/3p4/2PPN3/8/8/8 b - -", 36, ["c4"; "e4"]); //d5 - captures
            //yield ("8/7p/7n/8/8/8/8/8 b - -", 48, List.empty<string>);  //h7, Nh6 blocking
        }
        |> Seq.map( fun (a,b,c) -> [|box a; box b; box c|])

    [<Theory; MemberData("TestCasesWhite")>]
    member _. ``verify moves of White Pawn (data bound)`` (fen:string, startBitRef:int, expectedSquaresList:string list) =
        verifyMoves (fen, startBitRef, expectedSquaresList)

    [<Theory; MemberData("TestCasesBlack")>]
    member _. ``verify moves of Black Pawn (data bound)`` (fen:string, startBitRef:int, expectedSquaresList:string list) =
        verifyMoves (fen, startBitRef, expectedSquaresList)

    
    [<Fact>]
    member _. ``perft 3 level issue repro`` () =
        let fen = "rnbqkb1r/pppppppp/7n/8/8/7N/PPPPPPPP/RNBQKB1R w KQkq -"
        let pos = FenParsing.parseToPosition fen
        let srcBitRefs = pos |> Positions.getBitboardForSideToPlay |> BitUtils.getSetBits
        let pseudoMovesForSide = srcBitRefs |> Array.map (MoveGenerationLookupFunctions.generatePseudoMovesFullInfo lookups pos)
        let allPseudoMovesForSide = pseudoMovesForSide |> Array.collect id
        let pseudoMovesCount = allPseudoMovesForSide |> Array.length
        allPseudoMovesForSide |> Array.iter (Moves.toCoordinateNotation >> printfn "%s" )
        test <@ 20 = pseudoMovesCount @>
   
    //TODO - en passant