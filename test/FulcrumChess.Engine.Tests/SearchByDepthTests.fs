namespace FulcrumChess.Engine.Tests
open FulcrumChess.Engine
open FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open Swensen.Unquote

type SearchByDepthTests(magicGenerationSetupFixture:MagicGenerationSetupFixture) =

    let lookups = magicGenerationSetupFixture.Lookups

    let generateAttacks = Bitboards.MoveGenerationLookupFunctions.generateAttacks lookups
    let generatePseudoMoves = Bitboards.MoveGenerationLookupFunctions.generatePseudoMoves lookups
    //let generatePseudoMoves = Bitboards.MoveGenerationLookupFunctions.generatePseudoMoves lookups
    let tryMakeMoveInternal = Position.tryMakeMoveInternal generateAttacks
    let generateAllPseudoMovesForSide = Bitboards.MoveGenerationLookupFunctions.generateAllPseudoMovesForSide lookups
    let generateLegalMoves = Bitboards.MoveGenerationLookupFunctions.generateLegalMoves lookups

    let searchByDepth pos depth = 
        Search.goSearch pos generateLegalMoves tryMakeMoveInternal depth

    interface IAssemblyFixture<MagicGenerationSetupFixture>

    [<Theory(Skip="TODO not working yet")>]
    [<Category("Search")>]
    //[<InlineDataEx("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "e2e4", 1)>]
    [<InlineDataEx("1n2k2r/1p4pp/2q5/6N1/8/8/PPP2PB1/2K4R w - - 0 1", "g2c6", 1)>]
    // [<InlineDataEx("1r2k1r1/pbppRp1p/1bn2P2/8/Q7/B1PB1q2/P4PPP/3R2K1 b - - 0 20", true, false)>] //From Evergreen Game
    // [<InlineDataEx("1r3kr1/pbpBBp1p/1b3P2/8/8/2P2q2/P4PPP/3R2K1 b - - 0 24", true, true)>]       //From Evergreen Game
    // [<InlineDataEx("rnb1k1nr/p2p1ppp/3B4/1p1NPN1P/6P1/3P1Q2/P1P5/q4Kb1 w kq - 0 20", true, false)>]  //From Immortal Game 
    // [<InlineDataEx("r1bk3r/p2pBpNp/n4n2/1p1NP2P/6P1/3P4/P1P1K3/q5b1 b - - 1 23", true, true)>]  //From Immortal Game 
    member __.``search for best moves``(fen:string, expectedBestMove:string, depth:int) = 
        let pos = FenParsing.parseToPosition fen

        let (score, bestMove) = searchByDepth pos depth
        let algBestMove = bestMove |> Notation.toAlgebraicNotation
        test <@ expectedBestMove = algBestMove @>
       

    
