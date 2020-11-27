namespace FulcrumChess.Engine.Tests
open FulcrumChess.Engine
open FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open Swensen.Unquote

type PositionCheckMateTests(magicGenerationSetupFixture:MagicGenerationSetupFixture) =

    let lookups = magicGenerationSetupFixture.Lookups

    let generateAttacks = Bitboards.MoveGenerationLookupFunctions.generateAttacks lookups

    interface IAssemblyFixture<MagicGenerationSetupFixture>

    [<Theory>]
    [<Category("Checkmate")>]
    [<InlineDataEx("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", false, false)>]
    [<InlineDataEx("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1", false, false)>]
    [<InlineDataEx("1r2k1r1/pbppRp1p/1bn2P2/8/Q7/B1PB1q2/P4PPP/3R2K1 b - - 0 20", true, false)>] //From Evergreen Game
    [<InlineDataEx("1r3kr1/pbpBBp1p/1b3P2/8/8/2P2q2/P4PPP/3R2K1 b - - 0 24", true, true)>]       //From Evergreen Game
    [<InlineDataEx("rnb1k1nr/p2p1ppp/3B4/1p1NPN1P/6P1/3P1Q2/P1P5/q4Kb1 w kq - 0 20", true, false)>]  //From Immortal Game 
    [<InlineDataEx("r1bk3r/p2pBpNp/n4n2/1p1NP2P/6P1/3P4/P1P1K3/q5b1 b - - 1 23", true, true)>]  //From Immortal Game 
    member __.Test1(fen:string, isCheckExpected:bool, isMateExpected:bool) = 
        let pos = FenParsing.parseToPosition fen
        let generateAllPseudoMovesForSide = Bitboards.MoveGenerationLookupFunctions.generateAllPseudoMovesForSide lookups pos.SideToPlay

        pos |> Position.prettyPrint |> printfn "%s"
        let isCheck =  pos |> Position.isCheck generateAttacks
        let isMate =  pos |> Position.isCheckMate generateAllPseudoMovesForSide generateAttacks
        
        test <@ isCheck = isCheckExpected @>
        test <@ isMate = isMateExpected @>