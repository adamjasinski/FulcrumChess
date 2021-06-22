namespace FulcrumChess.Engine.Tests
open FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open FulcrumChess.Engine.Tests.PositionTestHelper
open FulcrumChess.Engine
open Swensen.Unquote.Assertions

type PositionPromotionTests(magicGenerationSetupFixture:MagicGenerationSetupFixture) =

    let lookups = magicGenerationSetupFixture.Lookups

    interface IAssemblyFixture<MagicGenerationSetupFixture>

    [<Theory>]
    [<Category("Promotion")>]
    [<InlineDataEx("4k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", "c7c8q", "2Q1k3/8/8/8/6p1/8/8/4K3 b - - 0 1")>] //standard promotion
    [<InlineDataEx("4k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", "c7c8r", "2R1k3/8/8/8/6p1/8/8/4K3 b - - 0 1")>] //underpromotion
    [<InlineDataEx("4k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", "c7c8b", "2B1k3/8/8/8/6p1/8/8/4K3 b - - 0 1")>] //underpromotion
    [<InlineDataEx("4k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", "c7c8n", "2N1k3/8/8/8/6p1/8/8/4K3 b - - 0 1")>] //underpromotion
    [<InlineDataEx("1n2k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", "c7b8q", "1Q2k3/8/8/8/6p1/8/8/4K3 b - - 0 1")>] //promotion and capture
    [<InlineDataEx("4k3/8/8/6P1/8/8/4p3/7K b - - 0 1", "e2e1q", "4k3/8/8/6P1/8/8/8/4q2K w - - 0 2")>] //standard promotion
    [<InlineDataEx("4k3/8/8/6P1/8/8/4p3/7K b - - 0 1", "e2e1r", "4k3/8/8/6P1/8/8/8/4r2K w - - 0 2")>] //underpromotion
    [<InlineDataEx("4k3/8/8/6P1/8/8/4p3/7K b - - 0 1", "e2e1b", "4k3/8/8/6P1/8/8/8/4b2K w - - 0 2")>] //underpromotion
    [<InlineDataEx("4k3/8/8/6P1/8/8/4p3/7K b - - 0 1", "e2e1n", "4k3/8/8/6P1/8/8/8/4n2K w - - 0 2")>] //underpromotion
    [<InlineDataEx("4k3/8/8/6P1/8/8/4p3/5R1K b - - 0 1", "e2f1q", "4k3/8/8/6P1/8/8/8/5q1K w - - 0 2")>] //promotion and capture
    member __. ``make move - promotion`` (fen:string, pawnMoveAlgNotation:string, expectedFen:string) =
        verifyPositionAfterMoveWithFullValidation lookups fen pawnMoveAlgNotation expectedFen

    //TODO - move to a separate test suite
    [<Theory>]
    [<Category("Zobrist2")>]
    [<InlineDataEx("r3k2r/p1ppqpb1/bn2pn2/3PN1p1/1p2P3/2N2Q1p/PPPBBPPP/R4K1R w kq - 0 2", "e5f7", "r3k2r/p1ppqpb1/bn2pn2/3PN1p1/1p2P3/2N2Q1p/PPPBBPPP/R4K1R w kq - 0 2")>]
    [<InlineDataEx("rnbqkbnr/ppppppp1/8/7p/P7/8/1PPPPPPP/RNBQKBNR w - - 0 2", "g1f3", "rnbqkbnr/ppppppp1/8/7p/P7/7N/1PPPPPPP/RNBQKB1R b - - 1 2")>]
    [<InlineDataEx("rnbqkbnr/pppppp1p/6p1/8/P7/8/1PPPPPPP/RNBQKBNR w - - 0 2", "f2f3", "rnbqkbnr/pppppp1p/6p1/8/P7/5P2/1PPPP1PP/RNBQKBNR b - - 0 2")>]
    [<InlineDataEx("r1bqkbnr/pp1ppppp/2n5/2p5/3PP3/5N2/PPP2PPP/RNBQKB1R b KQkq - 0 3", "a8b8", "1rbqkbnr/pp1ppppp/2n5/2p5/3PP3/5N2/PPP2PPP/RNBQKB1R w KQk - 1 4")>] //affecting castling rights
    [<InlineDataEx("rnbqkbnr/pp1ppppp/8/8/2p1P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 0 3", "d2d4", "rnbqkbnr/pp1ppppp/8/8/2pPP3/5N2/PPP2PPP/RNBQKB1R b KQkq d3 0 3")>] //adding enpassant rights
    [<InlineDataEx("rnbqkbnr/pp1ppppp/8/8/2pPP3/5N2/PPP2PPP/RNBQKB1R b KQkq d3 0 3", "b8c6", "r1bqkbnr/pp1ppppp/2n5/8/2pPP3/5N2/PPP2PPP/RNBQKB1R w KQkq - 1 4")>] //affecting enpassant rights
    [<InlineDataEx("r1bqkb1r/3ppppp/p1n2n2/1p6/2BNP3/2N5/PPP2PPP/R1BQK2R w KQkq - 0 7", "e1g1", "r1bqkb1r/3ppppp/p1n2n2/1p6/2BNP3/2N5/PPP2PPP/R1BQ1RK1 b kq - 1 7")>] //affecting castling rights by castling
    [<InlineDataEx("r1bqk2r/3p1ppp/p1nb1n2/1p2p3/2BNP3/1PN2Q2/P1P2PPP/R1B2RK1 b kq - 0 9", "e8g8", "r1bq1rk1/3p1ppp/p1nb1n2/1p2p3/2BNP3/1PN2Q2/P1P2PPP/R1B2RK1 w - - 1 10")>] //affecting castling rights by castling
    [<InlineDataEx("rnbqkbnr/ppppp1pp/5p2/8/8/1P6/P1PPPPPP/RNBQKBNR w - - 0 2", "c2c4", "rnbqkbnr/ppppp1pp/5p2/8/2P5/1P6/P2PPPPP/RNBQKBNR b - - 0 2")>] //simple position
    [<InlineDataEx("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1", "g1h3", "rnbqkbnr/pppppppp/8/8/8/7N/PPPPPPPP/RNBQKB1R b - - 1 1")>] //simple position
    member __.``make move with capture - verify zobrist hash`` (fen:string, moveAlgNotation:string, expectedFen:string) =
        //verifyPositionAfterMoveWithFullValidation lookups fen pawnMoveAlgNotation expectedFen
        printfn "Hello!"
        let pos = FenParsing.parseToPosition fen
        test <@ pos.HashKey = (pos |> Position.calculateZobristHash) @>

        let actualMove = UciMove.fromLongAlgebraicNotationToMove pos moveAlgNotation
        printfn "Gota moove: %d <- %d" (actualMove |> Move.getDestBitRef) (actualMove |> Move.getSrcBitRef)

        let generateAttacks' = generateAttacks lookups
        let generatePseudoMovesAdapter' = generatePseudoMovesAdapter lookups
        let positionAfterMove = pos |> Position.tryMakeMoveWithFullValidation generatePseudoMovesAdapter' generateAttacks' actualMove
        test <@ positionAfterMove |> Option.isSome @>

        let expectedHash = (positionAfterMove.Value |> Position.calculateZobristHash) 
        printfn "Expected hash: %d; actual hash: %d" expectedHash positionAfterMove.Value.HashKey
        test <@ positionAfterMove.Value.HashKey = expectedHash @>
// XORing with pc hash (Pawn, White) 29
// XORing with pc hash (Pawn, White) 13
// Warning: hash calculated via incremental updates doesn't match freshly calculated hash; expected: 17606663172825910538; actual: 9932027333174469576
// FEN: rnbqkbnr/ppppp1pp/5p2/8/8/1P6/P1PPPPPP/RNBQKBNR w - - 0 2
// Move: c2c4
// // ==============================================
// Starting test execution, please wait...
// A total of 1 test files matched the specified pattern.
// Warning: hash calculated via incremental updates doesn't match freshly calculated hash; expected: 5192241041181866007; actual: 3858533010864428757
// FEN: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1
// Move: g1h3
// ==============================================