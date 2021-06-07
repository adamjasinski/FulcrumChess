namespace FulcrumChess.Engine.Tests
open FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open FulcrumChess.Engine.Tests.PositionTestHelper

type PositionEnPassantTests(magicGenerationSetupFixture:MagicGenerationSetupFixture) =

    let lookups = magicGenerationSetupFixture.Lookups

    interface IAssemblyFixture<MagicGenerationSetupFixture>

    [<Theory>]
    [<Category("EnPassant")>]
    [<InlineDataEx("r1bqkbnr/ppp1pppp/2n5/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3", "e5d6", "r1bqkbnr/ppp1pppp/2nP4/8/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 3")>]
    [<InlineDataEx("rnbqkbnr/pppp1ppp/8/8/4pP2/2NP4/PPP1P1PP/R1BQKBNR b KQkq f3 0 3", "e4f3", "rnbqkbnr/pppp1ppp/8/8/8/2NP1p2/PPP1P1PP/R1BQKBNR w KQkq - 0 4")>]
    [<InlineDataEx("r3k2r/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R b KQkq a3 0 1", "b4a3", "r3k2r/p1ppqpb1/bn2pnp1/3PN3/4P3/p1N2Q1p/1PPBBPPP/R3K2R w KQkq - 0 2")>]
    [<InlineDataEx("rnbqkb1r/ppp1ppp1/5n2/3p2Pp/3P4/8/PPP1PP1P/RNBQKBNR w KQkq h6 0 4", "g5h6", "rnbqkb1r/ppp1ppp1/5n1P/3p4/3P4/8/PPP1PP1P/RNBQKBNR b KQkq - 0 4")>]
    member __. ``make move - en passant`` (fen:string, pawnMoveAlgNotation:string, expectedFen:string) =
        verifyPositionAfterMoveWithFullValidation lookups fen pawnMoveAlgNotation expectedFen

    [<InlineDataEx("rnbqkb1r/ppp1ppp1/5n2/3p2Pp/3P4/8/PPP1PP1P/RNBQKBNR w KQkq h6 0 4", "g5g6", "rnbqkb1r/ppp1ppp1/5nP1/3p3p/3P4/8/PPP1PP1P/RNBQKBNR b KQkq - 0 4")>]
    [<InlineDataEx("rnbqkbnr/ppp1p1pp/5p2/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3", "e5f6", "rnbqkbnr/ppp1p1pp/5P2/3p4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 3")>]
     [<Category("EnPassant")>]
    member __. ``make move - not exercised en passant should clear en passant target`` (fen:string, pawnMoveAlgNotation:string, expectedFen:string) =
        verifyPositionAfterMoveWithFullValidation lookups fen pawnMoveAlgNotation expectedFen


    [<Theory>]
    [<Category("EnPassant")>]
    [<InlineDataEx("r1bqkbnr/ppp1pppp/2n5/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 3", "e5d6")>]
    [<InlineDataEx("rnbqkbnr/pppp1ppp/8/8/4pP2/2NP4/PPP1P1PP/R1BQKBNR b KQkq - 0 3", "e4f3")>]
    member __. ``try to make move - en passant not allowed due to no en passant targets`` (fen:string, pawnMoveAlgNotation:string) =
        verifyPositionAfterIllegalMove lookups fen pawnMoveAlgNotation

    [<Theory>]
    [<Category("EnPassant")>]
    [<InlineDataEx("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", "a2a4", "r3k2r/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R b KQkq a3 0 1")>]
    [<InlineDataEx("rnbqkbnr/pp1ppppp/8/8/2p1P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 0 3", "b2b4", "rnbqkbnr/pp1ppppp/8/8/1Pp1P3/5N2/P1PP1PPP/RNBQKB1R b KQkq b3 0 3")>]
    [<InlineDataEx("r1bqkbnr/ppp1pppp/2n5/1P1p4/3P4/8/P1P1PPPP/RNBQKBNR b KQkq - 0 3", "a7a5", "r1bqkbnr/1pp1pppp/2n5/pP1p4/3P4/8/P1P1PPPP/RNBQKBNR w KQkq a6 0 4")>]
    [<InlineDataEx("rnbqkb1r/ppp1pppp/5n2/3p2P1/3P4/8/PPP1PP1P/RNBQKBNR b KQkq - 0 3", "h7h5", "rnbqkb1r/ppp1ppp1/5n2/3p2Pp/3P4/8/PPP1PP1P/RNBQKBNR w KQkq h6 0 4")>]
    [<InlineDataEx("rnbqkbnr/pppppp1p/8/8/4P1p1/2N5/PPPP1PPP/R1BQKBNR w KQkq - 0 3", "h2h4", "rnbqkbnr/pppppp1p/8/8/4P1pP/2N5/PPPP1PP1/R1BQKBNR b KQkq h3 0 3")>]
    member __. ``make move - pawn double move should update en passant target if it can be caught in next move by opponent`` (fen:string, pawnMoveAlgNotation:string, expectedFen:string) =
        verifyPositionAfterMoveWithFullValidation lookups fen pawnMoveAlgNotation expectedFen

    [<Theory>]
    [<Category("EnPassant")>]
    [<InlineDataEx("rnbqkbnr/pp1ppppp/8/2p1P3/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2", "g7g5", "rnbqkbnr/pp1ppp1p/8/2p1P1p1/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 3")>]
    [<InlineDataEx("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "e2e4", "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")>]
    [<InlineDataEx("rnbqkb1r/pppppppp/5n2/4P3/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2", "c7c5", "rnbqkb1r/pp1ppppp/5n2/2p1P3/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 3")>]
    member __. ``make move - pawn double move should not update en passant target if no pawn can capture it`` (fen:string, pawnMoveAlgNotation:string, expectedFen:string) =
        verifyPositionAfterMoveWithFullValidation lookups fen pawnMoveAlgNotation expectedFen