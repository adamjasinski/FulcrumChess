namespace FulcrumChess.Engine.Tests
open FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open FulcrumChess.Engine.Tests.PositionTestHelper

type PositionCastlingTests(magicGenerationSetupFixture:MagicGenerationSetupFixture) =

    let lookups = magicGenerationSetupFixture.Lookups

    interface IAssemblyFixture<MagicGenerationSetupFixture>

    [<Theory>]
    [<Category("Castling")>]
    [<InlineDataEx("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1", "e1g1", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQ1RK1 b kq - 1 1")>]
    [<InlineDataEx("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R3KBNR w KQkq - 0 1", "e1c1", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/2KR1BNR b kq - 1 1")>]
    [<InlineDataEx("rnbqk2r/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1", "e8g8", "rnbq1rk1/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQ - 1 2")>]
    [<InlineDataEx("r3kbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1", "e8c8", "2kr1bnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQ - 1 2")>]
    member __. ``make move - castling`` (fen:string, kingMoveAlgNotation:string, expectedFen:string) =
        verifyPositionAfterMoveWithFullValidation lookups fen kingMoveAlgNotation expectedFen

    [<Theory>]
    [<Category("Castling")>]
    [<InlineDataEx("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w Qkq - 0 1", "e1g1")>]
    [<InlineDataEx("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R3KBNR w Kkq - 0 1", "e1c1")>]
    [<InlineDataEx("rnbqk2r/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQq - 0 1", "e8g8")>]
    [<InlineDataEx("r3kbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQk - 0 1", "e8c8")>]
    member __. ``attempt to castle - no castling rights`` (fen:string, kingMoveAlgNotation:string) =
        verifyPositionAfterIllegalMove lookups fen kingMoveAlgNotation

    [<Theory>]
    [<Category("Castling")>]
    [<InlineDataEx("rnb1kbnr/ppp1qppp/8/3p4/8/8/PPPP1PPP/RNBQK2R w KQkq - 0 1", "e1g1", "white kingside, origin under check")>]
    [<InlineDataEx("rn1qkbnr/p1pp1ppp/bp6/4p3/8/8/PPPP1PPP/RNBQK2R w KQkq - 0 1", "e1g1", "white kingside, path under check")>]
    [<InlineDataEx("rnbqk1nr/pppp1ppp/8/2b1p3/8/5P2/PPPP2PP/RNBQK2R w KQkq - 0 1", "e1g1", "white kingside, destination under check")>]
    [<InlineDataEx("rnbqk1nr/ppp2ppp/8/3pp3/1b2P3/8/PPP2PPP/R3KBNR w KQkq - 0 1", "e1c1", "white queenside, origin under check")>]
    [<InlineDataEx("rn1qkbnr/ppp2ppp/3p4/4p3/4P1b1/8/PPP2PPP/R3KBNR w KQkq - 0 1", "e1c1", "white queenside, path under check")>] 
    [<InlineDataEx("rnbqk1nr/ppp2ppp/8/3pp1b1/4P3/8/PPP2PPP/R3KBNR w KQkq - 0 1", "e1c1", "white queenside, destination under check")>]

    [<InlineDataEx("rnbqk2r/ppp2ppp/8/1B1p4/3P4/1P2P3/P1P2PPP/RNBQK1NR b KQkq - 0 1", "e8g8", "black kingside, origin under check")>]
    [<InlineDataEx("rnbqk2r/ppp2ppp/8/3p4/3P4/BP6/P1P1PPPP/RN1QKBNR b KQkq - 0 1", "e8g8", "black kingside, path under check")>]
    [<InlineDataEx("rnbqk2r/ppp3pp/5p2/4p3/2BP4/1P2P3/P1P2PPP/RNBQK1NR b KQkq - 0 1", "e8g8", "black kingside, destination under check")>]
    [<InlineDataEx("r3kbnr/pp3ppp/8/1B1p4/3P4/8/PPP2PPP/RNBQK1NR b KQkq - 0 1", "e8c8", "black queenside, origin under check")>]
    [<InlineDataEx("r3kbnr/ppp2ppp/8/3p2B1/3P4/8/PPP1PPPP/RN1QKBNR b KQkq - 0 1", "e8c8", "black queenside, path under check")>]
    [<InlineDataEx("r3kbnr/pp3ppp/8/3p1B2/3P4/8/PPP2PPP/RNBQK1NR b KQkq - 0 1", "e8c8", "black queenside, destination under check")>]
    member __. ``illegal move - castling under check`` (fen:string, kingMoveAlgNotation:string, description:string)=
        verifyPositionAfterIllegalMove lookups fen kingMoveAlgNotation

    [<Theory>]
    [<Category("Castling")>]
    [<InlineDataEx("rn1qkbnr/ppp2ppp/8/3ppb2/8/2P5/PP3PPP/R3KBNR w KQkq - 0 1", "e1c1", "rn1qkbnr/ppp2ppp/8/3ppb2/8/2P5/PP3PPP/2KR1BNR b kq - 1 1")>]
    [<InlineDataEx("r3kbnr/pp3ppp/2p5/3p4/3P1B2/8/PPP1PPPP/RN1QKBNR b KQkq - 0 1", "e8c8", "2kr1bnr/pp3ppp/2p5/3p4/3P1B2/8/PPP1PPPP/RN1QKBNR w KQ - 1 2")>]
    member __. ``make move - queen side castling with check next to rook (edge case)`` (fen:string, kingMoveAlgNotation:string, expectedFen:string) =
        verifyPositionAfterMoveWithFullValidation lookups fen kingMoveAlgNotation expectedFen

    [<Theory>]
    [<Category("Castling")>]
    [<InlineDataEx("rn1qkbnr/ppp2ppp/8/3ppb2/8/2P5/PP3PPP/R3KBNR w KQkq - 0 1", "e1c1", "rn1qkbnr/ppp2ppp/8/3ppb2/8/2P5/PP3PPP/2KR1BNR b kq - 1 1")>]
    [<InlineDataEx("r3kbnr/pp3ppp/2p5/3p4/3P1B2/8/PPP1PPPP/RN1QKBNR b KQkq - 0 1", "e8c8", "2kr1bnr/pp3ppp/2p5/3p4/3P1B2/8/PPP1PPPP/RN1QKBNR w KQ - 1 2")>]
    member __. ``moving rook should affect castling rights`` (fen:string, rookMoveAlgNotation:string, expectedFen:string) =
        verifyPositionAfterMoveWithFullValidation lookups fen rookMoveAlgNotation expectedFen

