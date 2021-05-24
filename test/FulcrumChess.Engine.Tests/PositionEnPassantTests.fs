namespace FulcrumChess.Engine.Tests
open FulcrumChess.Engine
open FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open Swensen.Unquote

type PositionEnPassantTests(magicGenerationSetupFixture:MagicGenerationSetupFixture) =

    let lookups = magicGenerationSetupFixture.Lookups

    let generateAttacks = Bitboards.MoveGenerationLookupFunctions.generateAttacks lookups
    let generatePseudoMovesAdapter pos bitRef = Bitboards.MoveGenerationLookupFunctions.generatePseudoMovesWithSpecial lookups pos bitRef |> Seq.toArray

    interface IAssemblyFixture<MagicGenerationSetupFixture>

    [<Theory>]
    [<Category("EnPassant")>]
    [<InlineDataEx("r1bqkbnr/ppp1pppp/2n5/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3", "e5d6", "r1bqkbnr/ppp1pppp/2nP4/8/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 3")>]
    [<InlineDataEx("rnbqkbnr/pppp1ppp/8/8/4pP2/2NP4/PPP1P1PP/R1BQKBNR b KQkq f3 0 3", "e4f3", "rnbqkbnr/pppp1ppp/8/8/8/2NP1p2/PPP1P1PP/R1BQKBNR w KQkq - 0 4")>]
    [<InlineDataEx("r3k2r/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R b KQkq a3 0 1", "b4a3", "r3k2r/p1ppqpb1/bn2pnp1/3PN3/4P3/p1N2Q1p/1PPBBPPP/R3K2R w KQkq - 0 2")>]
    member __. ``make move - en passant`` (fen:string, pawnMoveAlgNotation:string, expectedFen:string) =
        let pos = FenParsing.parseToPosition fen

        let actualMove = UciMove.fromLongAlgebraicNotationToMove pos pawnMoveAlgNotation
        printfn "Gota moove: %d - %d" (actualMove |> Move.getDestBitRef) (actualMove |> Move.getSrcBitRef)

        let positionAfterMove = pos |> Position.tryMakeMoveWithFullValidation generatePseudoMovesAdapter generateAttacks actualMove
        test <@ positionAfterMove |> Option.isSome @>

        let posPrint = positionAfterMove.Value |> Position.prettyPrint
        printfn "%s" posPrint
        printfn "-------------------------"
        let actualFenAfterMove = positionAfterMove.Value |> FenParsing.toFen
        printfn "%s" actualFenAfterMove
        test <@ actualFenAfterMove = expectedFen @>

    [<Theory>]
    [<Category("EnPassant")>]
    [<InlineDataEx("r1bqkbnr/ppp1pppp/2n5/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 3", "e5d6")>]
    [<InlineDataEx("rnbqkbnr/pppp1ppp/8/8/4pP2/2NP4/PPP1P1PP/R1BQKBNR b KQkq - 0 3", "e4f3")>]
    member __. ``try to make move - en passant not allowed due to no en passant targets`` (fen:string, pawnMoveAlgNotation:string) =
        let pos = FenParsing.parseToPosition fen

        let actualMove = UciMove.fromLongAlgebraicNotationToMove pos pawnMoveAlgNotation
        printfn "Gota moove: %d - %d" (actualMove |> Move.getDestBitRef) (actualMove |> Move.getSrcBitRef)

        let positionAfterMove = pos |> Position.tryMakeMoveWithFullValidation generatePseudoMovesAdapter generateAttacks actualMove
        test <@ positionAfterMove |> Option.isNone @>

    [<Theory>]
    [<Category("EnPassant")>]
    [<InlineDataEx("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", "a2a4", "r3k2r/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R b KQkq a3 0 1")>]
    member __. ``try to make move - pawn double move should update en passant target`` (fen:string, pawnMoveAlgNotation:string, expectedFen:string) =
        let pos = FenParsing.parseToPosition fen

        let actualMove = UciMove.fromLongAlgebraicNotationToMove pos pawnMoveAlgNotation
        printfn "Gota moove: %d - %d" (actualMove |> Move.getDestBitRef) (actualMove |> Move.getSrcBitRef)

        let positionAfterMove = pos |> Position.tryMakeMoveWithFullValidation generatePseudoMovesAdapter generateAttacks actualMove
        test <@ positionAfterMove |> Option.isSome @>

        let posPrint = positionAfterMove.Value |> Position.prettyPrint
        printfn "%s" posPrint
        printfn "-------------------------"
        let actualFenAfterMove = positionAfterMove.Value |> FenParsing.toFen
        printfn "%s" actualFenAfterMove
        test <@ actualFenAfterMove = expectedFen @>