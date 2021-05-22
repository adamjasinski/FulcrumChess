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

    [<Category("EnPassant")>]
    [<InlineDataEx("r1bqkbnr/ppp1pppp/2n5/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 3", "e5d6")>]
    [<InlineDataEx("rnbqkbnr/pppp1ppp/8/8/4pP2/2NP4/PPP1P1PP/R1BQKBNR b KQkq - 0 3", "e4f3")>]
    member __. ``try to make move - en passant not allowed due to no en passant targets`` (fen:string, pawnMoveAlgNotation:string, expectedFen:string) =
        let pos = FenParsing.parseToPosition fen

        let actualMove = UciMove.fromLongAlgebraicNotationToMove pos pawnMoveAlgNotation
        printfn "Gota moove: %d - %d" (actualMove |> Move.getDestBitRef) (actualMove |> Move.getSrcBitRef)

        let positionAfterMove = pos |> Position.tryMakeMoveWithFullValidation generatePseudoMovesAdapter generateAttacks actualMove
        test <@ positionAfterMove |> Option.isSome @>

 
