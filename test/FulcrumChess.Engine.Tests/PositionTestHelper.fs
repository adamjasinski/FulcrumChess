namespace FulcrumChess.Engine.Tests
open FulcrumChess.Engine
open Swensen.Unquote

module PositionTestHelper =
    let private generateAttacks = Bitboards.MoveGenerationLookupFunctions.generateAttacks 
    let private generatePseudoMovesAdapter lookups pos bitRef = Bitboards.MoveGenerationLookupFunctions.generatePseudoMovesWithSpecial lookups pos bitRef |> Seq.toArray
    
    let verifyPositionAfterMoveWithFullValidation lookups fen moveAlgNotation expectedFen =
        let pos = FenParsing.parseToPosition fen

        let actualMove = UciMove.fromLongAlgebraicNotationToMove pos moveAlgNotation
        printfn "Gota moove: %d <- %d" (actualMove |> Move.getDestBitRef) (actualMove |> Move.getSrcBitRef)

        let generateAttacks' = generateAttacks lookups
        let generatePseudoMovesAdapter' = generatePseudoMovesAdapter lookups
        let positionAfterMove = pos |> Position.tryMakeMoveWithFullValidation generatePseudoMovesAdapter' generateAttacks' actualMove
        test <@ positionAfterMove |> Option.isSome @>

        let posPrint = positionAfterMove.Value |> Position.prettyPrint
        printfn "%s" posPrint
        printfn "-------------------------"
        let actualFenAfterMove = positionAfterMove.Value |> FenParsing.toFen
        printfn "%s" actualFenAfterMove
        test <@ actualFenAfterMove = expectedFen @>

    let verifyPositionAfterIllegalMove lookups fen moveAlgNotation =
        let pos = FenParsing.parseToPosition fen

        let actualMove = UciMove.fromLongAlgebraicNotationToMove pos moveAlgNotation
        printfn "Gota moove: %d - %d" (actualMove |> Move.getDestBitRef) (actualMove |> Move.getSrcBitRef)

        let generateAttacks' = generateAttacks lookups
        let generatePseudoMovesAdapter' = generatePseudoMovesAdapter lookups
        let positionAfterMove = pos |> Position.tryMakeMoveWithFullValidation generatePseudoMovesAdapter' generateAttacks' actualMove
        test <@ positionAfterMove |> Option.isNone @>