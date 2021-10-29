namespace FulcrumChess.Engine.Tests
open FulcrumChess.Engine
open Swensen.Unquote

module PositionTestHelper =
    let generateAttacks = Bitboards.MoveGenerationLookupFunctions.generateAttacks 
    let generatePseudoMovesAdapter lookups pos bitRef = Bitboards.MoveGenerationLookupFunctions.generatePseudoMovesWithSpecial lookups pos bitRef |> Seq.toArray
    
    let verifyPositionAfterMoveWithFullValidation lookups fen moveAlgNotation expectedFen =
        let pos = FenParsing.parseToPosition fen

        let uciMove = UciMove.createOrFail moveAlgNotation

        let generateAttacks' = generateAttacks lookups
        let generatePseudoMovesAdapter' = generatePseudoMovesAdapter lookups
        let positionAfterMove = pos |> Position.tryMakeMoveWithFullValidation generatePseudoMovesAdapter' generateAttacks' uciMove
        test <@ positionAfterMove |> Option.isSome @>

        let posPrint = positionAfterMove.Value |> Position.prettyPrint
        printfn "%s" posPrint
        printfn "-------------------------"
        let actualFenAfterMove = positionAfterMove.Value |> FenParsing.toFen
        printfn "%s" actualFenAfterMove
        test <@ actualFenAfterMove = expectedFen @>

    let verifyPositionAfterIllegalMove lookups fen moveAlgNotation =
        let pos = FenParsing.parseToPosition fen

        let uciMove = UciMove.createOrFail moveAlgNotation

        let generateAttacks' = generateAttacks lookups
        let generatePseudoMovesAdapter' = generatePseudoMovesAdapter lookups
        let positionAfterMove = pos |> Position.tryMakeMoveWithFullValidation generatePseudoMovesAdapter' generateAttacks' uciMove
        test <@ positionAfterMove |> Option.isNone @>