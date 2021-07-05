namespace FulcrumChess.Engine

// Represents a move sent via UCI protocol (e.g. chess GUI) to the engine
// As per UCI protocol: https://gist.github.com/aliostad/f4470274f39d29b788c1b09519e67372
//     The move format is in long algebraic notation.
//     A nullmove from the Engine to the GUI should be sent as 0000.
//     Examples:  e2e4, e7e5, e1g1 (white short castling), e7e8q (for promotion)

// type UciMove = { LongAlgebraic:string }

module UciMove =

    let private determineEnPassant (positionBeforeMove:Position) (srcBitRef, dstBitRef) =
        positionBeforeMove |> ValueSome
        |> ValueOption.filter (fun pos -> pos.EnPassantTarget > 0)
        |> ValueOption.bind (Position.getChessmanAndSide srcBitRef)
        |> ValueOption.map ( fun struct(chessman,side) ->
            match chessman with
            | Pawn -> 
                positionBeforeMove.EnPassantTarget = dstBitRef
            | _ -> false)
        |> ValueOption.exists ( (=) true)


    let fromLongAlgebraicNotationToMove (positionBeforeMove:Position) (moveLongAlgNotation:string) =
        let (srcBitRef, dstBitRef, promotionOpt) = Notation.fromLongAlgebraicNotationToBitRefs moveLongAlgNotation
        let isEnPassant = determineEnPassant positionBeforeMove (srcBitRef, dstBitRef)

        let specialFlags = 
            match (isEnPassant, promotionOpt) with
            | (true, _) -> SpecialMoveType.EnPassant
            | (_, Some promType) -> SpecialMoveType.Promotion promType
            | (_, _) -> SpecialMoveType.Conventional

        // TODO - determine castling
        Move.createSpecial (srcBitRef, dstBitRef) specialFlags
