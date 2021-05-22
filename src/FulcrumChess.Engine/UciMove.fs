namespace FulcrumChess.Engine

// Represents a move sent via UCI protocol (e.g. chess GUI) to the engine
// As per UCI protocol: https://gist.github.com/aliostad/f4470274f39d29b788c1b09519e67372
//     The move format is in long algebraic notation.
//     A nullmove from the Engine to the GUI should be sent as 0000.
//     Examples:  e2e4, e7e5, e1g1 (white short castling), e7e8q (for promotion)

// type UciMove = { LongAlgebraic:string }

module UciMove =

    let private determineEnPassant (positionBeforeMove:Position) (srcBitRef, dstBitRef) =
        positionBeforeMove |> Some
        |> Option.filter (fun pos -> pos.EnPassantTarget > 0)
        |> Option.bind (Position.getChessmanAndSide srcBitRef)
        |> Option.map ( fun (chessman,_) -> 
            match chessman with
            | Pawn -> 
                positionBeforeMove.EnPassantTarget = dstBitRef
            | _ -> false)
        |> Option.exists ( (=) true)


    let fromLongAlgebraicNotationToMove (positionBeforeMove:Position) (moveLongAlgNotation:string) =
        let (srcBitRef, dstBitRef) = Notation.fromLongAlgebraicNotationToBitRefs moveLongAlgNotation
        let isEnPassant = determineEnPassant positionBeforeMove (srcBitRef, dstBitRef)
        let specialFlags = match isEnPassant with | true -> SpecialMoveType.EnPassant | false -> SpecialMoveType.Conventional

        // TODO - determine castling and promotion
        Move.createSpecial (srcBitRef, dstBitRef) specialFlags
