namespace FulcrumChess.Engine

// Represents a move sent via UCI protocol (e.g. chess GUI) to the engine
// As per UCI protocol: https://gist.github.com/aliostad/f4470274f39d29b788c1b09519e67372
//     The move format is in long algebraic notation.
//     A nullmove from the Engine to the GUI should be sent as 0000.
//     Examples:  e2e4, e7e5, e1g1 (white short castling), e7e8q (for promotion)

type UciMove = { LongAlgebraic:string }

module UciMove =
    let create (moveLongAlgNotation:string) =
        if moveLongAlgNotation |> Notation.validateLongAlgebraicNotation then 
            Some { UciMove.LongAlgebraic=moveLongAlgNotation }
        else None

    let createOrFail (moveLongAlgNotation:string) =
        if (moveLongAlgNotation |> Notation.validateLongAlgebraicNotation) then 
            { UciMove.LongAlgebraic=moveLongAlgNotation }
        else 
            failwithf "Invalid move from UCI input: %s" moveLongAlgNotation