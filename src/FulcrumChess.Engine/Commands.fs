namespace FulcrumChess.Engine

type Commands = 
    | SetPosition of (string * string array)
    | Perft of int


module CommandHandler =
    let handleSetPosition (fenPosition:string) (movesAlgNotation:string array) generateAttacks =
        let position = fenPosition |> FenParsing.parseToPosition
        let makeMove p moveAlg = 
            let move = Notation.fromLongAlgebraicNotationToMove moveAlg
            let p' = p|> Position.makeMoveWithValidation generateAttacks move
            match p' with
            | Some p'' -> p''
            | None -> illegalMove moveAlg
        movesAlgNotation |> Array.fold makeMove position

module Uci =
    let output = printfn