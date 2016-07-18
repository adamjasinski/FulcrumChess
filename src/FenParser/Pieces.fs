module Pieces

type SquareFenValue = |Fen of char|Blank|Invalid

let numberAsFenCharacter (num:byte) =
    match num with
    | 0uy -> Blank
    | n when (num >= 65uy && num <= 90uy) || (num >= 97uy && num <= 122uy)
        -> Fen <| (char)num
    | _ -> Invalid

//let numberAsFenCharacter (num:byte) =
//    let result:char = (char)num
//    result

let inline fenCharacterAsNumber (ch:char) =
    let result:byte = (byte)ch
    result

let isWhitePiece = function
    | num when num >= 65uy && num <= 90uy -> true
    | num when num >= 97uy && num <= 122uy -> false
    | _ -> invalidArg "value" "out of range"

let isBlackPiece = isWhitePiece >> not

let numberAsPieceCodeColorInsensitive (num:byte) =
   match numberAsFenCharacter num with
   | Fen ch -> ch |> System.Char.ToUpperInvariant |> fenCharacterAsNumber |> Some
   | Blank -> None
   | Invalid -> None