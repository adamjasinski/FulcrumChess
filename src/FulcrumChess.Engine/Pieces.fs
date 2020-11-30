namespace FulcrumChess.Engine

type SquareFenValue = |Fen of char|Blank|Invalid

type Chessmen = |Rook|Knight|Bishop|Queen|King|Pawn

type SlidingPiece = |Rook|Bishop //an 'elementary' sliding piece (Queen not included)

module Pieces =
    let numberAsFenCharacter (num:byte) =
        match num with
        | 0uy -> Blank
        | n when (num >= 65uy && num <= 90uy) || (num >= 97uy && num <= 122uy)
            -> Fen <| (char)num
        | _ -> Invalid

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


module PieceFenLetters =
    [<Literal>]
    let WhiteKing = 'K'
    [<Literal>]
    let WhiteQueen = 'Q'
    [<Literal>]
    let WhiteRook = 'R'
    [<Literal>]
    let WhiteBishop = 'B'
    [<Literal>]
    let WhiteKnight = 'N'
    [<Literal>]
    let WhitePawn = 'P'
    [<Literal>]
    let BlackKing = 'k'
    [<Literal>]
    let BlackQueen = 'q'
    [<Literal>]
    let BlackRook = 'r'
    [<Literal>]
    let BlackBishop = 'b'
    [<Literal>]
    let BlackKnight = 'n'
    [<Literal>]
    let BlackPawn = 'p'

    let getLetter (pc, side) =
        match (pc, side) with
        | (Chessmen.King, Side.White) -> WhiteKing
        | (Chessmen.Queen, Side.White) -> WhiteQueen
        | (Chessmen.Rook, Side.White) -> WhiteRook
        | (Chessmen.Bishop, Side.White) -> WhiteBishop
        | (Chessmen.Knight, Side.White) -> WhiteKnight
        | (Chessmen.Pawn, Side.White) -> WhitePawn
        | (Chessmen.King, Side.Black) -> BlackKing
        | (Chessmen.Queen, Side.Black) -> BlackQueen
        | (Chessmen.Rook, Side.Black) -> BlackRook
        | (Chessmen.Bishop, Side.Black) -> BlackBishop
        | (Chessmen.Knight, Side.Black) -> BlackKnight
        | (Chessmen.Pawn, Side.Black) -> BlackPawn

    let fromLetter (letter:char) =
        match letter with
            | 'p' -> Some(Chessmen.Pawn, Side.Black)
            | 'n' -> Some(Chessmen.Knight, Side.Black)
            | 'b' -> Some(Chessmen.Bishop, Side.Black)
            | 'r' -> Some(Chessmen.Rook, Side.Black)
            | 'q' -> Some(Chessmen.Queen, Side.Black)
            | 'k' -> Some(Chessmen.King, Side.Black)
            | 'P' -> Some(Chessmen.Pawn, Side.White)
            | 'N' -> Some(Chessmen.Knight, Side.White)
            | 'B' -> Some(Chessmen.Bishop, Side.White)
            | 'R' -> Some(Chessmen.Rook, Side.White)
            | 'Q' -> Some(Chessmen.Queen, Side.White)
            | 'K' -> Some(Chessmen.King, Side.White)
            | _ -> None