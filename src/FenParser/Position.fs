module Positions

open Bitboards

type Position = 
    {
        WhiteKing:Bitboard;
        WhiteQueen:Bitboard;
        WhiteRooks:Bitboard;
        WhiteBishops:Bitboard;
        WhiteKnights:Bitboard;
        WhitePawns:Bitboard;
        BlackKing:Bitboard;
        BlackQueen:Bitboard;
        BlackRooks:Bitboard;
        BlackBishops:Bitboard;
        BlackKnights:Bitboard;
        BlackPawns:Bitboard;
    }

let whiteBitboard (pos:Position) =
    pos.WhiteKing ||| pos.WhiteQueen ||| pos.WhiteRooks ||| pos.WhiteBishops ||| pos.BlackKnights ||| pos.WhitePawns

let blackBitboard (pos:Position) =
    pos.BlackKing ||| pos.BlackQueen ||| pos.BlackRooks ||| pos.BlackBishops ||| pos.BlackKnights ||| pos.BlackPawns

//let fromFenString (fen:string) =
//    let board8x8 = FenParsing.parse fen