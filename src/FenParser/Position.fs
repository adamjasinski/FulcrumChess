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