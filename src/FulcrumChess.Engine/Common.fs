namespace FulcrumChess.Engine

type Board8x8Array = char list list

[<Struct>]
type Direction = |N|NE|E|SE|S|SW|W|NW

[<Struct>]
type Side = |White|Black

// Bit ordering: 0=h1 .. 63=a8;
// See also: LERBEF https://www.chessprogramming.org/File:Lerbef.JPG
type Bitboard = uint64

module Rows  =
    let [<Literal>] SecondRow = 0xFF00UL
    let [<Literal>] ThirdRow =  0xFF0000UL
    let [<Literal>] FourthRow = 0xFF000000UL
    let [<Literal>] FifthRow =  0xFF00000000UL
    let [<Literal>] SixthRow =  0xFF0000000000UL
    let [<Literal>] SeventhRow =  0xFF000000000000UL

[<System.FlagsAttribute>]
type CastlingRights = None = 0 | KingSide = 1 | QueenSide = 2 | Both = 3 

[<AutoOpen>]
module Common =
    let opposite = function
        | Side.White -> Side.Black
        | _ -> Side.White
    let inline getFileIndex bitRef = 7 - (bitRef % 8) //  = squareIndex & 7
    let inline getRankIndex bitRef = bitRef / 8  //= squareIndex >> 3 


module EngineConstants =
    [<Literal>] 
    let EngineName = "Fulcrum Chess 0.1"
    [<Literal>] 
    let AuthorName = "Adam Jasinski"
    [<Literal>]
    let UseFast32BitMultiplyingForHashing = 
    #if FAST_32BIT_MULT
        true
    #else
        false
    #endif

type EngineOptions = 
    { RookMagicFilePath:string
      BishopMagicFilePath:string }

type Position = {
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

    WhiteCastlingRights:CastlingRights;
    BlackCastlingRights:CastlingRights;
    SideToPlay:Side;
    EnPassantTarget:int;
    HalfMoveClock:int;
    FullMoveNumber:int;
    HashKey:uint64;
}

type CastlingLookup = {
    InitialPositionKing:Bitboard;
    InitialPositionKingsRook:Bitboard;
    InitialPositionQueensRook:Bitboard;
    BlockersKingsRook:Bitboard;
    BlockersQueensRook:Bitboard;
    PathNonUnderCheckKing:Bitboard;
    PathNonUnderCheckQueen:Bitboard;
    DestinationBitRefKingSideCastling:int;
    DestinationBitRefQueenSideCastling:int;
    DestinationKingSideCastling:Bitboard;
    DestinationQueenSideCastling:Bitboard;
}
