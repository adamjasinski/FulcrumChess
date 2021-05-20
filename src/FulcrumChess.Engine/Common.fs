namespace FulcrumChess.Engine

type Board8x8Array = char list list

type Direction = |N|NE|E|SE|S|SW|W|NW

type Side = |White|Black

// Bit ordering: 0=h1 .. 63=a8;
// See also: LERBEF https://www.chessprogramming.org/File:Lerbef.JPG
type Bitboard = uint64

module Rows  =
    let [<Literal>] ThirdRow =  0xFF0000UL
    let [<Literal>] FourthRow = 0xFF000000UL
    let [<Literal>] FifthRow =  0xFF00000000UL
    let [<Literal>] SixthRow =  0xFF0000000000UL

[<System.FlagsAttribute>]
type CastlingRights = None = 0 | KingSide = 1 | QueenSide = 2 | Both = 3 

[<AutoOpen>]
module Common =
    let opposite = function
        | Side.White -> Side.Black
        | _ -> Side.White