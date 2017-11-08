namespace FenParser

type Board8x8Array = char list list

type Direction = |N|NE|E|SE|S|SW|W|NW

type Side = |White|Black

// Bit ordering: 0=h1 .. 63=a8;
// See also: LERBEF http://chessprogramming.wikispaces.com/file/view/lerbef.JPG/423298024/lerbef.JPG
type Bitboard = uint64

[<AutoOpen>]
module Common =
    let opposite = function
        | Side.White -> Side.Black
        | _ -> Side.White