namespace FulcrumChess.Engine

// Zobrist hashing
// See https://www.chessprogramming.org/Zobrist_Hashing
module Zobrist =
    open System
    open RandomExtensions

    let rnd = Random(70026072)
    let chessmenHashes = 
        [|
            for i in 0..12 -> 
               [|for j in 0..63 -> rnd.NextUInt64()|]
        |]

    let enPassantHashes = [| for i in 0..7 -> rnd.NextUInt64() |]
    let castlingRightsHashes = [| for i in 0..4 -> rnd.NextUInt64() |]
    let sideHash = rnd.NextUInt64()

    //let inline getEmptyFieldHash bitRef = chessmenHashes.[0].[bitRef]
    //let inline getEmptyFieldHash bitRef = 0UL

    let getChessmanHash (chessman, side) bitRef =
        let idx = 
            match (chessman, side) with
            | (Chessmen.King, Side.White) -> 1
            | (Chessmen.Queen, Side.White) -> 2
            | (Chessmen.Rook, Side.White) -> 3
            | (Chessmen.Bishop, Side.White) -> 4
            | (Chessmen.Knight, Side.White) -> 5
            | (Chessmen.Pawn, Side.White) -> 6
            | (Chessmen.King, Side.Black) -> 7
            | (Chessmen.Queen, Side.Black) -> 8
            | (Chessmen.Rook, Side.Black) -> 9
            | (Chessmen.Bishop, Side.Black) -> 10
            | (Chessmen.Knight, Side.Black) -> 11
            | (Chessmen.Pawn, Side.Black) -> 12
        chessmenHashes.[idx].[bitRef]

    let getEnPassantHash enPassantTargetBitRef =
        if enPassantTargetBitRef = 0 then 0UL
        else
            let fileIdx = enPassantTargetBitRef / 8 
            enPassantHashes.[fileIdx]

    let inline getCastlingRightsHash (castlingRights:CastlingRights) =
        castlingRightsHashes.[int(castlingRights)]

    let getSideToPlayHash = function
        | White -> 0UL
        | Black -> sideHash