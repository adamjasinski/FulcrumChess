module Moves
open FenParser

type Move = { Piece:byte; From:byte; To:byte}

type Position = {Board:Board64.T; (* other fields *) }
//module Generator =
//    let generateAllMovesForWhiteKnight (board:byte[]) (fromSquareIndex:byte) =
//        let typedBoard  = Board64.create board
//        //TODO - find bitboard for white knight on fromSquareIndex
//        //let kbb = knightRays.[fromSquareIndex];
//        let kbb = 0L;
//        let allWhitePositions = typedBoard |> Board64.getAllWhitePiecesBitboard
//        let allWhitePositionsNegated = ~~~allWhitePositions
//        let hitbb = kbb &&& allWhitePositionsNegated
//        //TODO - convert bitboard to algebraic
//
//
//type AllMoves = ResizeArray<Move>

module RayDictionary =
    type T = System.Int64[]

type BoardFuncts = 
    {
        Move:Direction->int->int; //or Direction->int->int option
        IsOnBoard:int->bool;
        MakeBitboard:int[]->int64
    }

module BitboardGenerator =
    let generateRayBitboardsForSlidingPieceInDirection (funs:BoardFuncts) sourceSquareIndex dir = 
                 Seq.unfold (fun src ->
                    if src |> funs.IsOnBoard |> not then None
                    else Some (src, (funs.Move dir src))) sourceSquareIndex
                |> Array.ofSeq
                |> funs.MakeBitboard

    let generateRayBitboardsForSlidingPiece directions (funs:BoardFuncts) sourceSquareIndex  =
        directions 
        |> List.map (generateRayBitboardsForSlidingPieceInDirection funs sourceSquareIndex)
        |> List.filter ( fun b -> b <> 0L)
        |> Array.ofList

    let generateRookRayBitboards  =  [N;E;S;W] |> generateRayBitboardsForSlidingPiece 
    let generateBishopRayBitboards  =  [NE;SE;SW;NW] |> generateRayBitboardsForSlidingPiece 
    let generateQueenRayBitboards  =  [N;NE;E;SE;S;SW;W;NW] |> generateRayBitboardsForSlidingPiece 