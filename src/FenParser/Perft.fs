module Perft
//open FenParser
//open Bitboards
//open Positions

//let rec perft (lookups:MoveGenerationLookups) (pos:Position) (side:Side) (depth:int) =
    //let bitRefs = pos |> getBitboardForSide side |> BitUtils.getSetBits
    //let pseudoMovesForSide = bitRefs |> Array.map (MoveGenerationLookupFunctions.generatePseudoMoves lookups pos)
    //let pseudoMovesCount = pseudoMovesForSide |> Array.length
    //if(depth = 0) then
    //    pseudoMovesCount
    //else
        //let allPseudoMovesForSide = pseudoMovesForSide |> Array.collect BitUtils.getSetBits
        //allPseudoMovesForSide |> Array.map ( fun move ->
        //    Positions.makeMoveWithValidation 'x' move pos
        //)