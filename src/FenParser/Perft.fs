module Perft
open FenParser
open Bitboards
open Positions

/// Performance test/move path enumerator
let rec perft (lookups:MoveGenerationLookups) (pos:Position) (depth:int) =
    let srcBitRefs = pos |> getBitboardForSideToPlay |> BitUtils.getSetBits
    let pseudoMovesForSide = srcBitRefs |> Array.map (MoveGenerationLookupFunctions.generatePseudoMovesFullInfo lookups pos)
    let pseudoMovesCount = pseudoMovesForSide |> Array.length
    if(depth = 0) then
        1UL
    else
        let allPseudoMovesForSide = pseudoMovesForSide |> Array.collect id
    
        let nextValidatedPositions =
            allPseudoMovesForSide 
            |> Array.map ( fun move ->
                let pos' = pos |> Positions.makeMoveWithValidation move
                pos'
            )
            |> Array.where Option.isSome
            |> Array.map Option.get

        let currentDepthMoveCount = nextValidatedPositions |> Array.length |> uint64
        //printfn "Perft Depth %d: %d" depth currentDepthMoveCount
        //currentDepthMoveCount + (perft lookups )
        if depth > 1 then
            nextValidatedPositions
            |> Array.fold (fun acc p -> 
                let pn = perft lookups p (depth-1)
                acc + uint64(pn)     
            ) 0UL
        else
            currentDepthMoveCount

        //currentDepthMoveCount + getDeptMoveCountForNextDepth()
        //getDeptMoveCountForNextDepth()