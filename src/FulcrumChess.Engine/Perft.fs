module Perft
open FulcrumChess.Engine
open Bitboards
open Positions

/// Performance test/move path enumerator
let rec perft (lookups:MoveGenerationLookups) (srcMove:Move, pos:Position) (depth:int, totalDepth:int) =
    let srcBitRefs = pos |> getBitboardForSideToPlay |> BitUtils.getSetBits
    let pseudoMovesForSide = srcBitRefs |> Array.Parallel.map (MoveGenerationLookupFunctions.generatePseudoMovesFullInfo lookups pos)
    if(depth = 0) then
        Array.empty
    else
        let allPseudoMovesForSide = pseudoMovesForSide |> Array.collect id
        //printfn "Depth %d: %A" depth (allPseudoMovesForSide |> Array.map Moves.toAlgebraicNotation)
        let generateAttacks = MoveGenerationLookupFunctions.generateAllPseudoMovesForSide lookups
    
        let nextValidatedPositions =
            allPseudoMovesForSide 
            |> Array.Parallel.map ( fun move ->
                let pos' = pos |> Positions.makeMoveWithValidation generateAttacks move
                (move, pos')
            )
            |> Array.where (snd >> Option.isSome)
            |> Array.Parallel.map (fun (m, p) -> (m, Option.get p))

            
        let stats = 
            if depth < totalDepth then
                nextValidatedPositions 
                |> Array.map ( fun mp -> 
                    let nextLevels = perft lookups mp (depth+1, totalDepth)
                    ((fst mp), (nextLevels |> Array.sumBy snd |> uint64))
                )
            else
                nextValidatedPositions |> Array.map ( fun mp -> (fst mp, 1UL))

        stats