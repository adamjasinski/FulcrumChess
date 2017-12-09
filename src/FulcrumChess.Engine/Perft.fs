module Perft
open FulcrumChess.Engine
open Bitboards
open Positions

/// Performance test/move path enumerator
let rec perft (lookups:MoveGenerationLookups) (srcMove:Move, pos:Position) (depth:int, totalDepth:int) =
    let srcBitRefs = pos |> getBitboardForSideToPlay |> BitUtils.getSetBits
    let pseudoMovesForSide = srcBitRefs |> Array.map (MoveGenerationLookupFunctions.generatePseudoMovesFullInfo lookups pos)
    if(depth = 0) then
        1UL
    else
        let allPseudoMovesForSide = pseudoMovesForSide |> Array.collect id
        //printfn "Depth %d: %A" depth (allPseudoMovesForSide |> Array.map Moves.toCoordinateNotation)
        let generateAttacks = MoveGenerationLookupFunctions.generateAllPseudoMovesForSide lookups
    
        let nextValidatedPositions =
            allPseudoMovesForSide 
            |> Array.map ( fun move ->
                let pos' = pos |> Positions.makeMoveWithValidation generateAttacks move
                (move, pos')
            )
            |> Array.where (snd >> Option.isSome)
            |> Array.map (fun (m, p) -> (m, Option.get p))

        let currentDepthMoveCount = nextValidatedPositions |> Array.length |> uint64
        let res =
            if depth < totalDepth then
                nextValidatedPositions
                |> Array.fold (fun acc mp -> 
                    let pn = perft lookups mp (depth+1,totalDepth)
                    acc + uint64(pn)     
                ) 0UL
            else
                currentDepthMoveCount
        
        if depth = totalDepth then
            printfn "Perft Depth \t\t\t%d: %A %d" (depth) (srcMove |> Moves.toCoordinateNotation) res
        else
            printfn "Perft Depth %d subtotal: %A %d" (depth+1) (srcMove |> Moves.toCoordinateNotation) res
        res
