module Perft
open FulcrumChess.Engine
open Bitboards
open Position

/// Performance test/move path enumerator
let rec perft (lookups:MoveGenerationLookups) (srcMove:Move, pos:Position) (depth:int, totalDepth:int) =
    let srcBitRefs = pos |> getBitboardForSideToPlay |> BitUtils.getSetBits
    let pseudoMovesForSide = srcBitRefs |> Array.Parallel.map (MoveGenerationLookupFunctions.generatePseudoMoves lookups pos)
    if(depth = 0) then
        [||]
    else
        let allPseudoMovesForSide = pseudoMovesForSide |> Array.collect id
        //printfn "Depth %d: %A" depth (allPseudoMovesForSide |> Array.map Moves.toAlgebraicNotation)
        let generateAttacks = MoveGenerationLookupFunctions.generateAllPseudoMovesForSide lookups
    
        let nextValidatedPositions =
            allPseudoMovesForSide 
            |> Array.Parallel.map ( fun move ->
                let pos' = pos |> Position.tryMakeMoveInternal generateAttacks move
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

type PerftDivideReport = { InitialMovesNodeBreakdown:(string*uint64) array; TotalNodes: uint64}

let createPerfDivideReport (movesNodeBreakdown:(Move*uint64) array) =
    let moveAlgebraicComparerForNiceOutput (move1:Move) (move2:Move) =
        let bitRef1 = move1 |> Move.getDestBitRef
        let bitRef2 = move2 |> Move.getDestBitRef
        bitRef2 - bitRef1
        // let sgnDiff = Math.Sign ( (bitRef1 % 8) - (bitRef2 % 8))
        // match sgnDiff with
        // | 1 -> 1
        // | -1 -> -1
        // | _ -> Math.Sign ( bitRef1 - bitRef2)
    let movesNodeBreakdownAlgebraicCoordinates:(string*uint64) array =
        movesNodeBreakdown
        |> Array.sortWith( fun (move1,_) (move2,_) ->
            moveAlgebraicComparerForNiceOutput move1 move2
        )
        |> Array.map (Tuple2.mapFirst Notation.toAlgebraicNotation)

    let totalNodesCount = movesNodeBreakdown |> Array.sumBy snd

    {PerftDivideReport.InitialMovesNodeBreakdown = movesNodeBreakdownAlgebraicCoordinates; TotalNodes = totalNodesCount}

let generatePerftReport (lookups:MoveGenerationLookups) (srcMove:Move, pos:Position) (depth:int, totalDepth:int) =
    perft lookups (srcMove,pos) (depth, totalDepth)
    |> createPerfDivideReport