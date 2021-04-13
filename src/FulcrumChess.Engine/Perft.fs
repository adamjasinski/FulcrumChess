﻿namespace FulcrumChess.Engine

open FulcrumChess.Engine
open Bitboards
open Position

module PerftCache =
    open System.Collections.Generic
    //let cdict = new System.Collections.Concurrent.ConcurrentDictionary<(Side*Position),Move array>()
    let cacheAttacks = new Dictionary<(Side*Position),Bitboard>()
    let cachePseudoMoves = new Dictionary<(Side*Position),Move array>()
    //let bigLock = obj()

    let memoize (cache:Dictionary<'a,'b>) (f:'a->'b) =
        //let dict = new System.Collections.Generic.Dictionary<_,_>()
        fun n ->
            match cache.TryGetValue(n) with
            | (true, v) -> v
            | _ ->
                let temp = f(n)
                cache.Add(n, temp)
                temp

    // let memoize f =
    //     //let dict = new System.Collections.Generic.Dictionary<_,_>()
    //     fun n ->
    //         match dict1.TryGetValue(n) with
    //         | (true, v) -> v
    //         | _ ->
    //             let temp = f(n)
    //             dict1.Add(n, temp)
    //             temp

    // let memoizeThreadSafe (f:'a->'b) =
    //     let dict = new System.Collections.Concurrent.ConcurrentDictionary<'a,'b>()
    //     fun k ->
    //         dict.GetOrAdd(k, f)

    // let memoizeThreadSafeConcurrentDict (f:(Side*Position)->Move array) =
    //     //let dict = new System.Collections.Concurrent.ConcurrentDictionary<'a,'b>()
    //     fun k ->
    //         //cdict.GetOrAdd(k, f)

    // let memoizeThreadSafe (f:(Side*Position)->Move array) =
    //     //let dict = new System.Collections.Concurrent.ConcurrentDictionary<'a,'b>()
    //     fun n ->
    //         match dict.TryGetValue(n) with
    //         | (true, v) -> v
    //         | _ ->
    //             let temp = f(n)
    //             //lock bigLock (fun() -> dict.[n] <- temp)
    //             dict.[n] <- temp
    //             temp

module Perft =
    /// Performance test/move path enumerator
    let rec perft (lookups:MoveGenerationLookups) (srcMove:Move, pos:Position) (depth:int, totalDepth:int) =
        if(depth = 0) then
            [||]
        else
            //printfn "Depth %d: %A" depth (allPseudoMovesForSide |> Array.map Moves.toAlgebraicNotation)

            // let generateAttacks (s:Side) (p:Position) = 
            //     let gen = PerftCache.memoizeThreadSafe <| fun (s:Side,p:Position) ->
            //         MoveGenerationLookupFunctions.generateAllPseudoMovesForSide lookups s p
            //     gen (s,p)

            let generateAttacks (s:Side) (p:Position) =
                let gen = PerftCache.memoize PerftCache.cacheAttacks <| fun (s:Side,p:Position) ->
                    MoveGenerationLookupFunctions.generateAttacks lookups s p
                gen (s,p)

            let generateAllPseudoMovesForSide (s:Side) (p:Position) =
                let gen = PerftCache.memoize PerftCache.cachePseudoMoves <| fun (s:Side,p:Position) ->
                    MoveGenerationLookupFunctions.generateAllPseudoMovesForSide lookups s p |> Array.ofSeq
                gen (s,p)

            let allPseudoMovesForSide = pos |> MoveGenerationLookupFunctions.generateAllPseudoMovesForSide lookups pos.SideToPlay

            let nextValidatedPositions =
                allPseudoMovesForSide 
                |> Seq.map ( fun move ->
                    //printfn "Depth %d. Trying move #%d for %A: %s; History: %A" depth pos.FullMoveNumber pos.SideToPlay (move |> Notation.toAlgebraicNotation) (pos.History |> List.map(Notation.toAlgebraicNotation))
                    let pos' = pos |> Position.tryMakeMoveInternal generateAttacks move
                    pos' |> Option.map ( fun p -> (move, p))
                )
                |> Array.ofSeq
                |> Array.choose id

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