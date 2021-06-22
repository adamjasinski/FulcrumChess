namespace FulcrumChess.Engine

open FulcrumChess.Engine
open Bitboards
open Position

module PerftCache =
    open System.Collections.Generic
    //let cdict = new System.Collections.Concurrent.ConcurrentDictionary<(Side*Position),Move array>()
    let cacheAttacks = new Dictionary<uint64, Bitboard>()
    let cachePseudoMoves = new Dictionary<uint64, Move[]>()
    //let bigLock = obj()

    let memoize (cache:Dictionary<uint64,'b>) (h:'a->uint64) (f:'a->'b) =
        //let dict = new System.Collections.Generic.Dictionary<_,_>()
        fun n ->
            let hash = h(n)
            match cache.TryGetValue(hash) with
            | (true, v) -> v
            | _ ->
                let temp = f(n)
                cache.Add(hash, temp)
                //if cache.Count % 10000 = 0 then printfn "Cache item count: %d" cache.Count
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
    let assertHashCalculation (move:Move) (originalPos:Position) (newPos:Position) = 
        let expected = newPos |> calculateZobristHash
        let actualViaIncremental = newPos.HashKey
        if expected <> actualViaIncremental then
            printfn "Warning: hash calculated via incremental updates doesn't match freshly calculated hash; expected: %d; actual: %d" expected actualViaIncremental
            printfn "FEN: %s" (FenParsing.toFen originalPos)
            printfn "Move: %s" (Notation.toAlgebraicNotation move)
            printfn "=============================================="
            failwith "Fatal. End of game!"
        else newPos

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
                let gen = PerftCache.memoize PerftCache.cacheAttacks (Position.calculateZobristHash) <| fun (p:Position) ->
                    MoveGenerationLookupFunctions.generateAttacks lookups s p
                gen p

            let generateAllPseudoMovesForSide (s:Side) (p:Position) =
                let gen = PerftCache.memoize PerftCache.cachePseudoMoves (Position.calculateZobristHash) <| fun (p:Position) ->
                    MoveGenerationLookupFunctions.generateAllPseudoMovesForSide lookups s p
                gen p

            let allPseudoMovesForSide = pos |> MoveGenerationLookupFunctions.generateAllPseudoMovesForSide lookups pos.SideToPlay

            let nextValidatedPositions =
                allPseudoMovesForSide 
                |> Array.map ( fun move ->
                    let pos' = pos |> Position.tryMakeMoveInternal generateAttacks move
                    pos'
                    |> Option.map (assertHashCalculation move pos)
                    |> Option.map ( fun p -> (move, p))
                )
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
            // |> Array.sortWith( fun (move1,_) (move2,_) ->
            //     moveAlgebraicComparerForNiceOutput move1 move2
            // )
            |> Array.map (Tuple2.mapFirst Notation.toAlgebraicNotation)

        let totalNodesCount = movesNodeBreakdown |> Array.sumBy snd

        {PerftDivideReport.InitialMovesNodeBreakdown = movesNodeBreakdownAlgebraicCoordinates; TotalNodes = totalNodesCount}

    let generatePerftReport (lookups:MoveGenerationLookups) (srcMove:Move, pos:Position) (depth:int, totalDepth:int) =
        perft lookups (srcMove,pos) (depth, totalDepth)
        |> createPerfDivideReport