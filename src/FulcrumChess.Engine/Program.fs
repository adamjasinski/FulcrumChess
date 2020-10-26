// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
namespace FulcrumChess.Engine
open System
open Pieces

module Program =
    let runTimedFun f =
        let stopwatch = System.Diagnostics.Stopwatch()
        stopwatch.Start()
        f()
        stopwatch.Stop()
        printfn "Elapsed: %A" (stopwatch.Elapsed)

    [<EntryPoint>]
    let main argv = 
        printfn "%A" argv
        printfn "64-bit process: %A " Environment.Is64BitProcess
      
        let lookups = lazy Bitboards.MoveGenerationLookupFunctions.bootstrapAll()
        
        runTimedFun <| fun () ->
            lookups.Value |> ignore
        printfn "Magic generation finished"

        let perftDepth = 3
        printfn "Running perft for depth %d" perftDepth
        //runTimedFun <| fun () ->
        let totalNodesCountWithCurrentBreakdown = Perft.perft lookups.Value (0us, Positions.initialPosition) (1, perftDepth)
        let totalNodesCount = totalNodesCountWithCurrentBreakdown |> Array.sumBy snd
        totalNodesCountWithCurrentBreakdown
        |> Array.sortWith( fun (move1,_) (move2,_) ->
            let bitRef1 = move1 |> Move.getDestBitRef
            let bitRef2 = move2 |> Move.getDestBitRef
            bitRef1 - bitRef2
            // let sgnDiff = Math.Sign ( (bitRef1 % 7) - (bitRef2 % 7))
            // match sgnDiff with
            // | 1 -> 1
            // | -1 -> -1
            // | _ -> Math.Sign ( bitRef1 - bitRef2)
        )
        |> Array.iter( fun (move,count) -> printfn "%s %d" (move |> Notation.toAlgebraicNotation ) count)
        printfn "========= Total nodes count: %d =============" totalNodesCount

        0 // return an integer exit code