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
        let perftDivideReport = 
            Perft.generatePerftReport lookups.Value (0us, Positions.initialPosition) (1, perftDepth)

        perftDivideReport.InitialMovesNodeBreakdown
        |> Array.iter( fun (move,count) -> printfn "%s: %d" move count)
        printf "\n"
        printfn "Nodes searched: %d" perftDivideReport.TotalNodes

        0 // return an integer exit code