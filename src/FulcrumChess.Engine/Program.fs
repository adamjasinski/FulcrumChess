// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
namespace FulcrumChess.Engine
open System
open Pieces

module Program =
    let runTimedFun f =
        let stopwatch = new System.Diagnostics.Stopwatch()
        stopwatch.Start()
        f()
        stopwatch.Stop()
        printfn "Elapsed: %A" (stopwatch.Elapsed)

    let timedRookMagicGeneration() =
        let startTime = DateTime.Now
        let magick = Bitboards.bootstrapMagicNumberGeneration SlidingPiece.Rook
        let endTime = DateTime.Now
        printfn "%A" magick
        printfn "Elapsed: %A" (endTime - startTime)


    [<EntryPoint>]
    let main argv = 
        printfn "%A" argv
        printfn "64-bit process: %A " Environment.Is64BitProcess
      
        let lookups = Bitboards.MoveGenerationLookupFunctions.bootstrapAll() 
        printfn "Magic generation finished"
        printfn "Running Perft mode"
        printfn "Please enter perft depth:"
        //let perftDepthAsString = Console.ReadLine()
        //let perftDepth = System.Int32.Parse(perftDepthAsString)
        let perftDepth = 2
        printfn "Running perft for depth %d" perftDepth
        runTimedFun <| fun () ->
            let totalNodesCount = Perft.perft lookups (0us, Positions.initialPosition) (1,perftDepth)
            printfn "========= Total nodes count: %d =============" totalNodesCount
        0 // return an integer exit code