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

        printfn "%s by %s" EngineConstants.EngineName EngineConstants.AuthorName

        Uci.mainLoop()

        0 // return an integer exit code