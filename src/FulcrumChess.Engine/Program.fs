namespace FulcrumChess.Engine
open System

module Program =

    [<EntryPoint>]
    let main argv = 
        printfn "%A" argv
        printfn "64-bit process: %A " Environment.Is64BitProcess
      
        let lookups = lazy Bitboards.MoveGenerationLookupFunctions.bootstrapAll()
        
        TimeManager.runTimedFun <| fun () ->
            lookups.Value |> ignore
        printfn "Magic generation finished"

        printfn "%s by %s" EngineConstants.EngineName EngineConstants.AuthorName

        Uci.mainLoop()

        0 // return an integer exit code