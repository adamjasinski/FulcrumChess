namespace FulcrumChess.Engine

type Commands = 
    | Uci
    | IsReady
    | UciNewGame
    | SetPosition of (string * string array)
    | Perft of int
    | Display
    | Uknown of string
    | Help
    | Quit

type EngineState() =
    let options = 
        { EngineOptions.RookMagicFilePath = "RookMagicNumbers.json"
          BishopMagicFilePath = "BishopMagicNumbers.json" }
    let lookups = lazy Bitboards.MoveGenerationLookupFunctions.bootstrapAll(Some options)
    //let mutable currentPosition:Position = Position.initialPosition
    let generatePseudoMovesAdapter pos bitRef = Bitboards.MoveGenerationLookupFunctions.generatePseudoMovesWithSpecial lookups.Value pos bitRef |> Seq.toArray
    //member __.IsInitialized = lookups.IsValueCreated
    member val CurrentPosition:Position = Position.initialPosition with get, set
    member val GenerateAttacks = Bitboards.MoveGenerationLookupFunctions.generateAttacks lookups.Value with get
    member __.EnsureReady() = lookups.Value |> ignore
    member __.EngineCpuArch = if System.Environment.Is64BitProcess then "x64" else "x86"
    member __.Lookups = lookups.Value
    member val GeneratePseudoMoves = generatePseudoMovesAdapter
    
type CommandResult = | Ok | ExitSignal

module TimeManager =
    let runTimedFun f =
        let stopwatch = System.Diagnostics.Stopwatch()
        stopwatch.Start()
        f()
        stopwatch.Stop()
        printfn "Elapsed: %A" (stopwatch.Elapsed)

module CommandHandler =

    let output = printfn
    let debugInfo = printfn
    //let debugInfo = output "info string %s"
    //let debugInfoError = output "info string Error: %s"

    let handle (state:EngineState) cmd =

        let handleSetPosition (fenPosition:string, movesAlgNotation:string array) =
            let position = 
                if fenPosition = FenParsing.InitialPositionFen then 
                    Position.initialPosition
                else fenPosition |> FenParsing.parseToPosition
            let makeMove pos moveAlg = 
                let move = UciMove.fromLongAlgebraicNotationToMove pos moveAlg
                let posOpt = pos |> Position.tryMakeMoveWithFullValidation state.GeneratePseudoMoves state.GenerateAttacks move
                match posOpt with
                | Some p -> p
                | None -> illegalMove moveAlg
            let position' = movesAlgNotation |> Array.fold makeMove position
            state.CurrentPosition <- position'

        let handlePerft totalDepth =
            debugInfo "Running perft for depth %d" totalDepth
            let perftDivideReport = 
                Perft.generatePerftReport state.Lookups (0us, state.CurrentPosition) (1, totalDepth)
            
            perftDivideReport.InitialMovesNodeBreakdown
            |> Array.iter( fun (move,count) -> output "%s: %d" move count)

            output "\n"
            output "Nodes searched: %d" perftDivideReport.TotalNodes

        match cmd with
        | Uci ->
            //output "id name %s %s by %s" EngineConstants.EngineName state.EngineCpuArch EngineConstants.AuthorName
            output "id name %s %s" EngineConstants.EngineName state.EngineCpuArch 
            output "id author %s" EngineConstants.AuthorName
            output "uciok"
            Ok
        | UciNewGame -> Ok //nothing to do yet - keep current position
        | IsReady -> 
            state.EnsureReady()
            output "readyok"
            Ok
        | SetPosition args -> 
            handleSetPosition args
            Ok
        | Perft depth -> 
            TimeManager.runTimedFun (fun () -> handlePerft depth)
            Ok
        | Display -> 
            state.CurrentPosition |> Position.prettyPrint |> output "%s"
            state.CurrentPosition |> FenParsing.toFen |> output "Fen: %s"
            state.CurrentPosition.HashKey |> output "Key: %X"
            Ok
        | Uknown s -> 
            output "Uknown command: %s" s
            Ok
        | Help ->
            output "Special commands:\nd\ngo perft [n]\nquit"
            Ok
        | Quit ->
            ExitSignal
        //| _  -> failwithf "Fatal error"