namespace FulcrumChess.Engine

type Commands = 
    | Uci
    | IsReady
    | SetPosition of (string * string array)
    | Perft of int
    | Uknown of string

module EngineConstants =
    [<Literal>] 
    let EngineName = "Fulcrum Chess 0.1"
    [<Literal>] 
    let AuthorName = "AJ"


type EngineState() =
    let lookups = lazy Bitboards.MoveGenerationLookupFunctions.bootstrapAll()
    //let mutable currentPosition:Position = Position.initialPosition

    //member __.IsInitialized = lookups.IsValueCreated
    member val CurrentPosition:Position = Position.initialPosition with get, set
    member val GenerateAttacks = Bitboards.MoveGenerationLookupFunctions.generateAllPseudoMovesForSide lookups.Value with get
    member __.EnsureReady() = lookups.Value |> ignore
    member __.EngineCpuArch = if System.Environment.Is64BitProcess then "x64" else "x86"
    member __.Lookups = lookups.Value

module CommandHandler =

    let output = printfn
    let debugInfo = printfn

    let handle (state:EngineState) cmd =

        let handleSetPosition (fenPosition:string, movesAlgNotation:string array) =
            let position = fenPosition |> FenParsing.parseToPosition
            let makeMove pos moveAlg = 
                let move = Notation.fromLongAlgebraicNotationToMove moveAlg
                let posOpt = pos |> Position.makeMoveWithValidation state.GenerateAttacks move
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

        match cmd with
        | Uci ->
            //output "id name %s %s by %s" EngineConstants.EngineName state.EngineCpuArch EngineConstants.AuthorName
            output "id name %s %s" EngineConstants.EngineName state.EngineCpuArch 
            output "id author %s" EngineConstants.AuthorName
            output "uciok"
        | IsReady -> 
            state.EnsureReady()
            output "readyok"
        | SetPosition args -> handleSetPosition args
        | Perft depth -> handlePerft depth
        | Uknown s -> output "Uknown command: %s" s
        //| _  -> failwithf "Fatal error"