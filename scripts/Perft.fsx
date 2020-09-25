#r "../src/FulcrumChess.Engine/bin/Debug/netcoreapp3.1/FulcrumChess.Engine.dll"
open FulcrumChess.Engine

printfn "Generating magic values"
let lookups = Bitboards.MoveGenerationLookupFunctions.bootstrapAll() 
printfn "Magic generation finished"

printfn "Running Perft mode"
//printfn "Please enter perft depth:"
//let perftDepthAsString = Console.ReadLine()
//let perftDepth = System.Int32.Parse(perftDepthAsString)
let perftDepth = 3
printfn "Running perft for depth %d" perftDepth
//runTimedFun <| fun () ->
let totalNodesCount = Perft.perft lookups (0us, Positions.initialPosition) (1, perftDepth)
printfn "========= Total nodes count: %d =============" totalNodesCount