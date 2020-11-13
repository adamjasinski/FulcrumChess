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
let perftDivideReport = 
    Perft.generatePerftReport lookups (0us, Position.initialPosition) (1, perftDepth)

perftDivideReport.InitialMovesNodeBreakdown
|> Array.iter( fun (move,count) -> printfn "%s: %d" move count)
printf "\n"
printfn "Nodes searched: %d" perftDivideReport.TotalNodes