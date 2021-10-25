namespace FulcrumChess.Engine.Benchmarks

open System
open BenchmarkDotNet.Attributes
open FulcrumChess.Engine
open BenchmarkDotNet.Running

//[<SimpleJob(launchCount=3; warmupCount=5; targetCount=5; invocationCount=5)>]
[<SimpleJob(1, 1, 1, 1, null, false)>]
[<MemoryDiagnoser>]
type PerftSuite() =

    [<Benchmark>]
    member this.PerftInitialPosLevel5() = () //TODO 
        // // let s = "sdgsdgdgdgsg"
        // // let s' = s.Substring(0, 4)
        // // if s'.Length > 10 then raise (System.InvalidOperationException())

        // //Bitboards.MoveGenerationLookupFunctions.bootstrapAll None |> ignore

        // // let pc = SlidingPiece.Rook
        // // let occupancyMask = Bitboards.getOccupancyMask pc
        // // let occupancyVariations = occupancyMask  |>  Bitboards.generateOccupancyVariations
        // // let attackSets = Bitboards.generateAttackSets pc occupancyVariations occupancyMask |> Array.ofSeq
        // // attackSets |> ignore
        // Bitboards.bootstrapMagicNumberGeneration SlidingPiece.Rook |> ignore