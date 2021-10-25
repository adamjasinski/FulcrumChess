namespace FulcrumChess.Engine.Benchmarks

open System
open BenchmarkDotNet.Attributes
open FulcrumChess.Engine
open BenchmarkDotNet.Running

[<SimpleJob(1, 1, 1, 1, null, false)>]
[<MemoryDiagnoser>]
type MagicSuite() =

    [<Benchmark>]
    member this.MagicGeneration() = 
        Bitboards.bootstrapMagicNumberGeneration SlidingPiece.Rook |> ignore