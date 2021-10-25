namespace FulcrumChess.Engine.Benchmarks
open System
open BenchmarkDotNet.Running

module Program =
    [<EntryPoint>]
    let main argv =
        let summary = BenchmarkSwitcher.FromAssembly(typeof<PerftSuite>.Assembly).Run(argv)
        //BenchmarkRunner.Run<FulcrumChess.Engine.Benchmarks.PerftSuite>() |> ignore
        Console.WriteLine(summary)
        0