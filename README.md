## Project Description

FulcrumChess is a chess engine written in F#; still in an early phase of development.

## Local development

### Pre-requisites

1. .NET Core 3.1
2. (Optional) Fake (F# build for .NET Core)
`dotnet tool install fake-cli -g`
3. (Optional) .NET Core Trace
`dotnet tool install trace -g`

### Building

`dotnet build`
`dotnet run --project src/FulcrumChess.Engine`

or with Fake:
`dotnet fake build`
(also runs unit tests)

or with building native binaries:
`dotnet publish -c Release -r linux-x64 --project PublishReadyToRun:false`

### Running tests

```
cd test/FulcrumChess.Engine.Tests
dotnet test
```

OR:
`dotnet fake run build.fsx -t Test`

## Running the engine

Easy way, with compilation if needed:
`dotnet run -p src/FulcrumChess.Engine`
OR, if built already:
`./run.sh`

Run the binary from artifacts built through `dotnet publish` (see above):
`src/FulcrumChess.Engine/bin/Release/linux-x64/FulcrumChess.Engine`

## Running dedicated benchmarks suites
`dotnet run -c Release --project test/FulcrumChess.Engine.Benchmarks --filter *Suite*`
OR
`dotnet fake run build.fsx -t Benchmark`

## Ad-hoc perf testing 
### With .NET trace

`dotnet trace collect -p <PID> --format Speedscope`

### With .NET performance counters
Example:
`dotnet dotnet-counters collect --counters time-in-gc -p 8935`


## References

### Magic bitboards

- https://www.chessprogramming.org/Magic_Bitboards

### General UCI references:
- http://wbec-ridderkerk.nl/html/UCIProtocol.html
- https://www.shredderchess.com/download.html
- https://ucichessengine.wordpress.com/2011/03/16/description-of-uci-protocol/
- https://www.reddit.com/r/ComputerChess/comments/b6rdez/commandline_options_for_stockfish/

