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
`dotnet run -p src/FulcrumChess.Engine`

or with Fake:
`dotnet fake build`
(also runs unit tests)

or with building native binaries:
`dotnet publish -c Release -r linux-x64 -p PublishReadyToRun:false`

### Running tests

```
cd test/FulcrumChess.Engine.Tests
dotnet test
```

OR:
`dotnet fake run build.fsx -t Test`

## Running the engine

Easy way:
`dotnet run -p src/FulcrumChess.Engine`

Binaries built through `dotnet publish` (see above):
`src/FulcrumChess.Engine/bin/Release/linux-x64/FulcrumChess.Engine`

## References

### Magic bitboards

- https://www.chessprogramming.org/Magic_Bitboards

### General UCI references:
- http://wbec-ridderkerk.nl/html/UCIProtocol.html
- https://www.shredderchess.com/download.html
- https://ucichessengine.wordpress.com/2011/03/16/description-of-uci-protocol/
- https://www.reddit.com/r/ComputerChess/comments/b6rdez/commandline_options_for_stockfish/

