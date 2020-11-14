namespace FulcrumChess.Engine
open System.Text.RegularExpressions

module Uci =

    let (|InterpretedMatch|_|) pattern input =
        if isNull input then None
        else
            let m = Regex.Match(input, pattern)
            if m.Success then Some [for x in m.Groups -> x]
            else None

    //Match the pattern using a cached compiled Regex
    let (|CompiledMatch|_|) pattern input =
        if isNull input then None
        else
            let m = Regex.Match(input, pattern, RegexOptions.Compiled)
            if m.Success then Some [for x in m.Groups -> x]
            else None

    let parseCommand input =
        match input with
        | CompiledMatch "uci" [] -> Commands.Uci
        | CompiledMatch "isready" [] -> Commands.IsReady
        | CompiledMatch "position fen (.+)( moves (.+))?" [fen; movesChain] ->
            printfn "Got this: %s along with %s" fen.Value movesChain.Value
            let moves = movesChain.Value.Split(' ')
            Commands.SetPosition (fen.Value, moves)
        | CompiledMatch "go perft (\\d+)" [depth] -> Commands.Perft (System.Int32.Parse(depth.Value))
        | s -> Commands.Uknown s

    