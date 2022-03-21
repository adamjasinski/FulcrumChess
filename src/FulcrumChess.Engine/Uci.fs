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
            //printfn "inside regex"
            let m = Regex.Match(input, pattern, RegexOptions.Compiled)
            //printfn "Matched: %A" m.Success
            //printfn "Full info: %A" m
            if m.Success then Some [for x in m.Groups -> x]
            else None

    let parseCommand input =
        match input with
        | CompiledMatch @"^uci$" [_] -> Commands.Uci
        | CompiledMatch @"^isready$" [_] -> Commands.IsReady
        | CompiledMatch @"^ucinewgame$" [_] -> Commands.UciNewGame
        | CompiledMatch @"^position (fen (.+\s\d{1,2})|startpos)\s?(moves (.+))?$" [_; representation; fen; movesPresence; movesChain] ->
            //printfn "Got this: %s along with %s" fen.Value movesChain.Value
            let fenToUse = if representation.Value = "startpos" then FenParsing.InitialPositionFen else fen.Value
            let moves = if movesPresence.Value.StartsWith("moves") && not <| isNull movesChain.Value then movesChain.Value.Split(' ') else [||]
            Commands.SetPosition (fenToUse, moves)
        | CompiledMatch @"^go perft (\d+)$" [_; depth] -> Commands.Perft (System.Int32.Parse(depth.Value))
        | CompiledMatch @"^d$" [_] -> Commands.Display
        | CompiledMatch @"^eval$" [_] -> Commands.Eval
        | CompiledMatch @"^help$" [_] -> Commands.Help
        | CompiledMatch @"^quit$" [_] -> Commands.Quit
        | s -> Commands.Uknown s

    let mainLoop () =
        let state = EngineState()
        let rec loop() =
            let input = System.Console.ReadLine()
            let cmd = parseCommand input
            let cmdResult = CommandHandler.handle state cmd
            match cmdResult with
            | Ok -> loop()
            | ExitSignal -> ()
        loop()
    