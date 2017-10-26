module FenParsing
open FenParser

open System.Text.RegularExpressions
let (|FirstRegexGroup|_|) pattern input =
   let m = Regex.Match(input,pattern) 
   if (m.Success) then Some m.Groups.[1].Value else None  

let (|Digit|_|) (ch:char) =
   let mutable intvalue = 0
   if System.Int32.TryParse(string(ch), &intvalue) then Some(intvalue)
   else None

let parseSingleRow (input:string) : char list =
    if input.Length > 8 then invalidArg "input" "Input was longer than 8 characters"
    let singleRowChars = input.ToCharArray() |> List.ofArray
    let parseChar acc ch =
        match ch with
        | Digit num -> (List.replicate num ' ') @ acc 
        | letter -> letter :: acc
    let result  = singleRowChars |> List.fold parseChar []
    if result |> List.length <> 8 then invalidOp (sprintf "Input was supposed to yield 8 fields, but yielded %d" (List.length result))
    result |> List.rev

//rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
let parse (input:string) : Board8x8Array =
    let boardPart = input.Substring(0, input.IndexOf(" "))
    let rows = boardPart.Split([|'/'|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
    rows |> List.map parseSingleRow
