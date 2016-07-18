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
   
//let parseSingleRow (input:string) : char list =
//    if input.Length > 8 then invalidArg "input" "Input was longer than 8 characters"
//    let rec parseSingleRowInner (singleRow:char list) (acc:char list)  : char list =
//        match singleRow with
//        | [] -> acc
//        //| Digit num :: tail ->  acc |> (List.append  (List.replicate num ' ')  >> parseSingleRowInner tail )
//        | Digit num :: tail ->  acc |> List.append  (List.replicate num ' ')  |> parseSingleRowInner tail 
//        | letter :: tail ->  letter :: acc |> parseSingleRowInner tail 
//    let singleRow = input.ToCharArray() |> List.ofArray
//    let result = parseSingleRowInner singleRow [] |> List.rev
//    if result |> List.length <> 8 then invalidOp (sprintf "Input was supposed to yield 8 fields, but yielded %d" (List.length result))
//    result

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
    //parseSingleRow (input.ToCharArray() |> List.ofArray) []
    //[['a'; 'b']]



//let parseSingleRow (row:string) : char list =
//    let singleRow = row.ToCharArray() |> List.ofArray
//    let rec parseSingleRowInner (singleRow:char list) (acc:char list)  : char list =
//        match singleRow with
//        | [] -> acc
//        | head :: tail ->
//            match head with
//            | Digit num ->  List.append acc (List.replicate num ' ') |> parseSingleRowInner tail 
////            let ar = List.replicate num ' '
////            let b = List.append ar acc
//            | _ ->  head :: acc |> parseSingleRowInner tail 
//    parseSingleRowInner singleRow [] |> List.rev
