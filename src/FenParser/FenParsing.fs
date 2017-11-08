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


let private parseSide (input:string) : Side =
    let parts = input.Split(' ')
    match parts.[1] with
    | "w" -> Side.White
    | "b" -> Side.Black
    | x -> invalidOp ("Unknown side to play in FEN: " + x)

//rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
let parseToBoard8x8 (fen:string) : Board8x8Array =
    let boardPart = fen.Substring(0, fen.IndexOf(" "))
    let rows = boardPart.Split([|'/'|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
    rows |> List.map parseSingleRow


let parseToPosition (fen:string) : Position =
    let board8x8 = parseToBoard8x8 fen
    let allPiecesOnBoard = board8x8 |> List.collect id |> Array.ofList |> Array.rev
    let sideToPlay = fen |> parseSide
    let startingPosition = { Positions.emptyBitboard with SideToPlay=sideToPlay }
    let mapped = 
        ((0,startingPosition), allPiecesOnBoard) 
        ||> Array.fold (fun (counter,pos) (piece:char) -> 
            match piece with
            | ' ' -> (counter+1,pos)
            | pc -> 
                let pos' = pos |> Positions.setFenPiece piece counter
                (counter+1, pos') 
        )
    mapped |> snd