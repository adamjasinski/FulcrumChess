module FenParsing
open FulcrumChess.Engine

open System
open System.Text
open System.Text.RegularExpressions

let [<Literal>] InitialPositionFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

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

let private parseCastlingRights (input:string) =
    let parts = input.Split(' ')
    let castlingRightsCharArray = parts.[2].ToCharArray()

    ((CastlingRights.None,CastlingRights.None), castlingRightsCharArray)
    ||> Array.fold(fun (whiteRights,blackRights) letter ->
        match letter with
        | 'K' -> ((whiteRights ||| CastlingRights.KingSide), blackRights)
        | 'Q' -> ((whiteRights ||| CastlingRights.QueenSide), blackRights)
        | 'k' -> (whiteRights, (blackRights ||| CastlingRights.KingSide))
        | 'q' -> (whiteRights, (blackRights ||| CastlingRights.QueenSide))
        | '-' -> (whiteRights, blackRights)
        | _ -> invalidOp(sprintf "Unknown castling right in FEN: %c" letter))

// fen example: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
let parseToBoard8x8 (fen:string) : Board8x8Array =
    let boardPart = fen.Substring(0, fen.IndexOf(" "))
    let rows = boardPart.Split([|'/'|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
    rows |> List.map parseSingleRow

let parseToPosition (fen:string) : Position =
    let board8x8 = parseToBoard8x8 fen
    let allPiecesOnBoard = board8x8 |> List.collect id |> Array.ofList |> Array.rev
    let sideToPlay = fen |> parseSide
    let (whiteCastlingRights,blackCastlingRights) = fen |> parseCastlingRights
    let startingPosition = 
        { Position.emptyBitboard with 
            SideToPlay=sideToPlay; 
            WhiteCastlingRights = whiteCastlingRights; 
            BlackCastlingRights = blackCastlingRights }
    let mapped = 
        ((0,startingPosition), allPiecesOnBoard) 
        ||> Array.fold (fun (counter,pos) (piece:char) -> 
            match piece with
            | ' ' -> (counter+1,pos)
            | pc -> 
                let pos' = pos |> Position.setFenPiece piece counter
                (counter+1, pos') 
        )
    mapped |> snd

let toFen (pos:Position) =
    let boardArray = Position.dumpPosition pos

    let allRowsArray = boardArray |> Array.map( fun rowArray ->
        let rec rowCombiner j (rowSb:StringBuilder) lastReadBlanks =
            if j < 8 then
                let c = rowArray.[j]

                if c = ' ' then 
                    rowCombiner (j + 1) rowSb (lastReadBlanks + 1)
                else
                    if lastReadBlanks > 0 then
                        rowSb.Append(lastReadBlanks.ToString()) |> ignore
                    rowSb.Append(c) |> ignore
                    rowCombiner (j + 1) rowSb 0
            else
                if lastReadBlanks > 0 then
                    rowSb.Append(lastReadBlanks.ToString())  |> ignore
                rowSb.ToString()

        let rowSb = StringBuilder()
        rowCombiner 0 rowSb 0
    )

    let sideToLetter = function
        | White -> 'w'
        | Black -> 'b'

    let castlingRightsToLetter (whiteCastlingRights,blackCastlingRights) =
        let white = 
            match whiteCastlingRights with
            | r when r = CastlingRights.Both -> "KQ"
            | r when r = CastlingRights.KingSide -> "K"
            | r when r = CastlingRights.QueenSide -> "Q"
            | _ -> ""

        let black = 
            match blackCastlingRights with
            | r when r = CastlingRights.Both -> "kq"
            | r when r = CastlingRights.KingSide -> "k"
            | r when r = CastlingRights.QueenSide -> "q"
            | _ -> ""

        if white.Length > 0 || black.Length > 0 then
            white + black
        else "-"

    let chessmen = String.Join('/', allRowsArray)
    let side = sideToLetter pos.SideToPlay
    let castlingRights = castlingRightsToLetter (pos.WhiteCastlingRights, pos.BlackCastlingRights)
    sprintf "%s %c %s" chessmen side castlingRights