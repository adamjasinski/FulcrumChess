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

let private getNthSegmentOrNone (input:string) n =
    let parts = input.Split(' ')
    if parts.Length > n then Some parts.[n] else None

let private parseSide (input:string) : Side =
    let sideOption =  getNthSegmentOrNone input 1
    if sideOption.IsSome then
        match sideOption.Value with
        | "w" -> Side.White
        | "b" -> Side.Black
        | x -> invalidOp ("Unknown side to play in FEN: " + x)
    else invalidOp "Missing side in FEN"

let private parseCastlingRights (input:string) =
    getNthSegmentOrNone input 2
    |> Option.map(fun castlingRights ->
        let castlingRightsCharArray = castlingRights.ToCharArray()

        ((CastlingRights.None,CastlingRights.None), castlingRightsCharArray)
        ||> Array.fold(fun (whiteRights,blackRights) letter ->
            match letter with
            | 'K' -> ((whiteRights ||| CastlingRights.KingSide), blackRights)
            | 'Q' -> ((whiteRights ||| CastlingRights.QueenSide), blackRights)
            | 'k' -> (whiteRights, (blackRights ||| CastlingRights.KingSide))
            | 'q' -> (whiteRights, (blackRights ||| CastlingRights.QueenSide))
            | '-' -> (whiteRights, blackRights)
            | _ -> invalidOp(sprintf "Unknown castling right in FEN: %c" letter)))
    |> Option.defaultValue (CastlingRights.None,CastlingRights.None)

let private parseEnPassantTarget (input:string) =
    getNthSegmentOrNone input 3
    |> Option.map (fun target -> 
        match target with
        | t when t.Length = 2 -> Notation.fromSquareNotationToBitRef t
        | "-" -> 0
        | _ -> invalidOp(sprintf "Unknown en passant target in FEN: %s" target))
    |> Option.defaultValue 0

let private parseHalfMoveClock (input:string) =
    getNthSegmentOrNone input 4
    |> Option.map Int32.Parse
    |> Option.defaultValue 0

let private parseFullMoveNumber (input:string) =
    getNthSegmentOrNone input 5
    |> Option.map Int32.Parse
    |> Option.defaultValue 1

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
    let enPassantTarget = fen |> parseEnPassantTarget
    let halfMoveClock = fen |> parseHalfMoveClock
    let fullMoveNumber = fen |> parseFullMoveNumber
    let initialHashKey = 
        Zobrist.getCastlingRightsHash (whiteCastlingRights, blackCastlingRights) ^^^
        Zobrist.getEnPassantHash enPassantTarget ^^^
        Zobrist.getSideToPlayHash sideToPlay

    let startingPosition = 
        { Position.emptyBitboard with 
            SideToPlay=sideToPlay; 
            WhiteCastlingRights = whiteCastlingRights; 
            BlackCastlingRights = blackCastlingRights;
            EnPassantTarget = enPassantTarget;
            HalfMoveClock = halfMoveClock;
            FullMoveNumber = fullMoveNumber;
            HashKey = initialHashKey }
    let mapped = 
        ((0,startingPosition), allPiecesOnBoard) 
        ||> Array.fold (fun (counter,pos) (piece:char) -> 
            match piece with
            | ' ' -> (counter+1,pos)
            | pc -> 
                let pos' = pos |> Position.setFenPiece pc counter
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

    let enPassantToSquareNotation (enPassantTarget:int) =
        if enPassantTarget > 0 then
            Notation.bitRefToAlgebraicNotation enPassantTarget
        else "-"

    let chessmen = String.Join('/', allRowsArray)
    let side = sideToLetter pos.SideToPlay
    let castlingRights = castlingRightsToLetter (pos.WhiteCastlingRights, pos.BlackCastlingRights)
    let enPassantTargetNotation = enPassantToSquareNotation pos.EnPassantTarget
    sprintf "%s %c %s %s %d %d" chessmen side castlingRights enPassantTargetNotation pos.HalfMoveClock pos.FullMoveNumber