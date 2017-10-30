//namespace FenParser 

module Board64
//See also:
//https://chessprogramming.wikispaces.com/little-endian
//https://chessprogramming.wikispaces.com/Square+Mapping+Considerations#LittleEndianRankFileMapping

open System
open System.Globalization
open FenParser

type T = Board64 of byte[]
//module Board64
let create (input:byte[]) =
    if(input.Length <> 64) then invalidArg "input" "input array has to have exactly 64 bytes"
    Board64 input
//let apply f (Board64 s) = f s
//let value s = apply id s
let value (Board64 b) = b

let createEmpty () =
    Array.create 64 0uy |> Board64

let clone board =
    board |> value |> Array.copy |> create
    
let private getBitboard predicate board =
    let mutable bitboard = 0L
    let boardValues = board |> value
    for i = 0 to boardValues |> Array.length do
        if(predicate boardValues.[i]) then
            let mask = 1L <<< i
            bitboard <- bitboard ||| mask
    bitboard

let makeBitboardFromIndexes (squareIndexes:int[]) =
    squareIndexes |> Array.fold (fun state ind ->
        let mask = 1L <<< ind
        state ||| mask
        ) 0L

let private getBitboard2 predicate board =
    board |> value 
    |>  Array.mapi( fun i v ->
        if predicate v then Some i else None)
    |> Array.filter Option.isSome
    |> Array.map Option.get
    |> makeBitboardFromIndexes

let getAllWhitePiecesBitboard = getBitboard Pieces.isWhitePiece

let getAllBlackPiecesBitboard = getBitboard Pieces.isBlackPiece

let private fileLetters = [|'a';'b';'c';'d';'e';'f';'g';'h'|]
let private fileLettersToIndexes = dict[('a',0); ('b',1); ('c',2); ('d',3); ('e',4); ('f',5); ('g',6); ('h',7)]
//let private fileLetterAsIndex squareIndex = 
//    fileLettersToIndexes.[squareIndex]

let squareIndexToAlgebraicString squareIndex =
    let fileIndex = squareIndex % 8 //  = squareIndex & 7
    let rankIndex   = squareIndex / 8  //= squareIndex >> 3 
    sprintf "%c%d" fileLetters.[fileIndex] rankIndex

let algebraicStringAsSquareIndex (algCoord:string) =
    if(algCoord.Length <> 2) then 
        invalidArg "algCoord" ("Invalid algebraic coordinates " + algCoord)
    else
        let fileLetterNormalized = algCoord.[0] |> Char.ToLower
        let fileIndex = fileLettersToIndexes.[fileLetterNormalized]
        let rankIndex = CharUnicodeInfo.GetDecimalDigitValue algCoord.[1] - 1
        if(rankIndex >= 0 && rankIndex <= 7) then
            rankIndex*8 + fileIndex
        else
            invalidArg "algCoord" ("Invalid rank " + algCoord)

let fileRankIndexesToSquareIndex (file,rank) =
    rank * 8 + file

let isOnBoard squareIndex =
    squareIndex >=0 && squareIndex <= 63

let moveTo (dir:Direction) (srcSquareIndex:int) : int =
    raise (NotImplementedException("TODO"))

let dumpAsText board =
    //let boardValues = board |> value
    let (Board64 boardValues) = board
    let horizontalDelimiter = "+---+---+---+---+---+---+---+---+"
    let sb = System.Text.StringBuilder(33*17)
    for j=7 downto 0 do
        sb.AppendLine horizontalDelimiter |> ignore
        for i=0 to 7 do
            let squareIndex = fileRankIndexesToSquareIndex (i, j)
            let squareFenValue = Pieces.numberAsFenCharacter boardValues.[squareIndex]
            match squareFenValue with
            | SquareFenValue.Blank -> sb.Append "|   " |> ignore
            | SquareFenValue.Fen pc -> sb.Append (sprintf "| %c " pc) |> ignore
            | _ -> invalidOp ("invalid value on board: " + boardValues.[squareIndex].ToString())
        sb.AppendLine "|" |> ignore
    sb.AppendLine horizontalDelimiter |> ignore
    sb.ToString()

let dumpAsFenRecursive board =
    let boardValues = board |> value
    let LTRseq = 
        [|  for j in 0..7 do
                for i in 0..7 do
                    yield (7-j)*8+i  |]
    
    let rec loop index blanksPrecedingCount acc =
        if index < 64 then
            let isLastFileInRank = ((index+1) % 8 = 0)
            let acc' = if (index>0 && index%8=0) then acc + "/" else acc
            let currentSquareValue = boardValues.[LTRseq.[index]]
            let squareFenValue = Pieces.numberAsFenCharacter currentSquareValue
            match squareFenValue with
                | SquareFenValue.Blank when not isLastFileInRank ->
                    loop (index+1) (blanksPrecedingCount+1) acc'
                | SquareFenValue.Blank when isLastFileInRank ->
                    loop (index+1) 0 (acc' + (sprintf "%d" (blanksPrecedingCount+1)))
                | SquareFenValue.Fen pc when blanksPrecedingCount > 0 ->
                    loop (index+1) 0 (acc' + (sprintf "%d%c" blanksPrecedingCount pc))
                | SquareFenValue.Fen pc -> 
                    loop (index+1) 0 (acc' + (sprintf "%c" pc))
                | _ -> invalidOp "invalid value on board"
            else
                acc
//        match currentSquareValue with
//        | 0uy when not isLastFileInRank ->
//            loop nextSquareIndex (blanksPrecedingCount+1) acc
//        | 0uy when isLastFileInRank ->
//            loop nextSquareIndex 0 (acc + (sprintf "%d" (blanksPrecedingCount+1)))
//        | num when blanksPrecedingCount > 0 ->
//            loop nextSquareIndex 0 (acc + (sprintf "%d%c" blanksPrecedingCount <| Pieces.numberAsFenCharacter num))
//        | num -> 
//            loop nextSquareIndex 0 (acc + (sprintf "%c" <| Pieces.numberAsFenCharacter num))

    let fenBoard = loop 0 0 ""
    //TODO  - FEN is incomplete; need other game information
    fenBoard
//
//let test1 src dir =
//    let target =
//        match dir with
//        | 0 -> if(src % 8 > 0) then Some <| src-1 else None //W
//        | 1 -> if(src % 8 > 0 && src < 56) then Some (src+7) else None
    
 