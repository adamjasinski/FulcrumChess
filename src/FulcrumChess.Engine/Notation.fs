namespace FulcrumChess.Engine
open System.Text.RegularExpressions

module Notation =
    let private fileLetters = [|'a';'b';'c';'d';'e';'f';'g';'h'|]

    let inline getFileIndex bitRef = 7 - (bitRef % 8) //  = squareIndex & 7
    let inline getRankIndex bitRef = bitRef / 8  //= squareIndex >> 3 

    let bitRefToAlgebraicNotation bitRef =
        let fileIndex = getFileIndex bitRef
        let rankIndex = getRankIndex bitRef
        sprintf "%c%d" fileLetters.[fileIndex] (rankIndex+1)

    let toAlgebraicNotation (move:Move) =
        let (srcBitRef, dstBitRef) = move |> Move.getSrcAndDestBitRefs
        if srcBitRef <> dstBitRef then
            sprintf "%s%s" (bitRefToAlgebraicNotation srcBitRef) (bitRefToAlgebraicNotation dstBitRef)
        else
            "0000" //NULL move

    let fromLongAlgebraicNotationToBitRefs (s:string) =
        let pattern = "[a-h][1-8][a-h][1-8]"
        if not <| Regex.IsMatch(s, pattern) then failwithf "Invalid long algebraic notation: %s" s
      
        let srcFile = byte('h') - byte(s.[0]) |> int
        let srcRank = byte(s.[1]) - byte('1') |> int
        let dstFile = byte('h') - byte(s.[2]) |> int
        let dstRank = byte(s.[3]) - byte('1') |> int

        (srcRank*8 + srcFile, dstRank*8 + dstFile)

    let fromLongAlgebraicNotationToMove = fromLongAlgebraicNotationToBitRefs >> Move.create

        
