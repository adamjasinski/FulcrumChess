namespace FulcrumChess.Engine

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
            sprintf "%s-%s" (bitRefToAlgebraicNotation srcBitRef) (bitRefToAlgebraicNotation dstBitRef)
        else
            "NULL"
