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
        let struct(srcBitRef, dstBitRef) = move |> Move.getSrcAndDestBitRefs
        let maybeCaptureSuffix = if (move |> Move.isCapture) then "x" else ""
        if srcBitRef <> dstBitRef then
            sprintf "%s%s%s" (bitRefToAlgebraicNotation srcBitRef) (bitRefToAlgebraicNotation dstBitRef) maybeCaptureSuffix
        else
            "0000" //NULL move

    let fromSquareNotationToBitRef (s:string) =
        let pattern = "[a-h][1-8]"
        if not <| Regex.IsMatch(s, pattern) then failwithf "Invalid square notation: %s" s

        let file = byte('h') - byte(s.[0]) |> int
        let rank = byte(s.[1]) - byte('1') |> int
        (rank*8+file)

    let private longAlgPattern = "^[a-h][1-8][a-h][1-8][qrbn]?$"

    let validateLongAlgebraicNotation (s:string) =
        Regex.IsMatch(s, longAlgPattern)

    let fromLongAlgebraicNotationToBitRefs (s:string) =
        if not <| Regex.IsMatch(s, longAlgPattern) then failwithf "Invalid long algebraic notation: %s" s
      
        let srcBitRef = s.Substring(0,2) |> fromSquareNotationToBitRef
        let dstBitRef = s.Substring(2,2) |> fromSquareNotationToBitRef

        let promotionOpt = 
            match s.Substring(4).ToUpperInvariant() with
            | "Q" -> PromotionType.QueenProm |> Some
            | "R" -> PromotionType.RookProm |> Some
            | "B" -> PromotionType.BishopProm |> Some
            | "N" -> PromotionType.KnightProm |> Some
            | _ -> None

        struct(srcBitRef, dstBitRef, promotionOpt)
