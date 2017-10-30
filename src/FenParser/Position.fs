namespace FenParser
open Common

type Position = {
    WhiteKing:Bitboard;
    WhiteQueen:Bitboard;
    WhiteRooks:Bitboard;
    WhiteBishops:Bitboard;
    WhiteKnights:Bitboard;
    WhitePawns:Bitboard;
    BlackKing:Bitboard;
    BlackQueen:Bitboard;
    BlackRooks:Bitboard;
    BlackBishops:Bitboard;
    BlackKnights:Bitboard;
    BlackPawns:Bitboard;
}

module Positions =
    let emptyBitboard = {
        Position.WhiteKing=0UL;WhiteQueen=0UL;WhiteRooks=0UL;WhiteBishops=0UL;WhiteKnights=0UL;WhitePawns=0UL;
        Position.BlackKing=0UL;BlackQueen=0UL;BlackRooks=0UL;BlackBishops=0UL;BlackKnights=0UL;BlackPawns=0UL; }

    let whiteBitboard (pos:Position) =
        pos.WhiteKing ||| pos.WhiteQueen ||| pos.WhiteRooks ||| pos.WhiteBishops ||| pos.WhiteKnights ||| pos.WhitePawns

    let blackBitboard (pos:Position) =
        pos.BlackKing ||| pos.BlackQueen ||| pos.BlackRooks ||| pos.BlackBishops ||| pos.BlackKnights ||| pos.BlackPawns

    let bothSidesBitboard (pos:Position) =
        Arrow.onSingleCombine (|||) whiteBitboard blackBitboard pos

    let getBitboardForSide (side:Side) (pos:Position) =
        match side with 
        | White -> pos |> whiteBitboard
        | _ -> pos |> blackBitboard
        
    let setFenPiece (piece:char) (bitRef:int) (pos:Position) =
        if bitRef < 0 || bitRef > 63 then invalidArg "bitRef" ("parameter has invalid value: " + bitRef.ToString())
        let candidate:Bitboard = 0UL |> BitUtils.setBit bitRef
        let pos' = 
            match piece with
            | 'p' -> {pos with BlackPawns=pos.BlackPawns ||| candidate }
            | 'n' -> {pos with BlackKnights=pos.BlackKnights ||| candidate }
            | 'b' -> {pos with BlackBishops=pos.BlackBishops ||| candidate }
            | 'r' -> {pos with BlackRooks=pos.BlackRooks ||| candidate }
            | 'q' -> {pos with BlackQueen=pos.BlackQueen ||| candidate }
            | 'k' -> {pos with BlackKing=pos.BlackKing ||| candidate }
            | 'P' -> {pos with WhitePawns=pos.WhitePawns ||| candidate }
            | 'N' -> {pos with WhiteKnights=pos.WhiteKnights ||| candidate }
            | 'B' -> {pos with WhiteBishops=pos.WhiteBishops ||| candidate }
            | 'R' -> {pos with WhiteRooks=pos.WhiteRooks ||| candidate }
            | 'Q' -> {pos with WhiteQueen=pos.WhiteQueen ||| candidate }
            | 'K' -> {pos with WhiteKing=pos.WhiteKing ||| candidate }
            | _ -> invalidArg "piece" ("parameter has invalid value: " + piece.ToString())
        pos'

    let fromFenString (fen:string) : Position =
        let board8x8 = FenParsing.parse fen
        let allPiecesOnBoard = board8x8 |> List.collect id |> Array.ofList |> Array.rev
        let mapped = 
            ((0,emptyBitboard), allPiecesOnBoard) 
            ||> Array.fold (fun (counter,pos) (piece:char) -> 
                match piece with
                | ' ' -> (counter+1,pos)
                | pc -> 
                    let pos' = pos |> setFenPiece piece counter
                    (counter+1, pos') 
            )
        mapped |> snd

    let (|HasBitSet|) (bitRef:int) (bitboard:Bitboard) =
       if bitboard |> BitUtils.hasBitSet bitRef then true
       else false

            
    //let (|isp|isn|isb|) (bitRef:int) (pos:Position) =
        //let hasIt (bitboard:Bitboard) = bitboard |> BitUtils.hasBitSet bitRef
        //match pos with
        //| hasIt bitRef ->
            
            
    
   
    let getChessmenAndSide (bitRef:int) (pos:Position) : Chessmen*Side =
        //match pos with
        //| HasBitSet bitRef -> true
        //TODO
        let hasIt (bitboard:Bitboard) = bitboard |> BitUtils.hasBitSet bitRef
        if pos.BlackPawns |> hasIt then (Chessmen.Pawn, Black)
        else if pos.BlackRooks |> hasIt then (Chessmen.Rook, Black)
        else if pos.BlackKnights |> hasIt then (Chessmen.Knight, Black) 
        else if pos.BlackBishops |> hasIt then (Chessmen.Bishop, Black)
        else if pos.BlackQueen |> hasIt then (Chessmen.Queen, Black)
        else if pos.BlackKing |> hasIt then (Chessmen.King, Black)
        else if pos.WhiteRooks |> hasIt then (Chessmen.Rook, White)
        else if pos.WhiteKnights |> hasIt then (Chessmen.Knight, White)
        else if pos.WhiteBishops |> hasIt then (Chessmen.Bishop, White)
        else if pos.WhiteQueen |> hasIt then (Chessmen.Queen, White)
        else if pos.WhiteKing |> hasIt then (Chessmen.King, White)
        else (Chessmen.Pawn, White)

    let getAttacksFromPseudoMoves (movesBitboard:Bitboard) (bitRef:int) (pos:Position) =
        let chessman, side = pos |> getChessmenAndSide bitRef
        let opponentPieces = pos |> getBitboardForSide (opposite side) 
        movesBitboard &&& opponentPieces
            
       
