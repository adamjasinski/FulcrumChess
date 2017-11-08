namespace FenParser

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

    SideToPlay:Side;
}

module Positions =
    let emptyBitboard = {
        Position.WhiteKing=0UL;WhiteQueen=0UL;WhiteRooks=0UL;WhiteBishops=0UL;WhiteKnights=0UL;WhitePawns=0UL;
        Position.BlackKing=0UL;BlackQueen=0UL;BlackRooks=0UL;BlackBishops=0UL;BlackKnights=0UL;BlackPawns=0UL;
        SideToPlay=White }

    let initialPosition = {
         Position.WhiteKing = 8UL;
         WhiteQueen = 16UL;
         WhiteRooks = 129UL;
         WhiteBishops = 36UL;
         WhiteKnights = 66UL;
         WhitePawns = 65280UL;
         BlackKing = 576460752303423488UL;
         BlackQueen = 1152921504606846976UL;
         BlackRooks = 9295429630892703744UL;
         BlackBishops = 2594073385365405696UL;
         BlackKnights = 4755801206503243776UL;
         BlackPawns = 71776119061217280UL;
         SideToPlay = White;}

    let whiteBitboard (pos:Position) =
        pos.WhiteKing ||| pos.WhiteQueen ||| pos.WhiteRooks ||| pos.WhiteBishops ||| pos.WhiteKnights ||| pos.WhitePawns

    let blackBitboard (pos:Position) =
        pos.BlackKing ||| pos.BlackQueen ||| pos.BlackRooks ||| pos.BlackBishops ||| pos.BlackKnights ||| pos.BlackPawns

    let bothSidesBitboard (pos:Position) =
        Arrow.onSingleCombine (|||) whiteBitboard blackBitboard pos

    let getBitboardForSide (side:Side) (pos:Position) =
        match side with 
        | White -> pos |> whiteBitboard
        | Black -> pos |> blackBitboard
        
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

    let asBitboardSequence (pos:Position) =
        seq {
            yield (pos.BlackPawns, (Chessmen.Pawn, Black))
            yield (pos.BlackRooks, (Chessmen.Rook, Black))
            yield (pos.BlackKnights, (Chessmen.Knight, Black))
            yield (pos.BlackBishops, (Chessmen.Bishop, Black))
            yield (pos.BlackQueen, (Chessmen.Queen, Black))
            yield (pos.BlackKing, (Chessmen.King, Black))
            yield (pos.WhiteRooks, (Chessmen.Rook, White))
            yield (pos.WhiteKnights, (Chessmen.Knight, White))
            yield (pos.WhiteBishops, (Chessmen.Bishop, White))
            yield (pos.WhiteQueen, (Chessmen.Queen, White))
            yield (pos.WhiteKing, (Chessmen.King, White))
            yield (pos.WhitePawns, (Chessmen.Pawn, White))
        }


    let getChessmanAndSide (bitRef:int) (pos:Position) : (Chessmen*Side) option =
        let hasBitRef (bitboard:Bitboard) = bitboard |> BitUtils.hasBitSet bitRef
        let res =   pos |> (asBitboardSequence >> Seq.tryFind (fun (bb,_) -> bb |> hasBitRef))

        res |> Option.map snd

        //res |> Option.bind (fun x -> match x with | Some (a,b,c) -> Some(b,c) | None -> None)
        //if pos.BlackPawns |> hasIt then Some(Chessmen.Pawn, Black)
        //else if pos.BlackRooks |> hasIt then Some(Chessmen.Rook, Black)
        //else if pos.BlackKnights |> hasIt then Some(Chessmen.Knight, Black) 
        //else if pos.BlackBishops |> hasIt then Some(Chessmen.Bishop, Black)
        //else if pos.BlackQueen |> hasIt then Some(Chessmen.Queen, Black)
        //else if pos.BlackKing |> hasIt then Some(Chessmen.King, Black)
        //else if pos.WhiteRooks |> hasIt then Some(Chessmen.Rook, White)
        //else if pos.WhiteKnights |> hasIt then Some(Chessmen.Knight, White)
        //else if pos.WhiteBishops |> hasIt then Some(Chessmen.Bishop, White)
        //else if pos.WhiteQueen |> hasIt then Some(Chessmen.Queen, White)
        //else if pos.WhiteKing |> hasIt then Some(Chessmen.King, White)
        //else if pos.WhitePawns |> hasIt then Some(Chessmen.Pawn, White)
        //else None

    let getCapturesFromPseudoMoves (movesBitboard:Bitboard) (bitRef:int) (pos:Position) =
        let (chessman, side) = pos |> getChessmanAndSide bitRef |> Option.get
        let opponentPieces = pos |> getBitboardForSide (opposite side) 
        movesBitboard &&& opponentPieces


    let bitboardToLerbefArray (bitboard:Bitboard) =
        let arr = Array.zeroCreate<byte> 64
        bitboard 
        |> BitUtils.getSetBits
        |> Array.iter( fun bitRef -> arr.[bitRef] <- 1uy)
        arr
    //let asBoard8x8 (pos:Position) =
        //TODO

    let makeMoveWithValidation (piece:char) (bitRef:int) (pos:Position) =
        //TODO
        None