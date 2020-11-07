﻿namespace FulcrumChess.Engine

[<System.FlagsAttribute>]
type CastlingRights = None = 0 | KingSide = 1 | QueenSide = 2 

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

    CastlingRightsPerSide:CastlingRights*CastlingRights;
    SideToPlay:Side;
}

type CastlingLookup = {
    InitialPositionKing:Bitboard;
    InitialPositionKingsRook:Bitboard;
    InitialPositionQueensRook:Bitboard;
    BlockersKingsRook:Bitboard;
    BlockersQueensRook:Bitboard;
    DestinationBitRefKingSideCastling:int;
    DestinationBitRefQueenSideCastling:int;
}
    
module Positions =
    let emptyBitboard = {
        Position.WhiteKing=0UL;WhiteQueen=0UL;WhiteRooks=0UL;WhiteBishops=0UL;WhiteKnights=0UL;WhitePawns=0UL;
        Position.BlackKing=0UL;BlackQueen=0UL;BlackRooks=0UL;BlackBishops=0UL;BlackKnights=0UL;BlackPawns=0UL;
        CastlingRightsPerSide = (CastlingRights.None,CastlingRights.None);
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
         CastlingRightsPerSide = (CastlingRights.None,CastlingRights.None);
         SideToPlay = White;}

    let castlingLookups = dict[
        Side.White, { 
            CastlingLookup.InitialPositionKing = 8UL;
            InitialPositionKingsRook = 1UL;
            InitialPositionQueensRook = 128UL;
            BlockersKingsRook = 6UL;
            BlockersQueensRook = 112UL;
            DestinationBitRefKingSideCastling = 1;
            DestinationBitRefQueenSideCastling = 5;
            };
        Side.Black, { 
            CastlingLookup.InitialPositionKing = 576460752303423488UL;
            InitialPositionKingsRook = (1UL <<< 56);
            InitialPositionQueensRook = (1UL <<< 63);
            BlockersKingsRook = (1UL <<< 57) ||| (1UL <<< 58);
            BlockersQueensRook = (1UL <<< 60) ||| (1UL <<< 61) ||| (1UL <<<62);
            DestinationBitRefKingSideCastling = 57;
            DestinationBitRefQueenSideCastling = 61;
            };
    ]

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

    let getBitboardForSideToPlay (pos:Position) =
        pos |> getBitboardForSide pos.SideToPlay
        
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

    let getKingBitboard (side:Side) (pos:Position) =
        match side with
        | Side.White -> pos.WhiteKing
        | Side.Black -> pos.BlackKing

    let getRooksBitboard (side:Side) (pos:Position) =
        match side with
        | Side.White -> pos.WhiteRooks
        | Side.Black -> pos.BlackRooks

    let getChessmanAndSide (bitRef:int) (pos:Position) : (Chessmen*Side) option =
        let hasBitRef (bitboard:Bitboard) = bitboard |> BitUtils.hasBitSet bitRef
        let res =
            pos
            |> asBitboardSequence
            |> Seq.tryFind (fun (bb,_) -> bb |> hasBitRef)

        res |> Option.map snd

    let getCapturesFromPseudoMoves (movesBitboard:Bitboard) (bitRef:int) (pos:Position) =
        let (chessman, side) = pos |> getChessmanAndSide bitRef |> Option.get
        let opponentPieces = pos |> getBitboardForSide (opposite side) 
        movesBitboard &&& opponentPieces


    // let bitboardToLerbefArray (bitboard:Bitboard) =
    //     let arr = Array.zeroCreate<byte> 64
    //     bitboard 
    //     |> BitUtils.getSetBits
    //     |> Array.iter( fun bitRef -> arr.[bitRef] <- 1uy)
    //     arr

    let private setPieceInternal (piece:Chessmen, side:Side) (bitRef:int) (pos:Position) =
        let fenLetter = (piece, side) |> PieceFenLetters.getLetter
        pos |> setFenPiece fenLetter bitRef

    let private clearPieceInternal (piece:Chessmen, side:Side) (bitRef:int) (pos:Position) =
        if bitRef < 0 || bitRef > 63 then invalidArg "bitRef" ("parameter has invalid value: " + bitRef.ToString())
        let clearBitRef = BitUtils.clearBit bitRef
        let pos' = 
            match piece,side with
            | (Chessmen.Pawn, Side.Black) -> {pos with BlackPawns= pos.BlackPawns |> clearBitRef}
            | (Chessmen.Knight, Side.Black) -> {pos with BlackKnights=pos.BlackKnights |> clearBitRef }
            | (Chessmen.Bishop, Side.Black) -> {pos with BlackBishops=pos.BlackBishops |> clearBitRef }
            | (Chessmen.Rook, Side.Black) -> {pos with BlackRooks=pos.BlackRooks |> clearBitRef }
            | (Chessmen.Queen, Side.Black) -> {pos with BlackQueen=pos.BlackQueen |> clearBitRef }
            | (Chessmen.King, Side.Black) -> {pos with BlackKing=pos.BlackKing |> clearBitRef }
            | (Chessmen.Pawn, Side.White) -> {pos with WhitePawns=pos.WhitePawns |> clearBitRef }
            | (Chessmen.Knight, Side.White) -> {pos with WhiteKnights=pos.WhiteKnights |> clearBitRef }
            | (Chessmen.Bishop, Side.White) -> {pos with WhiteBishops=pos.WhiteBishops |> clearBitRef }
            | (Chessmen.Rook, Side.White) -> {pos with WhiteRooks=pos.WhiteRooks |> clearBitRef }
            | (Chessmen.Queen, Side.White) -> {pos with WhiteQueen=pos.WhiteQueen |> clearBitRef }
            | (Chessmen.King, Side.White) -> {pos with WhiteKing=pos.WhiteKing |> clearBitRef }
        pos'

    let isCheck (getAttacks:Side->Position->Move array) (kingSide:Side) (pos:Position) =
        let opponentAttacks = pos |> getAttacks (opposite kingSide) |> Move.movesToDstBitboard
        let kingBitboard = pos |> getKingBitboard kingSide
        kingBitboard &&& opponentAttacks > 0UL

    let private isCastlingPathUnderAttack (castlingType:CastlingType) (getAttacks:Side->Position->Move array) (kingSide:Side) (pos:Position) =
        let opponentAttacks = pos |> getAttacks (opposite kingSide) |> Move.movesToDstBitboard
        let castlingLookup = castlingLookups.[kingSide]
        let castlingPathBitboard = 
            match castlingType with
            | KingSide -> castlingLookup.BlockersKingsRook
            | QueenSide -> castlingLookup.BlockersQueensRook
        castlingPathBitboard &&& opponentAttacks > 0UL

    let makeMoveWithValidation (getAttacks:Side->Position->Move array) (move:Move) (pos:Position) =
        let (srcBitRef, dstBitRef) = move |> Move.getSrcAndDestBitRefs
        let (chessman, side) = pos |> getChessmanAndSide srcBitRef |> Option.get

        //TODO - validate legality of the move
        //TODO - support castling and en passant
        let castlingTypeOpt = move |> Move.determineCastlingType (chessman, side)

        let alsoMoveRookIfCastling (p:Position) =
        //TODO - check castling eligibility
            let rookMoveIfCastling =
                match (side, castlingTypeOpt) with
                | (White, Some(KingSide)) ->   (0, 2) |> Option.Some
                | (White, Some(QueenSide)) ->  (7, 4) |> Option.Some
                | (Black, Some(KingSide)) ->  (56, 58) |> Option.Some
                | (Black, Some(QueenSide)) -> (63, 60) |> Option.Some
                | (_, None) -> Option.None

            match rookMoveIfCastling with
            | Some(srcBitRefRook, dstBitRefRook) -> 
                p
                |> clearPieceInternal (Chessmen.Rook, side) srcBitRefRook
                |> setPieceInternal (Chessmen.Rook, side) dstBitRefRook
            | None -> p

        let clearOpponentPieceIfCapture (p:Position) =
            let dstSquare = pos |> getChessmanAndSide dstBitRef
            if dstSquare |> Option.isNone then p
            else
                let (opponentPiece,opponentSide) = dstSquare |> Option.get
                if(opponentSide <> (side |> opposite)) then illegalMove "Error: Move destination targets a friendly piece"
                p |> clearPieceInternal (opponentPiece, opponentSide) dstBitRef

        let swapSide (p:Position) =
            { p with SideToPlay=opposite side }

        let castlingPrevalidation p = 
            match castlingTypeOpt with
            | Some(_) -> if (p|> isCheck getAttacks side) then None else Some p
            | None -> Some p

        let makeMoveInternal = 
            setPieceInternal (chessman, side) dstBitRef
            >> clearPieceInternal (chessman, side) srcBitRef
            >> clearOpponentPieceIfCapture
            >> alsoMoveRookIfCastling
            >> swapSide 

        let predicateFilter predicate x =
            if predicate x then Some x else None

        let isNotCheckFilter = predicateFilter (fun p -> not (isCheck getAttacks side p))
        let isNotCastlingPathUnderAttackFilter = predicateFilter (fun p ->
            castlingTypeOpt |> Option.isNone || not (isCastlingPathUnderAttack castlingTypeOpt.Value getAttacks side p))

        pos
        |> castlingPrevalidation
        |> Option.map makeMoveInternal
        |> Option.bind isNotCheckFilter
        |> Option.bind isNotCastlingPathUnderAttackFilter
