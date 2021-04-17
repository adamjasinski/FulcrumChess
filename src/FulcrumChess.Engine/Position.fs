namespace FulcrumChess.Engine

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

    WhiteCastlingRights:CastlingRights;
    BlackCastlingRights:CastlingRights;
    SideToPlay:Side;
    EnPassantTarget:int;
    HalfMoveClock:int;
    FullMoveNumber:int
}

type CastlingLookup = {
    InitialPositionKing:Bitboard;
    InitialPositionKingsRook:Bitboard;
    InitialPositionQueensRook:Bitboard;
    BlockersKingsRook:Bitboard;
    BlockersQueensRook:Bitboard;
    PathNonUnderCheckKing:Bitboard;
    PathNonUnderCheckQueen:Bitboard;
    DestinationBitRefKingSideCastling:int;
    DestinationBitRefQueenSideCastling:int;
    DestinationKingSideCastling:Bitboard;
    DestinationQueenSideCastling:Bitboard;
}

type GetAttacks = Side->Position->Bitboard
type GetMovesForSide = Position->Move array
    
module Position =
    let emptyBitboard = {
        Position.WhiteKing=0UL;WhiteQueen=0UL;WhiteRooks=0UL;WhiteBishops=0UL;WhiteKnights=0UL;WhitePawns=0UL;
        Position.BlackKing=0UL;BlackQueen=0UL;BlackRooks=0UL;BlackBishops=0UL;BlackKnights=0UL;BlackPawns=0UL;
        WhiteCastlingRights=CastlingRights.None;
        BlackCastlingRights=CastlingRights.None;
        SideToPlay=White;
        EnPassantTarget=0;
        HalfMoveClock=0;
        FullMoveNumber=1; }

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
         WhiteCastlingRights=CastlingRights.None;
         BlackCastlingRights=CastlingRights.None;
         SideToPlay=White;
         EnPassantTarget=0;
         HalfMoveClock=0;
         FullMoveNumber=1; }

    let castlingLookups = dict[
        Side.White, { 
            CastlingLookup.InitialPositionKing = 8UL;
            InitialPositionKingsRook = 1UL;
            InitialPositionQueensRook = 128UL;
            BlockersKingsRook = 6UL;
            BlockersQueensRook = 112UL;
            PathNonUnderCheckKing = (1UL <<< 1) ||| (1UL <<< 2);
            PathNonUnderCheckQueen = (1UL <<< 4) ||| (1UL <<< 5);
            DestinationBitRefKingSideCastling = 1;
            DestinationBitRefQueenSideCastling = 5;
            DestinationKingSideCastling = (1UL <<< 1);
            DestinationQueenSideCastling = (1UL <<< 5);
            };
        Side.Black, { 
            CastlingLookup.InitialPositionKing = 576460752303423488UL;
            InitialPositionKingsRook = (1UL <<< 56);
            InitialPositionQueensRook = (1UL <<< 63);
            BlockersKingsRook = (1UL <<< 57) ||| (1UL <<< 58);
            BlockersQueensRook = (1UL <<< 60) ||| (1UL <<< 61) ||| (1UL <<<62);
            PathNonUnderCheckKing = (1UL <<< 57) ||| (1UL <<< 58);
            PathNonUnderCheckQueen = (1UL <<< 60) ||| (1UL <<< 61);
            DestinationBitRefKingSideCastling = 57;
            DestinationBitRefQueenSideCastling = 61;
            DestinationKingSideCastling = (1UL <<< 57);
            DestinationQueenSideCastling = (1UL <<< 61);
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

    
    let dumpPosition (pos:Position) =
        [|for i in 7..-1..0 ->
            [|for j in 7..-1..0 ->
                let pc = pos |> getChessmanAndSide (8*i+j)
                match pc with
                | Some(chessmanAndSide) -> PieceFenLetters.getLetter chessmanAndSide
                | None -> ' '
            |]
        |]

    let prettyPrint (pos:Position) =
        let boardArray = dumpPosition pos

        let stringJoinWithLeadingAndTrailing (separator:string) (values:string array) =
            let inner = System.String.Join(separator, values)
            sprintf "%s%s%s" separator inner separator

        let allRows = boardArray |> Array.map( fun rowArray -> 
            let charsWithSpaces = rowArray |> Array.map( fun c -> sprintf " %c " c)
            stringJoinWithLeadingAndTrailing "|" charsWithSpaces
        )

        let rowSeparator = "\n+---+---+---+---+---+---+---+---+\n"
        stringJoinWithLeadingAndTrailing rowSeparator allRows

    let isCheck (getAttacks:GetAttacks) (pos:Position) =
        let side = pos.SideToPlay
        let opponentAttacks = pos |> getAttacks (opposite side)
        let kingBitboard = pos |> getKingBitboard side
        kingBitboard &&& opponentAttacks > 0UL

    let private isCastlingPathUnderAttack (castlingType:CastlingType) (getAttacks:GetAttacks) (side:Side) (pos:Position) =
        let opponentAttacks = pos |> getAttacks (opposite side)
        let castlingLookup = castlingLookups.[side]
        let castlingPathBitboard = 
            match castlingType with
            | KingSide -> castlingLookup.PathNonUnderCheckKing
            | QueenSide -> castlingLookup.PathNonUnderCheckQueen
        castlingPathBitboard &&& opponentAttacks > 0UL

    let private getCastlingRights (side:Side) (pos:Position)=
        match side with
            | White -> pos.WhiteCastlingRights
            | Black -> pos.BlackCastlingRights

    let private hasCastlingRights (castlingType:CastlingType) (side:Side) (pos:Position) =
        let castlingRights = pos |> getCastlingRights side
        match (castlingType) with
            | KingSide ->  castlingRights &&& (CastlingRights.KingSide) > CastlingRights.None
            | QueenSide -> castlingRights &&& (CastlingRights.QueenSide) > CastlingRights.None

    let private setCastlingRights (castlingRights:CastlingRights) (side:Side) (pos:Position) =
        match side with
            | White -> { pos with WhiteCastlingRights = castlingRights }
            | Black -> { pos with BlackCastlingRights = castlingRights }
        
    let tryMakeMoveInternal (getAttacks:GetAttacks) (move:Move) (pos:Position) =
        let (srcBitRef, dstBitRef) = move |> Move.getSrcAndDestBitRefs
        let (chessman, side) = pos |> getChessmanAndSide srcBitRef |> Option.get

        // NB - this function is intended to be used for pseudo moves that have been previously generated by the engine
        // Therefore we skip full validation and focus on check validation and special moves
        // TODO - support castling and en passant
        let castlingTypeOpt = move |> Move.determineCastlingType (chessman, side)

        let alsoMoveRookIfCastling (p:Position) =
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

        let isCastlingPreconditionsMet p = 
            match castlingTypeOpt with
            | Some castlingType -> 
                (p |> hasCastlingRights castlingType side) && not(p|> isCheck getAttacks)
            | None -> true

        let alignOtherPiecesForSpecialMovesFilter = alsoMoveRookIfCastling

        let makeMoveInternal = 
            setPieceInternal (chessman, side) dstBitRef
            >> clearPieceInternal (chessman, side) srcBitRef
            >> clearOpponentPieceIfCapture
            >> alignOtherPiecesForSpecialMovesFilter

        let isNotCheckFilter = isCheck getAttacks >> not
        let isNotCastlingPathUnderAttackFilter p =
            castlingTypeOpt |> Option.isNone || not (isCastlingPathUnderAttack castlingTypeOpt.Value getAttacks side p)

        let updateCastlingRightsIfApplicableFilter p =
            let currentCastlingRights = p |> getCastlingRights side
            let castlingLookup = castlingLookups.[side]

            let revisedCastlingRightsOpt = 
                match chessman with
                | Chessmen.King -> Some CastlingRights.None
                | Chessmen.Rook -> 
                    let dstBitRefKingSide = castlingLookup.DestinationBitRefKingSideCastling 
                    let dstBitRefQueenSide = castlingLookup.DestinationBitRefQueenSideCastling 
                    match dstBitRef with
                    | dst when dst = dstBitRefKingSide -> Some (currentCastlingRights &&& ~~~CastlingRights.KingSide)
                    | dst when dst = dstBitRefQueenSide -> Some (currentCastlingRights &&& ~~~CastlingRights.QueenSide)
                    | _ -> None
                | _ -> None
            
            match revisedCastlingRightsOpt with
            | Some revisedCastlingRights -> p |> setCastlingRights revisedCastlingRights side
            | None -> p 

        pos |> Some 
        |> Option.filter isCastlingPreconditionsMet
        |> Option.map makeMoveInternal
        |> Option.filter isNotCheckFilter
        |> Option.filter isNotCastlingPathUnderAttackFilter
        |> Option.map updateCastlingRightsIfApplicableFilter
        |> Option.map swapSide

    let tryMakeMoveWithFullValidation (generatePseudoMoves:Position->int->Move array) (getAttacks:GetAttacks) (move:Move) (pos:Position) =
        let (srcBitRef, dstBitRef) = move |> Move.getSrcAndDestBitRefs
        
        let sideToPlayPredicate p = 
            let (_, side) = p |> getChessmanAndSide srcBitRef |> Option.get  
            side = p.SideToPlay
        
        let possibleMovePredicate p =
            let generatedPseudoMoves = 
                generatePseudoMoves pos srcBitRef 
                |> Array.map Move.getSrcAndDestBitRefs
            generatedPseudoMoves |> Array.exists (fun srcAndDest -> srcAndDest = (srcBitRef,dstBitRef))

        let legalMoveFilter = tryMakeMoveInternal getAttacks move

        pos |> Some
        |> Option.filter sideToPlayPredicate
        |> Option.filter possibleMovePredicate
        |> Option.bind legalMoveFilter

    let isCheckMate (generateAllPseudoMovesForSide:GetMovesForSide) (getAttacks:GetAttacks) (pos:Position) =
        pos 
        |> generateAllPseudoMovesForSide
        |> Array.exists (fun m -> 
            tryMakeMoveInternal getAttacks m pos |> Option.isSome)
        |> not

