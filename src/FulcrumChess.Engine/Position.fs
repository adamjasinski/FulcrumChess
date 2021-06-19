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
    FullMoveNumber:int;
    HashKey:uint64;
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
        FullMoveNumber=1; 
        HashKey=0UL;}

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
         FullMoveNumber=1; 
         HashKey=16566630060555062715UL }

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
        let updateWithTargetChessmanHash pc = pos.HashKey ^^^ Zobrist.getChessmanHash (pc, pos.SideToPlay) bitRef
        let pos' = 
            match piece with
            | 'p' -> {pos with BlackPawns=pos.BlackPawns ||| candidate; HashKey=updateWithTargetChessmanHash Chessmen.Pawn }
            | 'n' -> {pos with BlackKnights=pos.BlackKnights ||| candidate; HashKey=updateWithTargetChessmanHash Chessmen.Knight  }
            | 'b' -> {pos with BlackBishops=pos.BlackBishops ||| candidate; HashKey=updateWithTargetChessmanHash Chessmen.Bishop  }
            | 'r' -> {pos with BlackRooks=pos.BlackRooks ||| candidate; HashKey=updateWithTargetChessmanHash Chessmen.Rook  }
            | 'q' -> {pos with BlackQueen=pos.BlackQueen ||| candidate; HashKey=updateWithTargetChessmanHash Chessmen.Queen  }
            | 'k' -> {pos with BlackKing=pos.BlackKing ||| candidate; HashKey=updateWithTargetChessmanHash Chessmen.King  }
            | 'P' -> {pos with WhitePawns=pos.WhitePawns ||| candidate; HashKey=updateWithTargetChessmanHash Chessmen.Pawn  }
            | 'N' -> {pos with WhiteKnights=pos.WhiteKnights ||| candidate; HashKey=updateWithTargetChessmanHash Chessmen.Knight  }
            | 'B' -> {pos with WhiteBishops=pos.WhiteBishops ||| candidate; HashKey=updateWithTargetChessmanHash Chessmen.Bishop  }
            | 'R' -> {pos with WhiteRooks=pos.WhiteRooks ||| candidate; HashKey=updateWithTargetChessmanHash Chessmen.Rook  }
            | 'Q' -> {pos with WhiteQueen=pos.WhiteQueen ||| candidate; HashKey=updateWithTargetChessmanHash Chessmen.Queen  }
            | 'K' -> {pos with WhiteKing=pos.WhiteKing ||| candidate; HashKey=updateWithTargetChessmanHash Chessmen.King  }
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
        //let updatedHash = pos.HashKey ^^^ Zobrist.getEmptyFieldHash bitRef
        let updatedHash = pos.HashKey ^^^ Zobrist.getChessmanHash (piece, pos.SideToPlay) bitRef

        let pos' = 
            match piece,side with
            | (Chessmen.Pawn, Side.Black) -> {pos with BlackPawns= pos.BlackPawns |> clearBitRef; HashKey=updatedHash}
            | (Chessmen.Knight, Side.Black) -> {pos with BlackKnights=pos.BlackKnights |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Bishop, Side.Black) -> {pos with BlackBishops=pos.BlackBishops |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Rook, Side.Black) -> {pos with BlackRooks=pos.BlackRooks |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Queen, Side.Black) -> {pos with BlackQueen=pos.BlackQueen |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.King, Side.Black) -> {pos with BlackKing=pos.BlackKing |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Pawn, Side.White) -> {pos with WhitePawns=pos.WhitePawns |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Knight, Side.White) -> {pos with WhiteKnights=pos.WhiteKnights |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Bishop, Side.White) -> {pos with WhiteBishops=pos.WhiteBishops |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Rook, Side.White) -> {pos with WhiteRooks=pos.WhiteRooks |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Queen, Side.White) -> {pos with WhiteQueen=pos.WhiteQueen |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.King, Side.White) -> {pos with WhiteKing=pos.WhiteKing |> clearBitRef; HashKey=updatedHash }
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

    let hasCastlingRights (castlingType:CastlingType) (side:Side) (pos:Position) =
        let castlingRights = pos |> getCastlingRights side
        match (castlingType) with
            | KingSide ->  castlingRights &&& (CastlingRights.KingSide) > CastlingRights.None
            | QueenSide -> castlingRights &&& (CastlingRights.QueenSide) > CastlingRights.None

    let private setCastlingRights (castlingRights:CastlingRights) (side:Side) (pos:Position) =
        let updatedHashKey = pos.HashKey ^^^ Zobrist.getCastlingRightsHash castlingRights
        match side with
            | White -> { pos with WhiteCastlingRights = castlingRights; HashKey = updatedHashKey }
            | Black -> { pos with BlackCastlingRights = castlingRights; HashKey = updatedHashKey }

    let calculateZobristHash (pos:Position) =
        let castlingRightsHash = pos |> getCastlingRights pos.SideToPlay |> Zobrist.getCastlingRightsHash
        let enPassantsHash = pos.EnPassantTarget |> Zobrist.getEnPassantHash
        let sideHash = pos.SideToPlay |> Zobrist.getSideToPlayHash
        let chessmenHash = 
            pos 
            |> asBitboardSequence
            |> Seq.collect (fun (bb, pcAndSide) ->
                    let bitRefs = BitUtils.getSetBits_u64 bb
                    bitRefs |> Array.map (Zobrist.getChessmanHash pcAndSide))
            |> Seq.reduce (^^^)
        chessmenHash ^^^ castlingRightsHash ^^^ enPassantsHash ^^^ sideHash

    let private getEnPassantPotentialCaptorOfBlack dstBitRef =
            //white pawns directly beside the black pawn that just moved by 2 squares; indexed by black pawn destination 
            //   e.g. 32-> 33
            //   33 -> 32,34
            //   34 -> 33,35
            //   40 -> 39
            let rankIndex = getRankIndex dstBitRef
            let fileIndex = getFileIndex dstBitRef
            
            match (rankIndex, fileIndex) with
            | (4, 0) -> BitUtils.setBit 38 0UL
            | (4, col) when col > 0 && col < 7 -> (BitUtils.setBit (dstBitRef-1) 0UL) ||| (BitUtils.setBit (dstBitRef+1) 0UL)
            | (4, 7) -> BitUtils.setBit 33 0UL
            | (_, _) -> 0UL 

    let private getEnPassantPotentialCaptorOfWhite dstBitRef =
            //white pawns directly beside the white pawn that just moved by 2 squares; indexed by white pawn destination 
            let rankIndex = getRankIndex dstBitRef
            let fileIndex = getFileIndex dstBitRef

            match (rankIndex, fileIndex) with
            | (3, 0) -> BitUtils.setBit 30 0UL
            | (3, col) when col > 0 && col < 7 -> (BitUtils.setBit (dstBitRef-1) 0UL) ||| (BitUtils.setBit (dstBitRef+1) 0UL)
            | (3, 7) -> BitUtils.setBit 25 0UL
            | (_, _) -> 0UL 
 
    let tryMakeMoveInternal (getAttacks:GetAttacks) (move:Move) (pos:Position) =
        let (srcBitRef, dstBitRef) = move |> Move.getSrcAndDestBitRefs
        let (chessman, side) = pos |> getChessmanAndSide srcBitRef |> Option.get

        // NB - this function is intended to be used for pseudo moves that have been previously generated by the engine
        // Therefore we skip full validation and focus on check validation and special moves
        let castlingTypeOpt = move |> Castling.determineCastlingType (chessman, side)

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

        let clearOpponentPawnIfEnPassant (p:Position) =
            if move |> Move.isEnPassant then
                let opponentPawnToClearBitRef = 
                    match side with
                    | White -> dstBitRef-8
                    | Black -> dstBitRef+8
                let opponentPawnToClearSquare = pos |> getChessmanAndSide opponentPawnToClearBitRef
                if opponentPawnToClearSquare |> Option.isNone then 
                    illegalMove "Error: Move is supposed to be en passant, but there's no opponent pawn in front of the en passant target"
                p |> clearPieceInternal (Chessmen.Pawn, opposite side) opponentPawnToClearBitRef
            else p

        let replacePawnIfPromotion (p:Position) =
            let newPieceOpt = 
                move 
                |> Move.getPromotionType
                |> Option.map (function 
                    | QueenProm -> Chessmen.Queen 
                    | RookProm -> Chessmen.Rook 
                    | BishopProm -> Chessmen.Bishop 
                    | KnightProm-> Chessmen.Knight)

            match newPieceOpt with
            | Some pc -> 
                p 
                |> clearPieceInternal (Chessmen.Pawn, side) dstBitRef
                |> setPieceInternal (pc, side) dstBitRef
            | _ -> p

        let increaseCountersAndSwapSide (p:Position) =
            //NB - takes advantage of the fact that this is the last step in the pipeline (including en passant)
            let isCapture =
                let originalOpponentBitboard = pos |> getBitboardForSide (opposite side)
                let opponentBitboardAfterMove = p |> getBitboardForSide (opposite side)
                opponentBitboardAfterMove <> originalOpponentBitboard
            
            let (isPawnMove, isPawnDoubleMove) = 
                match (chessman, side) with
                | (Chessmen.Pawn, Side.White) -> 
                    let isPawnDoubleMove = 
                        Rows.FourthRow |> BitUtils.hasBitSet dstBitRef && Rows.SecondRow |> BitUtils.hasBitSet srcBitRef
                    (true, isPawnDoubleMove)
                | (Chessmen.Pawn, Side.Black) -> 
                    let isPawnDoubleMove = 
                        Rows.FifthRow |> BitUtils.hasBitSet dstBitRef && Rows.SeventhRow |> BitUtils.hasBitSet srcBitRef
                    (true, isPawnDoubleMove)
                | (_, _) -> (false,false)

            let enPassantTarget = 
                match (isPawnDoubleMove, side) with
                | (true, White) ->
                    let potentialCaptorsBitboard = getEnPassantPotentialCaptorOfWhite dstBitRef
                    if pos.BlackPawns &&& potentialCaptorsBitboard > 0UL then dstBitRef-8 |> Some else None
                | (true, Black) -> 
                    let potentialCaptorsBitboard = getEnPassantPotentialCaptorOfBlack dstBitRef
                    if pos.WhitePawns &&& potentialCaptorsBitboard > 0UL then dstBitRef+8 |> Some else None
                | (false,_) -> None

            // Half Move clock rules - see https://www.chessprogramming.org/Halfmove_Clock
            let nextHalfMoveClock = if (isCapture || isPawnMove) then 0 else p.HalfMoveClock + 1

            let (nextMoveNumber, oppositeSide) = 
                match p.SideToPlay with 
                | White -> (p.FullMoveNumber, Black)
                | Black -> (p.FullMoveNumber+1, White)

            let updatedHash = 
                pos.HashKey ^^^ 
                (oppositeSide |> Zobrist.getSideToPlayHash ) ^^^ 
                (enPassantTarget |> Option.map Zobrist.getEnPassantHash |> Option.defaultValue 0UL)

            { p with 
                SideToPlay = oppositeSide;
                EnPassantTarget = enPassantTarget |> Option.defaultValue 0
                FullMoveNumber = nextMoveNumber; 
                HalfMoveClock = nextHalfMoveClock;
                HashKey = updatedHash }

        let isCastlingPreconditionsMet p = 
            match castlingTypeOpt with
            | Some castlingType -> 
                (p |> hasCastlingRights castlingType side) && not(p|> isCheck getAttacks)
            | None -> true

        let alignOtherPiecesForSpecialMovesFilter = clearOpponentPawnIfEnPassant >> alsoMoveRookIfCastling >> replacePawnIfPromotion

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
        |> Option.map increaseCountersAndSwapSide

    // Intended to be used for moves entered via UCI interface (i.e from chess GUI)
    let tryMakeMoveWithFullValidation (generatePseudoMoves:Position->int->Move array) (getAttacks:GetAttacks) (move:Move) (pos:Position) =
        let (srcBitRef, dstBitRef) = move |> Move.getSrcAndDestBitRefs
        
        let sideToPlayPredicate p = 
            let (_, side) = p |> getChessmanAndSide srcBitRef |> Option.get  
            side = p.SideToPlay
        
        let possibleMovePredicate p =
            let generatedPseudoMoves = 
                generatePseudoMoves pos srcBitRef 
                |> Array.map Move.getSrcAndDestBitRefs
            // printfn "Got following pseudo moves: %A" generatedPseudoMoves
            // printfn "Attempted move: %A" (srcBitRef,dstBitRef)
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

