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
    KingSideCastlingMove:Move;
    QueenSideCastlingMove:Move; }

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
         WhiteCastlingRights=CastlingRights.Both;
         BlackCastlingRights=CastlingRights.Both;
         SideToPlay=White;
         EnPassantTarget=0;
         HalfMoveClock=0;
         FullMoveNumber=1; 
         HashKey=6346835357807462450UL }

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
            KingSideCastlingMove = Move.createCastling struct(White, CastlingType.KingSide);
            QueenSideCastlingMove = Move.createCastling struct(White, CastlingType.QueenSide);
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
            KingSideCastlingMove = Move.createCastling struct(Black, CastlingType.KingSide);
            QueenSideCastlingMove = Move.createCastling struct(Black, CastlingType.QueenSide);
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

    let getBitboardForOppositeSide (pos:Position) =
        pos |> getBitboardForSide (opposite pos.SideToPlay)
        
    let setFenPiece (piece:char) (bitRef:int) (pos:Position) =
        if bitRef < 0 || bitRef > 63 then invalidArg "bitRef" ("parameter has invalid value: " + bitRef.ToString())
        let candidate:Bitboard = 0UL |> BitUtils.setBit bitRef
        let updateWithTargetChessmanHash pcAndSide = 
            //printfn "XORing with pc hash %A %d" (pc, pos.SideToPlay) bitRef
            pos.HashKey ^^^ Zobrist.getChessmanHash pcAndSide bitRef
        let a= struct(Chessmen.Pawn, Side.Black)
        let pos' = 
            match piece with
            | 'p' -> {pos with BlackPawns=pos.BlackPawns ||| candidate; HashKey=updateWithTargetChessmanHash struct(Chessmen.Pawn, Side.Black)}
            | 'n' -> {pos with BlackKnights=pos.BlackKnights ||| candidate; HashKey=updateWithTargetChessmanHash struct(Chessmen.Knight, Side.Black) }
            | 'b' -> {pos with BlackBishops=pos.BlackBishops ||| candidate; HashKey=updateWithTargetChessmanHash struct(Chessmen.Bishop, Side.Black) }
            | 'r' -> {pos with BlackRooks=pos.BlackRooks ||| candidate; HashKey=updateWithTargetChessmanHash struct(Chessmen.Rook, Side.Black) }
            | 'q' -> {pos with BlackQueen=pos.BlackQueen ||| candidate; HashKey=updateWithTargetChessmanHash struct(Chessmen.Queen, Side.Black) }
            | 'k' -> {pos with BlackKing=pos.BlackKing ||| candidate; HashKey=updateWithTargetChessmanHash struct(Chessmen.King, Side.Black) }
            | 'P' -> {pos with WhitePawns=pos.WhitePawns ||| candidate; HashKey=updateWithTargetChessmanHash struct(Chessmen.Pawn, Side.White) }
            | 'N' -> {pos with WhiteKnights=pos.WhiteKnights ||| candidate; HashKey=updateWithTargetChessmanHash struct(Chessmen.Knight, Side.White) }
            | 'B' -> {pos with WhiteBishops=pos.WhiteBishops ||| candidate; HashKey=updateWithTargetChessmanHash struct(Chessmen.Bishop, Side.White) }
            | 'R' -> {pos with WhiteRooks=pos.WhiteRooks ||| candidate; HashKey=updateWithTargetChessmanHash struct(Chessmen.Rook, Side.White) }
            | 'Q' -> {pos with WhiteQueen=pos.WhiteQueen ||| candidate; HashKey=updateWithTargetChessmanHash struct(Chessmen.Queen, Side.White)}
            | 'K' -> {pos with WhiteKing=pos.WhiteKing ||| candidate; HashKey=updateWithTargetChessmanHash struct(Chessmen.King, Side.White)}
            | _ -> invalidArg "piece" ("parameter has invalid value: " + piece.ToString())
        pos'

    let asBitboardSequence (pos:Position) =
        seq {
            yield struct(pos.BlackPawns, struct(Chessmen.Pawn, Black))
            yield struct(pos.BlackRooks, struct(Chessmen.Rook, Black))
            yield struct(pos.BlackKnights, struct(Chessmen.Knight, Black))
            yield struct(pos.BlackBishops, struct(Chessmen.Bishop, Black))
            yield struct(pos.BlackQueen, struct(Chessmen.Queen, Black))
            yield struct(pos.BlackKing, struct(Chessmen.King, Black))
            yield struct(pos.WhiteRooks, struct(Chessmen.Rook, White))
            yield struct(pos.WhiteKnights, struct(Chessmen.Knight, White))
            yield struct(pos.WhiteBishops, struct(Chessmen.Bishop, White))
            yield struct(pos.WhiteQueen, struct(Chessmen.Queen, White))
            yield struct(pos.WhiteKing, struct(Chessmen.King, White))
            yield struct(pos.WhitePawns, struct(Chessmen.Pawn, White))
        }

    let getKingBitboard (side:Side) (pos:Position) =
        match side with
        | Side.White -> pos.WhiteKing
        | Side.Black -> pos.BlackKing

    let getRooksBitboard (side:Side) (pos:Position) =
        match side with
        | Side.White -> pos.WhiteRooks
        | Side.Black -> pos.BlackRooks

    let getChessmanAndSide (bitRef:int) (pos:Position) : struct(Chessmen*Side) voption =
        let hasBitRef (bitboard:Bitboard) = bitboard |> BitUtils.hasBitSet bitRef
        if pos.BlackPawns |> hasBitRef then struct(Chessmen.Pawn, Black) |> ValueSome
        elif pos.WhitePawns  |> hasBitRef then struct(Chessmen.Pawn, White)  |> ValueSome
        elif pos.BlackQueen |> hasBitRef then  struct(Chessmen.Queen, Black)  |> ValueSome
        elif pos.WhiteQueen  |> hasBitRef then  struct(Chessmen.Queen, White)  |> ValueSome
        elif pos.BlackRooks |> hasBitRef then struct(Chessmen.Rook, Black)  |> ValueSome
        elif pos.WhiteRooks  |> hasBitRef then  struct(Chessmen.Rook, White)  |> ValueSome
        elif pos.BlackKnights |> hasBitRef then struct(Chessmen.Knight, Black)  |> ValueSome
        elif pos.WhiteKnights  |> hasBitRef then  struct(Chessmen.Knight, White)  |> ValueSome
        elif pos.BlackBishops |> hasBitRef then struct(Chessmen.Bishop, Black)  |> ValueSome
        elif pos.WhiteBishops  |> hasBitRef then  struct(Chessmen.Bishop, White)  |> ValueSome
        elif pos.BlackKing  |> hasBitRef then  struct(Chessmen.King, Black)  |> ValueSome
        elif pos.WhiteKing  |> hasBitRef then  struct(Chessmen.King, White)  |> ValueSome
        else ValueNone
       
    let getCapturesFromPseudoMoves (movesBitboard:Bitboard) (bitRef:int) (pos:Position) =
        let struct(_, side) = pos |> getChessmanAndSide bitRef |> ValueOption.get
        let opponentPieces = pos |> getBitboardForSide (opposite side) 
        movesBitboard &&& opponentPieces


    // let bitboardToLerbefArray (bitboard:Bitboard) =
    //     let arr = Array.zeroCreate<byte> 64
    //     bitboard 
    //     |> BitUtils.getSetBits
    //     |> Array.iter( fun bitRef -> arr.[bitRef] <- 1uy)
    //     arr

    let private setPieceInternal struct(piece:Chessmen, side:Side) (bitRef:int) (pos:Position) =
        let fenLetter = struct(piece, side) |> PieceFenLetters.getLetter
        pos |> setFenPiece fenLetter bitRef

    let private clearPieceInternal (piece:Chessmen, side:Side) (bitRef:int) (pos:Position) =
        if bitRef < 0 || bitRef > 63 then invalidArg "bitRef" ("parameter has invalid value: " + bitRef.ToString())
        let clearBitRef = BitUtils.clearBit bitRef
        //let updatedHash = pos.HashKey ^^^ Zobrist.getEmptyFieldHash bitRef
        //printfn "XORing with pc hash %A %d" (piece, side) bitRef
        let updatedHash = pos.HashKey ^^^ Zobrist.getChessmanHash struct(piece, side) bitRef

        let pos' = 
            match piece,side with
            | (Chessmen.Pawn, Side.Black) -> { pos with BlackPawns=pos.BlackPawns |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Knight, Side.Black) -> { pos with BlackKnights=pos.BlackKnights |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Bishop, Side.Black) -> { pos with BlackBishops=pos.BlackBishops |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Rook, Side.Black) -> { pos with BlackRooks=pos.BlackRooks |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Queen, Side.Black) -> { pos with BlackQueen=pos.BlackQueen |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.King, Side.Black) -> { pos with BlackKing=pos.BlackKing |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Pawn, Side.White) -> { pos with WhitePawns=pos.WhitePawns |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Knight, Side.White) -> { pos with WhiteKnights=pos.WhiteKnights |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Bishop, Side.White) -> { pos with WhiteBishops=pos.WhiteBishops |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Rook, Side.White) -> { pos with WhiteRooks=pos.WhiteRooks |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.Queen, Side.White) -> { pos with WhiteQueen=pos.WhiteQueen |> clearBitRef; HashKey=updatedHash }
            | (Chessmen.King, Side.White) -> { pos with WhiteKing=pos.WhiteKing |> clearBitRef; HashKey=updatedHash }
        pos'

    
    let dumpPosition (pos:Position) =
        [|for i in 7..-1..0 ->
            [|for j in 7..-1..0 ->
                let pc = pos |> getChessmanAndSide (8*i+j)
                match pc with
                | ValueSome(chessmanAndSide) -> PieceFenLetters.getLetter chessmanAndSide
                | ValueNone -> ' '
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
        let newCastlingRightsPair = 
            match side with
            | White -> (castlingRights, pos.BlackCastlingRights)
            | Black -> (pos.WhiteCastlingRights, castlingRights)
        let updatedHashKey = 
                pos.HashKey ^^^
                ((pos.WhiteCastlingRights, pos.BlackCastlingRights) |> Zobrist.getCastlingRightsHash) ^^^ //clear previous
                (newCastlingRightsPair |> Zobrist.getCastlingRightsHash)   //set current

        match side with
            | White -> { pos with WhiteCastlingRights = castlingRights; HashKey = updatedHashKey }
            | Black -> { pos with BlackCastlingRights = castlingRights; HashKey = updatedHashKey }

    let calculateZobristHash (pos:Position) =
        let castlingRightsHash = (pos.WhiteCastlingRights, pos.BlackCastlingRights)|> Zobrist.getCastlingRightsHash
        let enPassantsHash = pos.EnPassantTarget |> Zobrist.getEnPassantHash
        let sideHash = pos.SideToPlay |> Zobrist.getSideToPlayHash
        let chessmenHash = 
            pos 
            |> asBitboardSequence
            |> Seq.collect (fun struct(bb, pcAndSide) ->
                    let bitRefs = BitUtils.getSetBits_u64 bb
                    bitRefs |> Array.map (Zobrist.getChessmanHash pcAndSide))
            |> Seq.fold (^^^) 0UL
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
        let struct(srcBitRef, dstBitRef) = move |> Move.getSrcAndDestBitRefs
        let struct(chessman, side) = pos |> getChessmanAndSide srcBitRef |> ValueOption.get

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
            if dstSquare |> ValueOption.isNone then p
            else
                let struct(opponentPiece,opponentSide) = dstSquare |> ValueOption.get
                if(opponentSide <> (side |> opposite)) then illegalMove "Error: Move destination targets a friendly piece"
                p |> clearPieceInternal (opponentPiece, opponentSide) dstBitRef

        let clearOpponentPawnIfEnPassant (p:Position) =
            if move |> Move.isEnPassant then
                let opponentPawnToClearBitRef = 
                    match side with
                    | White -> dstBitRef-8
                    | Black -> dstBitRef+8
                let opponentPawnToClearSquare = pos |> getChessmanAndSide opponentPawnToClearBitRef
                if opponentPawnToClearSquare |> ValueOption.isNone then 
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

            //let previousEnPassantHash = pos.EnPassantTarget |> Zobrist.getEnPassantHash
            let enPassantHashUpd = 
                (pos.EnPassantTarget |> Zobrist.getEnPassantHash) ^^^  //clear previous en passant target hash
                (enPassantTarget |> Option.map Zobrist.getEnPassantHash |> Option.defaultValue 0UL) //set current

            let updatedHash = 
                p.HashKey ^^^ 
                Zobrist.sideHash ^^^  //flipping the side
                enPassantHashUpd

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

    let isCheckMate (generateAllPseudoMovesForSide:GetMovesForSide) (getAttacks:GetAttacks) (pos:Position) =
        pos 
        |> generateAllPseudoMovesForSide
        |> Array.exists (fun m -> 
            tryMakeMoveInternal getAttacks m pos |> Option.isSome)
        |> not
        
    // Intended to be used for moves entered via UCI interface (i.e from chess GUI)
    let tryMakeMoveWithFullValidation (generatePseudoMoves:Position->int->Move array) (getAttacks:GetAttacks) (uciMove:UciMove) (pos:Position) =
        
        let struct(srcBitRef, dstBitRef, promotionOpt) = Notation.fromLongAlgebraicNotationToBitRefs uciMove.LongAlgebraic

        let deconstructMove (m:Move) =
            let struct(src,dst) = m |> Move.getSrcAndDestBitRefs
            let promo = m |> Move.getPromotionType
            struct(src, dst, promo) 

        let pseudoMovesFromEngine = generatePseudoMoves pos srcBitRef
        let matchingMoveFromEngine =
            pseudoMovesFromEngine |> Array.tryFind(fun m ->
                (m |> deconstructMove) = (srcBitRef, dstBitRef, promotionOpt))


        let legalMoveFilter m = tryMakeMoveInternal getAttacks m pos

        matchingMoveFromEngine
        |> Option.bind legalMoveFilter