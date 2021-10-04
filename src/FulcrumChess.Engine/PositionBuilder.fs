namespace FulcrumChess.Engine

[<AutoOpen>]
module ByRefExtensions =
    let inline xorv (x : byref<_>) i = x <- x ^^^ i
    //let inline setv (x : byref<_>) i = x <- x ||| i
    let inline setBitRef (bb : byref<_>) bitRef = 
        bb <- bb |> BitUtils.setBit bitRef
    let inline clearBitRef (bb : byref<_>) bitRef = 
        bb <- bb |> BitUtils.clearBit bitRef


type PositionBuilder(pos:Position) =
    let mutable whiteKing = pos.WhiteKing
    let mutable whiteQueen = pos.WhiteQueen
    let mutable whiteRooks = pos.WhiteRooks
    let mutable whiteBishops = pos.WhiteBishops
    let mutable whiteKnights = pos.WhiteKnights
    let mutable whitePawns = pos.WhitePawns
    let mutable blackKing = pos.BlackKing
    let mutable blackQueen = pos.BlackQueen
    let mutable blackRooks = pos.BlackRooks
    let mutable blackBishops = pos.BlackBishops
    let mutable blackKnights = pos.BlackKnights
    let mutable blackPawns = pos.BlackPawns
    let mutable whiteCastlingRigts = pos.WhiteCastlingRights
    let mutable blackCastlingRigts = pos.BlackCastlingRights
    let mutable sideToPlay = pos.SideToPlay
    let mutable enPassantTarget = pos.EnPassantTarget
    let mutable halfMoveClock = pos.HalfMoveClock
    let mutable fullMoveNumber = pos.FullMoveNumber
    let mutable hashKey = pos.HashKey

    member __.ToPosition() = {
        WhiteKing = whiteKing
        WhiteQueen = whiteQueen
        WhiteRooks = whiteRooks
        WhiteBishops = whiteBishops
        WhiteKnights = whiteKnights
        WhitePawns = whitePawns
        BlackKing = blackKing
        BlackQueen = blackQueen
        BlackRooks = blackRooks
        BlackBishops = blackBishops
        BlackKnights = blackKnights 
        BlackPawns = blackPawns
        WhiteCastlingRights = whiteCastlingRigts
        BlackCastlingRights = blackCastlingRigts
        SideToPlay = sideToPlay
        EnPassantTarget = enPassantTarget
        HalfMoveClock = halfMoveClock
        FullMoveNumber = fullMoveNumber
        HashKey = hashKey }

    // let updateWithTargetChessmanHash pcAndSide = 
    //     //printfn "XORing with pc hash %A %d" (pc, pos.SideToPlay) bitRef
    //     hashKey <- hashKey ^^^ Zobrist.getChessmanHash pcAndSide bitRef

    // member this.BlackKing
    //     with get() = blackKing
    //     with set(value) = 
    //         blackKing <- value
    //         update

    member __.ClearPiece struct(piece:Chessmen, side:Side) (bitRef:int) =
        if bitRef < 0 || bitRef > 63 then invalidArg "bitRef" ("parameter has invalid value: " + bitRef.ToString())

        // let updateWithTargetChessmanHash pcAndSide = 
        //     //printfn "XORing with pc hash %A %d" (pc, pos.SideToPlay) bitRef
        //     hashKey <- hashKey ^^^ Zobrist.getChessmanHash pcAndSide bitRef
        // //let updatedHash = pos.HashKey ^^^ Zobrist.getEmptyFieldHash bitRef
        // //printfn "XORing with pc hash %A %d" (piece, side) bitRef
        //let updatedHash = hashKey ^^^ Zobrist.getChessmanHash struct(piece, side) bitRef

        match piece,side with
        | (Chessmen.Pawn, Side.Black) -> clearBitRef &blackPawns bitRef
        | (Chessmen.Knight, Side.Black) -> clearBitRef &blackKnights bitRef
        | (Chessmen.Bishop, Side.Black) -> clearBitRef &blackBishops bitRef
        | (Chessmen.Rook, Side.Black) -> clearBitRef &blackRooks bitRef
        | (Chessmen.Queen, Side.Black) -> clearBitRef &blackQueen bitRef
        | (Chessmen.King, Side.Black) -> clearBitRef &blackKing bitRef
        | (Chessmen.Pawn, Side.White) -> clearBitRef &whitePawns bitRef
        | (Chessmen.Knight, Side.White) -> clearBitRef &whiteKnights bitRef
        | (Chessmen.Bishop, Side.White) -> clearBitRef &whiteBishops bitRef
        | (Chessmen.Rook, Side.White) -> clearBitRef &whiteRooks bitRef
        | (Chessmen.Queen, Side.White) -> clearBitRef &whiteQueen bitRef
        | (Chessmen.King, Side.White) -> clearBitRef &whiteKing bitRef

        hashKey <- hashKey ^^^ Zobrist.getChessmanHash struct(piece, side) bitRef


    member this.SetPiece struct(piece:Chessmen, side:Side) (bitRef:int) =
        let fenLetter = struct(piece, side) |> PieceFenLetters.getLetter
        this.SetFenPiece fenLetter bitRef

    member __.SetFenPiece (piece:char) (bitRef:int) =
        if bitRef < 0 || bitRef > 63 then invalidArg "bitRef" ("parameter has invalid value: " + bitRef.ToString())
        let candidate:Bitboard = 0UL |> BitUtils.setBit bitRef

        let updateWithTargetChessmanHash pcAndSide = 
            //printfn "XORing with pc hash %A %d" (pc, pos.SideToPlay) bitRef
            hashKey <- hashKey ^^^ Zobrist.getChessmanHash pcAndSide bitRef
       
        //let a= struct(Chessmen.Pawn, Side.Black)
        match piece with
        | 'p' -> xorv &blackPawns candidate; updateWithTargetChessmanHash struct(Chessmen.Pawn, Side.Black)
        | 'n' -> xorv &blackKnights candidate; updateWithTargetChessmanHash struct(Chessmen.Knight, Side.Black)
        | 'b' -> xorv &blackBishops candidate; updateWithTargetChessmanHash struct(Chessmen.Bishop, Side.Black)
        | 'r' -> xorv &blackRooks candidate; updateWithTargetChessmanHash struct(Chessmen.Rook, Side.Black)
        | 'q' -> xorv &blackQueen candidate; updateWithTargetChessmanHash struct(Chessmen.Queen, Side.Black)
        | 'k' -> xorv &blackKing candidate; updateWithTargetChessmanHash struct(Chessmen.King, Side.Black)
        | 'P' -> xorv &whitePawns candidate; updateWithTargetChessmanHash struct(Chessmen.Pawn, Side.White)
        | 'N' -> xorv &whiteKnights candidate; updateWithTargetChessmanHash struct(Chessmen.Knight, Side.White)
        | 'B' -> xorv &whiteBishops candidate; updateWithTargetChessmanHash struct(Chessmen.Bishop, Side.White)
        | 'R' -> xorv &whiteRooks candidate; updateWithTargetChessmanHash struct(Chessmen.Rook, Side.White)
        | 'Q' -> xorv &whiteQueen candidate; updateWithTargetChessmanHash struct(Chessmen.Queen, Side.White)
        | 'K' -> xorv &whiteKing candidate; updateWithTargetChessmanHash struct(Chessmen.King, Side.White)
        | _ -> invalidArg "piece" ("parameter has invalid value: " + piece.ToString())
    
