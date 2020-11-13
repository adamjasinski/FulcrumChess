namespace FulcrumChess.Engine

type Move = uint16

type SpecialMoveType = |Conventional|Promotion|EnPassant|Castling
type CastlingType = |KingSide|QueenSide

module Move =

    // Follows Stockfish convention
    /// bit  0- 5: destination square (from 0 to 63)
    /// bit  6-11: origin square (from 0 to 63)
    /// bit 12-13: promotion piece type - 2 (from KNIGHT-2 to QUEEN-2)
    /// bit 14-15: special move flag: promotion (1), en passant (2), castling (3)
    /// NOTE: EN-PASSANT bit is set only when a pawn can be captured

    let inline create (srcBitRef:int, destBitRef:int) : Move =
        uint16 ((srcBitRef <<< 6) ||| destBitRef)

    let createSpecial (srcBitRef:int, destBitRef:int) (specialMoveType:SpecialMoveType) : Move =
        let basicBits = create (srcBitRef, destBitRef)
        let extraMask = 
            match specialMoveType with
            | Promotion -> 0x4000us
            | EnPassant -> 0x8000us
            | Castling -> 0xC000us
            | _ -> 0us
        basicBits ||| extraMask

    // let createCastling (castlingType:CastlingType) (side:Side) : Move =
    //     //uint16 ((srcBitRef <<< 6) ||| destBitRef)
    //    let castlingLookup = Position.castlingLookups.[side]
    //    let dst = match castlingLookup with
    //    | QueenSide -> castlingLookup

    let inline getDestBitRef (move:Move) =
        int(move &&& 0x3Fus)

    let inline getSrcBitRef (move:Move) =
        int((move &&& 0xFC0us) >>> 6)

    let getSrcAndDestBitRefs = 
        Arrow.onSingle getSrcBitRef getDestBitRef 

    let movesToDstBitboard (moves:Move array) =
        (0UL, moves) 
        ||> Array.fold (fun acc mv -> 
            acc |> BitUtils.setBit (getDestBitRef mv))

    let determineCastlingTypeIfKingsMove (side:Side) (move:Move) =
        let (srcBitRef, dstBitRef) = move |> getSrcAndDestBitRefs
        match side with
        | Side.White -> 
            match (srcBitRef, dstBitRef) with
            | (3, 1) -> KingSide |> Option.Some
            | (3, 5) -> QueenSide |> Option.Some
            | _ -> Option.None
        | Side.Black ->
            match (srcBitRef, dstBitRef) with
            | (59, 57) -> KingSide |> Option.Some
            | (59, 61) -> QueenSide |> Option.Some
            | _ -> Option.None

    let determineCastlingType (chessman:Chessmen, side:Side) (move:Move) =
        match chessman with
        | King -> determineCastlingTypeIfKingsMove side move
        | _ -> Option.None       


    