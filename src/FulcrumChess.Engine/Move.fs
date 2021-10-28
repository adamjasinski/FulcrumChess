namespace FulcrumChess.Engine

type Move = uint32

[<Struct>]
type PromotionType = |KnightProm|BishopProm|RookProm|QueenProm //:D (piece types only were confusing the compiler with Chessmen type)

[<Struct>]
type SpecialMoveType = |Conventional|Promotion of PromotionType|EnPassant|Castling

module Move =

    /// bit  0- 5: destination square (from 0 to 63)
    /// bit  6-11: origin square (from 0 to 63)
    /// bit 12-13: promotion piece type - 2 (from KNIGHT- 0 to QUEEN-3)
    /// bit 14-15: special move flag: promotion (1), en passant (2), castling (3)
    /// NOTE: EN-PASSANT bit is set only when a pawn can be captured
    /// Values for castling:
    /// - King side castling: bits 14-15 are set
    /// - Queen side castling bits 12-15 are set
    /// bit 16: capture
    
    let private captureMask = 1u <<< 16

    let inline create (srcBitRef:int, destBitRef:int) (isCapture:bool): Move =
        let maybeCaptureMask = if isCapture then captureMask else 0u
        uint32 (( srcBitRef <<< 6) ||| destBitRef) ||| maybeCaptureMask

    let createSpecialFromExisting (move:Move) (specialMoveType:SpecialMoveType) : Move =
        let basicBits = move
        let extraMask = 
            let promotionTypeToMask = function
                | QueenProm -> 0x3000u
                | RookProm -> 0x2000u
                | BishopProm -> 0x1000u
                | KnightProm -> 0x0u
            match specialMoveType with
            | Promotion target -> 0x4000u ||| promotionTypeToMask target
            | EnPassant -> 0x8000u
            | Castling -> 0xC000u
            | _ -> 0x0u
        basicBits ||| extraMask

    let createSpecial (srcBitRef:int, destBitRef:int) (isCapture:bool) (specialMoveType:SpecialMoveType) : Move =
        let basicBits = create (srcBitRef, destBitRef) isCapture
        createSpecialFromExisting basicBits specialMoveType

    let isCapture (move:Move) = move &&& captureMask > 0u

    // let createCastling (castlingType:CastlingType) (side:Side) : Move =
    //     //uint16 ((srcBitRef <<< 6) ||| destBitRef)
    //    let castlingLookup = Position.castlingLookups.[side]
    //    let dst = match castlingLookup with
    //    | QueenSide -> castlingLookup

    let inline getDestBitRef (move:Move) =
        int(move &&& 0x3Fu)

    let inline getSrcBitRef (move:Move) =
        int((move &&& 0xFC0u) >>> 6)

    let getSrcAndDestBitRefs = 
        getSrcBitRef .&&&. getDestBitRef 

    let movesToDstBitboard (moves:Move array) =
        (0UL, moves) 
        ||> Array.fold (fun acc mv -> 
            acc |> BitUtils.setBit (getDestBitRef mv))

    let isEnPassant (move:Move) =
        move &&& 0x8000u > 0u

    let getPromotionType (move:Move) =
        if move &&& 0x4000u > 0u then
            let promotionType = 
                match (move &&& 0x3000u) >>> 12 with
                | 0u -> PromotionType.KnightProm
                | 1u -> PromotionType.BishopProm
                | 2u -> PromotionType.RookProm
                | _ -> PromotionType.QueenProm
            promotionType |> Some
        else None

    let nullMove = 0u

type CastlingType = |KingSide|QueenSide

module Castling =
    let private determineCastlingTypeIfKingsMove (side:Side) (move:Move) =
        let (srcBitRef, dstBitRef) = move |> Move.getSrcAndDestBitRefs
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

    // Determines castling type if move castling flag isn't set explicitly
    let determineCastlingType (chessman:Chessmen, side:Side) (move:Move) =
        match chessman with
        | King -> determineCastlingTypeIfKingsMove side move
        | _ -> Option.None