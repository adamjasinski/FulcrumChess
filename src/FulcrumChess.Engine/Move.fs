﻿namespace FulcrumChess.Engine

type Move = uint16

type PromotionType = |KnightProm|BishopProm|RookProm|QueenProm //:D (piece types only were confusing the compiler with Chessmen type)

type SpecialMoveType = |Conventional|Promotion of PromotionType|EnPassant|Castling

module Move =

    // Follows Stockfish convention
    /// bit  0- 5: destination square (from 0 to 63)
    /// bit  6-11: origin square (from 0 to 63)
    /// bit 12-13: promotion piece type - 2 (from KNIGHT- 0 to QUEEN-3)
    /// bit 14-15: special move flag: promotion (1), en passant (2), castling (3)
    /// NOTE: EN-PASSANT bit is set only when a pawn can be captured
    /// Slighly customized values for castling:
    /// - King side castling: bits 14-15 are set
    /// - Queen side castling bits 12-15 are set

    let inline create (srcBitRef:int, destBitRef:int) : Move =
        uint16 ((srcBitRef <<< 6) ||| destBitRef)

    let createSpecialFromExisting (move:Move) (specialMoveType:SpecialMoveType) : Move =
        let basicBits = move
        let extraMask = 
            let promotionTypeToMask = function
                | QueenProm -> 0x3000us
                | RookProm -> 0x2000us
                | BishopProm -> 0x1000us
                | KnightProm -> 0x0us
            match specialMoveType with
            | Promotion target -> 0x4000us ||| promotionTypeToMask target
            | EnPassant -> 0x8000us
            | Castling -> 0xC000us
            | _ -> 0x0us
        basicBits ||| extraMask

    let createSpecial (srcBitRef:int, destBitRef:int) (specialMoveType:SpecialMoveType) : Move =
        let basicBits = create (srcBitRef, destBitRef)
        createSpecialFromExisting basicBits specialMoveType

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
        getSrcBitRef .&&&. getDestBitRef 

    let movesToDstBitboard (moves:Move array) =
        (0UL, moves) 
        ||> Array.fold (fun acc mv -> 
            acc |> BitUtils.setBit (getDestBitRef mv))

    let isEnPassant (move:Move) =
        move &&& 0x8000us > 0us

    let getPromotionType (move:Move) =
        if move &&& 0x4000us > 0us then
            let promotionType = 
                match (move &&& 0x3000us) >>> 12 with
                | 0us -> PromotionType.KnightProm
                | 1us -> PromotionType.BishopProm
                | 2us -> PromotionType.RookProm
                | _ -> PromotionType.QueenProm
            promotionType |> Some
        else None

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

    let determineCastlingType (chessman:Chessmen, side:Side) (move:Move) =
        match chessman with
        | King -> determineCastlingTypeIfKingsMove side move
        | _ -> Option.None
