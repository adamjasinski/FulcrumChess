namespace FulcrumChess.Engine

type Move = uint32

[<Struct>]
type PromotionType = |KnightProm|BishopProm|RookProm|QueenProm //:D (piece types only were confusing the compiler with Chessmen type)

[<Struct>]
type SpecialMoveType = |Conventional|Promotion of PromotionType|EnPassant|Castling

[<Struct>]
type CastlingType = |KingSide|QueenSide


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
    
    module Masks =
        let captureMask = 1u <<< 16
        let [<Literal>] specialMoveProbe = 0xC000u
        let [<Literal>] castling = 0xC000u
        let [<Literal>] enPassant = 0x8000u
        let [<Literal>] promGeneral = 0x4000u
        let [<Literal>] queenProm = 0x3000u
        let [<Literal>] rookProm = 0x2000u
        let [<Literal>] bishopProm = 0x1000u
        let [<Literal>] knightProm = 0x0u
        let [<Literal>] srcBits = 0xFC0u
        let [<Literal>] dstBits = 0x3Fu

    let inline create struct(srcBitRef:int, destBitRef:int) (isCapture:bool): Move =
        let maybeCaptureMask = if isCapture then Masks.captureMask else 0u
        uint32 ((srcBitRef <<< 6) ||| destBitRef) ||| maybeCaptureMask

    let createCastling = function
        | struct(Side.White, CastlingType.KingSide) -> Masks.castling ||| create struct(3,1) false
        | struct(Side.White, CastlingType.QueenSide) -> Masks.castling ||| create struct(3,5) false
        | struct(Side.Black, CastlingType.KingSide) -> Masks.castling ||| create struct(59,57) false
        | struct(Side.Black, CastlingType.QueenSide) -> Masks.castling ||| create struct(59,61) false

    let createEnPassant struct(srcBitRef:int, destBitRef:int) =
        Masks.enPassant ||| create (srcBitRef, destBitRef) true

    let createPromotion (move:Move) (promotionType:PromotionType) =
        let promotionTypeMask = 
            match promotionType with
            | QueenProm -> Masks.queenProm
            | RookProm -> Masks.rookProm
            | BishopProm -> Masks.bishopProm
            | KnightProm -> Masks.knightProm
        move ||| Masks.promGeneral ||| promotionTypeMask

    let inline isCapture (move:Move) = move &&& Masks.captureMask > 0u

    let inline isCastling (move:Move) = move &&& Masks.specialMoveProbe = Masks.castling

    let inline isEnPassant (move:Move) =
        move &&& Masks.specialMoveProbe = Masks.enPassant

    let inline isPromotion (move:Move) =
        move &&& Masks.specialMoveProbe = Masks.promGeneral

    let inline getDestBitRef (move:Move) =
        int(move &&& Masks.dstBits)

    let inline getSrcBitRef (move:Move) =
        int((move &&& Masks.srcBits) >>> 6)

    let getSrcAndDestBitRefs = 
        getSrcBitRef .&&&. getDestBitRef 

    let movesToDstBitboard (moves:Move array) =
        (0UL, moves) 
        ||> Array.fold (fun acc mv -> 
            acc |> BitUtils.setBit (getDestBitRef mv))
    
    let getPromotionType (move:Move) =
        if move |> isPromotion then
            let promotionType = 
                match (move &&& 0x3000u) >>> 12 with
                | 0u -> PromotionType.KnightProm
                | 1u -> PromotionType.BishopProm
                | 2u -> PromotionType.RookProm
                | _ -> PromotionType.QueenProm
            promotionType |> Some
        else None

    let nullMove = 0u

module Castling =
    let private determineCastlingTypeIfKingsMove (side:Side) (move:Move) =
        let struct(srcBitRef, dstBitRef) = move |> Move.getSrcAndDestBitRefs
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


//  module MoveScore =
//     let applyScore (move:Move) =
//         let score1 m = m |>
    //Capture, Promotions, Castling?, 