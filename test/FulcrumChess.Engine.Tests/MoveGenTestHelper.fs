namespace FulcrumChess.Engine.Tests.MoveGeneration
open FulcrumChess.Engine
open FulcrumChess.Engine.Tests

[<AutoOpen>]
module MoveGenTestHelper =
   let setBitsToAlgebraicNotations (bitboard:Bitboard) =
        let targetBitRefs = bitboard |> BitUtils.getSetBits
        targetBitRefs |> Array.map Notation.bitRefToAlgebraicNotation

   let private promotionOptToLetter (promotionTypeOpt) =
      match promotionTypeOpt with
      | Some KnightProm -> "n"
      | Some BishopProm -> "b"
      | Some RookProm -> "r"
      | Some QueenProm -> "q"
      | None -> ""

   let private moveDstToAlgebraicNotation move =
      let mapDstToLetter = Move.getDestBitRef >> Notation.bitRefToAlgebraicNotation
      let mapPromotionTypeOptLetter = Move.getPromotionType >> promotionOptToLetter
      move |> Arrow.onSingleCombine (+) mapDstToLetter mapPromotionTypeOptLetter

   let movesToAlgebraicNotations (moves:Move array) =
      moves |> Array.map moveDstToAlgebraicNotation
         
  