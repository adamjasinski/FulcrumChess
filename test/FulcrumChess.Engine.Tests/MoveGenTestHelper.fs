namespace FulcrumChess.Engine.Tests.MoveGeneration
open FulcrumChess.Engine
open FulcrumChess.Engine.Tests

[<AutoOpen>]
module MoveGenTestHelper =
   let setBitsToAlgebraicNotations (bitboard:Bitboard) =
        let targetBitRefs = bitboard |> BitUtils.getSetBits
        targetBitRefs |> Array.map Notation.bitRefToAlgebraicNotation

   let movesToAlgebraicNotations (moves:Move array) =
      moves |> Array.map (
         Move.getDestBitRef >> Notation.bitRefToAlgebraicNotation) 
         
  