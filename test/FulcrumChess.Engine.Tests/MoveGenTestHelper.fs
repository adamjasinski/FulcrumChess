namespace FulcrumChess.Engine.Tests.MoveGeneration
open FulcrumChess.Engine
open FulcrumChess.Engine.Tests

[<AutoOpen>]
module MoveGenTestHelper =
   let setBitsToAlgebraicNotations (bitboard:Bitboard) =
        let targetBitRefs = bitboard |> BitUtils.getSetBits
        targetBitRefs |> Array.map Notation.bitRefToAlgebraicNotation