namespace FenParserTests.NUnit.MoveGeneration
open FenParserTests.NUnit
open FenParser

module MoveGenTestHelper =
   let setBitsToAlgebraicNotations (bitboard:Bitboards.Bitboard) =
        let targetBitRefs = bitboard |> BitUtils.getSetBits
        targetBitRefs |> Array.map Bitboards.bitRefToAlgebraicNotation