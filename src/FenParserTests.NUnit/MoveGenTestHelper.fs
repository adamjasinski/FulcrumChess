namespace FenParserTests.NUnit.MoveGeneration
open FenParserTests.NUnit
open FenParser

module MoveGenTestHelper =
   let setBitsToAlgebraicNotations (bitboard:Bitboard) =
        let targetBitRefs = bitboard |> BitUtils.getSetBits
        targetBitRefs |> Array.map Bitboards.bitRefToAlgebraicNotation