﻿namespace FulcrumChess.Engine.Tests.MoveGeneration
open FulcrumChess.Engine
open FulcrumChess.Engine.Tests

module MoveGenTestHelper =
   let setBitsToAlgebraicNotations (bitboard:Bitboard) =
        let targetBitRefs = bitboard |> BitUtils.getSetBits
        targetBitRefs |> Array.map Bitboards.bitRefToAlgebraicNotation