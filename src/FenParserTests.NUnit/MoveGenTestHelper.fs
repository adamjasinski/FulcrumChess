namespace FenParserTests.NUnit.MoveGeneration
open FenParserTests.NUnit
open FenParser

module MoveGenTestHelper =
    let generateRookMovesViaBitboards (allPieces:Bitboards.Bitboard) (friendlyPieces:Bitboards.Bitboard) startBitref =
        let rookMagicMovesDb = Bitboards.bootstrapRookMagicMoves()
        Bitboards.generateMovesForPosition Pieces.SlidingPiece.Rook rookMagicMovesDb allPieces friendlyPieces startBitref Magic.PregeneratedMagic.magicNumbersAndShiftsRook

    let setBitsToAlgebraicNotations (bitboard:Bitboards.Bitboard) =
        let targetBitRefs = bitboard |> BitUtils.getSetBits
        targetBitRefs |> Array.map Bitboards.bitRefToAlgebraicNotation