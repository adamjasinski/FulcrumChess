namespace FenParserTests

module MoveGenTests =
    open FenParser
    open Swensen.Unquote
    open Xunit

    let generateViaBoard64 (occupancy:Board64.T) pieceCode startSquareAlg =
        let pieceColorInsensitive = pieceCode |> Pieces.fenCharacterAsNumber |> Pieces.numberAsPieceCodeColorInsensitive
        let startSquareIndex = Board64.algebraicStringAsSquareIndex startSquareAlg
        let board64Functs = { Moves.BoardFuncts.IsOnBoard=Board64.isOnBoard; Moves.MakeBitboard=Board64.makeBitboardFromIndexes; Moves.Move=Board64.moveTo}
        //TODO
        Moves.BitboardGenerator.generateRayBitboardsForSlidingPiece

    let generateRookMovesViaBitboards (allPieces:Bitboards.Bitboard) (friendlyPieces:Bitboards.Bitboard) startBitref =
        let rookMagicMovesDb = Bitboards.bootstrapRookMagicMoves()
        Bitboards.generateMovesForPosition Pieces.SlidingPiece.Rook rookMagicMovesDb allPieces friendlyPieces startBitref Magic.PregeneratedMagic.magicNumbersAndShiftsRook

//    [<Theory;MoveGenTestDataFile("MoveGenTestData.txt")>]
//    let DumpAllSamplesFromTheFile(record:MoveGenTestRecord) =
//       printfn "%s %s %A" record.Header record.FEN record.ExpectedMoves 
//        //TODO
//        ()

    let setBitsToAlgebraicNotations (bitboard:Bitboards.Bitboard) =
        let targetBitRefs = bitboard |> BitUtils.getSetBits
        targetBitRefs |> Array.map Bitboards.bitRefToAlgebraicNotation

    [<Fact>]
    let ``verify moves of first variant of rook on h1 with no other occupancy`` () =
        let rookMagicMovesDb = Bitboards.bootstrapRookMagicMoves()
        let moves = rookMagicMovesDb.[0].[0]
        let algNotations = moves |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["a1";"b1";"c1";"d1";"e1";"f1";"g1";"h2";"h3";"h4";"h5";"h6";"h7";"h8"] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>

    [<Fact>]
    let ``verify moves of Black Rook at c6; own pawn at c2`` () =
        let startBitRef = 45
        let opponentOccupancy = 0UL
        let friendlyOccupancy = 1UL <<< startBitRef ||| 1UL <<< 13
        let allOccupancy = opponentOccupancy ||| friendlyOccupancy
        
        let result = generateRookMovesViaBitboards allOccupancy friendlyOccupancy startBitRef
        test <@ result <> 0UL @>
        let algNotations = result |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["c3";"c4";"c5";"c7";"c8";"a6";"b6";"d6";"e6";"f6";"g6";"h6"] |> Set.ofList
        test <@ algNotations |> Array.exists (fun x -> x = "f6") @>
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>

    [<Fact>]
    let ``verify moves of Black Rook at c6; white pawn at c2`` () =
        let startBitRef = 45
        let opponentOccupancy = 1UL <<< 13
        let friendlyOccupancy = 1UL <<< startBitRef
        let allOccupancy = opponentOccupancy ||| friendlyOccupancy
        
        let result = generateRookMovesViaBitboards allOccupancy friendlyOccupancy startBitRef
        test <@ result <> 0UL @>
        let algNotations = result |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["c2";"c3";"c4";"c5";"c7";"c8";"a6";"b6";"d6";"e6";"f6";"g6";"h6"] |> Set.ofList
        test <@ algNotations |> Array.exists (fun x -> x = "f6") @>
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>

    //skip - not working; using NUnit instead anyway
    //[<Fact>]
    let ``verify moves of Black Rook at a5; a few other black and white pieces on the board`` () =
        let startBitRef = 39    //a5
        let opponentOccupancy = 1UL <<< 36  //d5
        let friendlyOccupancy = (1UL <<< startBitRef) ||| (1UL <<< 55) ||| (1UL <<< 15)    //a5, a7, a2
        let allOccupancy = opponentOccupancy ||| friendlyOccupancy
        
        let result = generateRookMovesViaBitboards allOccupancy friendlyOccupancy startBitRef
        test <@ result <> 0UL @>
        let algNotations = result |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["a3";"a4";"a6";"b5";"c5";"d5"] |> Set.ofList
        test <@ algNotations |> Array.exists (fun x -> x = "f6") @>
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>
