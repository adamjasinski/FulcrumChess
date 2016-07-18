namespace FenParserTests

module MoveGenTests =

    open Xunit
    open TestHelpers
    open FenParser
    open Swensen.Unquote

    let generateViaBoard64 (occupancy:Board64.T) pieceCode startSquareAlg =
        let pieceColorInsensitive = pieceCode |> Pieces.fenCharacterAsNumber |> Pieces.numberAsPieceCodeColorInsensitive
        let startSquareIndex = Board64.algebraicStringAsSquareIndex startSquareAlg
        let board64Functs = { Moves.BoardFuncts.IsOnBoard=Board64.isOnBoard; Moves.MakeBitboard=Board64.makeBitboardFromIndexes; Moves.Move=Board64.moveTo}
        //TODO
        Moves.BitboardGenerator.generateRayBitboardsForSlidingPiece

    let generateRookMovesViaBitboards (allPieces:Bitboards.Bitboard) (friendlyPieces:Bitboards.Bitboard) startBitref =
        let rookMagicMovesDb = Bitboards.bootstrapRookDatabase()
        Bitboards.generateRookMoveDestinationBitboard rookMagicMovesDb allPieces friendlyPieces startBitref

    [<Theory;MoveGenTestDataFileAttribute("MoveGenTestData.txt")>]
    let DumpAllSamplesFromTheFile(record:MoveGenTestRecord) =
        printfn "%s %s %A" record.Header record.FEN record.ExpectedMoves 
        //TODO
        ()

    //[<Fact>]
    let ``verify moves of Black Rook at c6; white pawn at c2`` ()   =
        let startBitRef = 18
        let opponentOccupancy = 1L <<< 23
        let friendlyOccupancy = 1L <<< startBitRef
        let allOccupancy = opponentOccupancy ||| friendlyOccupancy
        
        let result = generateRookMovesViaBitboards allOccupancy friendlyOccupancy startBitRef
        printfn "%s" "Got raw results"
        test <@ result <> 0L @>
        // let targetBitRefs = result |> Bitboards.getSetBits
        // let algNotations = targetBitRefs |> Array.map Bitboards.bitRefToAlgebraicNotation
        // printfn "%A" (algNotations)
        // test <@ algNotations |> Array.exists (fun x -> x = "f6") @>
        // ()
