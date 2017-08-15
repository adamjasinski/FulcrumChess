namespace FenParserTests.NUnit.MoveGeneration

module MoveGenRookTests =

    open NUnit.Framework
    //open TestHelpers
    open FenParser
    open Swensen.Unquote

    let generateRookMovesViaBitboards (allPieces:Bitboards.Bitboard) (friendlyPieces:Bitboards.Bitboard) startBitref =
        let rookMagicMovesDb = Bitboards.bootstrapRookMagicMoves()
        Bitboards.generateRookMovesForPosition rookMagicMovesDb allPieces friendlyPieces startBitref Magic.PregeneratedMagic.magicNumbersAndShiftsRook

    let setBitsToAlgebraicNotations (bitboard:Bitboards.Bitboard) =
        let targetBitRefs = bitboard |> BitUtils.getSetBits
        targetBitRefs |> Array.map Bitboards.bitRefToAlgebraicNotation

    [<TestCase>]
    let ``verify moves of first variant of rook on h1 with no other occupancy`` () =
        let rookMagicMovesDb = Bitboards.bootstrapRookMagicMoves()
        let moves = rookMagicMovesDb.[0].[0]
        let algNotations = moves |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["a1";"b1";"c1";"d1";"e1";"f1";"g1";"h2";"h3";"h4";"h5";"h6";"h7";"h8"] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>

    [<TestCase>]
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

    [<TestCase>]
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

    [<TestCase>]
    let ``verify moves of Black Rook at a5; a few other black and white pieces on the board`` () =
        //TODO - the problem here is that the pre-generated magic has been generated with a different Shift&Multiply function
        let startBitRef = 39    //a5
        let opponentOccupancy = 1UL <<< 36  //d5
        let friendlyOccupancy = (1UL <<< startBitRef) ||| (1UL <<< 55) ||| (1UL <<< 15)    //a5, a7, a2
        let allOccupancy = opponentOccupancy ||| friendlyOccupancy
        
        let result = generateRookMovesViaBitboards allOccupancy friendlyOccupancy startBitRef
        test <@ result <> 0UL @>
        let algNotations = result |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["a3";"a4";"a6";"b5";"c5";"d5"] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>
        ()

    [<TestCase>]
    //[<Slow>]
    let ``dry run of magic number and moves generation for rook `` () =
        let magicNumbersAndShifts = Bitboards.bootstrapMagicNumberGenerationForRook()
        printfn "%A" magicNumbersAndShifts
        let occupancyMasks = Bitboards.Constants.occupancyMaskRook
        let rookMovesDb = 
            occupancyMasks  |>  
            (Bitboards.generateOccupancyVariations >> Bitboards.generateRookMagicMoves occupancyMasks magicNumbersAndShifts) 
        ()

    [<TestCase>]
    //[<Slow>]
    //FEN: 8/p7/8/r2N4/8/8/p7/8 w - -
    let ``verify moves of Black Rook at a5; a few other black and white pieces on the board - with fresh magic`` () =
        let magicNumbersAndShifts = Bitboards.bootstrapMagicNumberGenerationForRook()
        //let magicNumbersAndShifts = Magic.PregeneratedMagic.magicNumbersAndShiftsRook
        let occupancyMasks = Bitboards.Constants.occupancyMaskRook
        let rookMovesDb = 
            occupancyMasks  |>  
            (Bitboards.generateOccupancyVariations >> Bitboards.generateRookMagicMoves occupancyMasks magicNumbersAndShifts) 
        
        let startBitRef = 39    //a5
        let opponentOccupancy = 1UL <<< 36  //d5
        let friendlyOccupancy = (1UL <<< startBitRef) ||| (1UL <<< 55) ||| (1UL <<< 15)    //a5, a7, a2
        let allOccupancy = opponentOccupancy ||| friendlyOccupancy

        let result =  Bitboards.generateRookMovesForPosition rookMovesDb allOccupancy friendlyOccupancy startBitRef magicNumbersAndShifts
        test <@ result <> 0UL @>
        let algNotations = result |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["a3";"a4";"a6";"b5";"c5";"d5"] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>

    //[<TestCase>]
    //let ``verify attack set of Black Rook at a5; a few other black and white pieces on the board`` () = 

    [<TestCase>]
    //[<Slow>]
    //FEN: 8/p7/8/r2N4/8/8/p7/8 w - -
    let ``verify moves of Black Rook at a5; a few other black and white pieces on the board - with fresh magic and FEN`` () =
        let magicNumbersAndShifts = Bitboards.bootstrapMagicNumberGenerationForRook()
        //let magicNumbersAndShifts = MagicGenerationSetupFixture.currentMagic
        //let magicNumbersAndShifts = Magic.PregeneratedMagic.magicNumbersAndShiftsRook
        let occupancyMasks = Bitboards.Constants.occupancyMaskRook
        let rookMovesDb = 
            occupancyMasks  |>  
            (Bitboards.generateOccupancyVariations >> Bitboards.generateRookMagicMoves occupancyMasks magicNumbersAndShifts) 
        
        let startBitRef = 39    //a5
        let pos = Positions.fromFenString "8/p7/8/r2N4/8/8/p7/8 w - -"
        let opponentOccupancy = pos |> Positions.whiteBitboard  //d5
        let friendlyOccupancy = pos |> Positions.blackBitboard    //a5, a7, a2
        let allOccupancy = opponentOccupancy ||| friendlyOccupancy

        let result =  Bitboards.generateRookMovesForPosition rookMovesDb allOccupancy friendlyOccupancy startBitRef magicNumbersAndShifts
        test <@ result <> 0UL @>
        let algNotations = result |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["a3";"a4";"a6";"b5";"c5";"d5"] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>

  

