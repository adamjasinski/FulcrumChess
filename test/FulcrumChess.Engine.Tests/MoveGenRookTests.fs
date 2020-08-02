namespace FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open Swensen.Unquote
open FulcrumChess.Engine
open FulcrumChess.Engine.Tests
open MoveGenTestHelper

type MoveGenRookTests(magicGenerationSetupFixture:MagicGenerationSetupFixture) =

    let magicNumbersAndShifts = magicGenerationSetupFixture.Lookups.MagicNumbersAndShifts.MagicNumbersAndShiftsRook

    let generateRookMovesViaBitboards (allPieces:Bitboard) (friendlyPieces:Bitboard) startBitref =
        let rookMagicMovesDb = magicGenerationSetupFixture.Lookups.RookMovesDb
        Bitboards.generateMovesForPosition SlidingPiece.Rook rookMagicMovesDb allPieces friendlyPieces startBitref magicNumbersAndShifts
    
    [<Fact>]
    let ``verify moves of first variant of rook on h1 with no other occupancy`` () =
        let rookMagicMovesDb = Bitboards.bootstrapRookMagicMoves magicNumbersAndShifts
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

    [<Fact>]
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
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>

    [<Fact; Trait("Category", "Slow")>]
    [<BoardRef("8/p7/8/r2N4/8/8/p7/8 b - -")>]
    let ``verify moves of Black Rook at a5; a few other black and white pieces on the board - with fresh magic`` () =
        let occupancyMasks = Bitboards.Constants.occupancyMaskRook
        let rookMovesDb = 
            occupancyMasks  |>  
            (Bitboards.generateOccupancyVariations >> Bitboards.generateRookMagicMoves occupancyMasks magicNumbersAndShifts) 
        
        let startBitRef = 39    //a5
        let opponentOccupancy = 1UL <<< 36  //d5
        let friendlyOccupancy = (1UL <<< startBitRef) ||| (1UL <<< 55) ||| (1UL <<< 15)    //a5, a7, a2
        let allOccupancy = opponentOccupancy ||| friendlyOccupancy

        let result =  Bitboards.generateMovesForPosition SlidingPiece.Rook rookMovesDb allOccupancy friendlyOccupancy startBitRef magicNumbersAndShifts
        test <@ result <> 0UL @>
        let algNotations = result |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["a3";"a4";"a6";"b5";"c5";"d5"] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>

    [<Fact; Trait("Category", "Slow")>]
    [<BoardRef("8/p7/8/r2N4/8/8/p7/8 b - -")>]
    let ``verify moves of Black Rook at a5; a few other black and white pieces on the board - with fresh magic and FEN`` () =
        let occupancyMasks = Bitboards.Constants.occupancyMaskRook
        let rookMovesDb = 
            occupancyMasks  |>  
            (Bitboards.generateOccupancyVariations >> Bitboards.generateRookMagicMoves occupancyMasks magicNumbersAndShifts) 
        
        let startBitRef = 39    //a5
        let pos = FenParsing.parseToPosition"8/p7/8/r2N4/8/8/p7/8 w - -"
        let opponentOccupancy = pos |> Positions.whiteBitboard  //d5
        let friendlyOccupancy = pos |> Positions.blackBitboard    //a5, a7, a2
        let allOccupancy = opponentOccupancy ||| friendlyOccupancy

        let result =  Bitboards.generateMovesForPosition SlidingPiece.Rook rookMovesDb allOccupancy friendlyOccupancy startBitRef magicNumbersAndShifts
        test <@ result <> 0UL @>
        let algNotations = result |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["a3";"a4";"a6";"b5";"c5";"d5"] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>

    interface IAssemblyFixture<MagicGenerationSetupFixture>
    

  

