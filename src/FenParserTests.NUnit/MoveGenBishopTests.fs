namespace FenParserTests.NUnit.MoveGeneration
open NUnit.Framework
open Swensen.Unquote
open FenParser
open FenParserTests.NUnit

module MoveGenBishopTests =
    open MoveGenTestHelper

    let magicNumbersAndShifts = 
        let allMagic = FenParserTests.NUnit.MoveGeneration.MagicGenerationSetupFixture.getCurrentMagic()
        allMagic.MagicNumbersAndShiftsBishop

    [<TestCase>]
    [<BoardRef("8/5p2/p7/8/2B5/8/4P3/8 w - -", "https://lichess.org/editor/8/5p2/p7/8/2B5/3P4/8/8_w_-_-")>]
    let ``verify moves of White Bishop at c4; a few other black and white pieces on the board - with fresh magic and FEN`` () =
        let pc = Pieces.SlidingPiece.Bishop
        let occupancyMasks = Bitboards.getOccupancyMask pc
        let movesDb = 
            occupancyMasks  |>  
            (Bitboards.generateOccupancyVariations >> (Bitboards.generateMagicMoves pc) occupancyMasks magicNumbersAndShifts) 
        
        let startBitRef = 29    //c4
        let pos = Positions.fromFenString "8/5p2/p7/8/2B5/8/4P3/8 w - -"
        let opponentOccupancy = pos |> Positions.blackBitboard  //a6, f7
        let friendlyOccupancy = pos |> Positions.whiteBitboard    //e2
        let allOccupancy = opponentOccupancy ||| friendlyOccupancy

        let result =  Bitboards.generateMovesForPosition Pieces.SlidingPiece.Bishop movesDb allOccupancy friendlyOccupancy startBitRef magicNumbersAndShifts
        test <@ result <> 0UL @>
        let algNotations = result |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["a2"; "b3"; "d5"; "e6"; "f7"; "a6"; "b5"; "d3"] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>