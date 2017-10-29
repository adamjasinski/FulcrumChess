namespace FenParserTests.NUnit.MoveGeneration
open NUnit.Framework
open Swensen.Unquote
open FenParser
open FenParserTests.NUnit

module MoveGenQueenTests =
    open MoveGenTestHelper

    [<TestCase>]
    [<BoardRef("2r5/5p2/p7/8/2Q3b1/8/4P3/2R5 w - -", "https://lichess.org/editor/2r5/5p2/p7/8/2Q3b1/8/4P3/2R5_w_-_-")>]
    let ``verify moves of White Queen at c4; a few other black and white pieces on the board - with fresh magic and FEN`` () =
        let pc = Pieces.SlidingPiece.Rook
        let magicNumbersAndShifts = Bitboards.bootstrapMagicNumberGeneration pc
        let occupancyMasks = Bitboards.getOccupancyMask pc
        let movesDb = 
            occupancyMasks  |>  
            (Bitboards.generateOccupancyVariations >> (Bitboards.generateMagicMoves pc) occupancyMasks magicNumbersAndShifts) 
        
        let startBitRef = 29    //c4
        let pos = Positions.fromFenString "2r5/5p2/p7/8/2Q3b1/8/4P3/2R5 w - -"
        let opponentOccupancy = pos |> Positions.blackBitboard  //a6, c8, f7
        let friendlyOccupancy = pos |> Positions.whiteBitboard    //c1, e2, c4 (self)
        let opponentOccupancyAsBitArray = (uint64(opponentOccupancy) |> BitUtils.getSetBits)
        let friendlyOccupancyAsBitArray = (uint64(friendlyOccupancy) |> BitUtils.getSetBits)
        test <@ opponentOccupancyAsBitArray = [|47; 61; 50; 25|] @>
        test <@ friendlyOccupancyAsBitArray = [|5; 11; 29; |] @>
        let allOccupancy = opponentOccupancy ||| friendlyOccupancy

        let result =  Bitboards.generateMovesForPosition Pieces.SlidingPiece.Rook movesDb allOccupancy friendlyOccupancy startBitRef magicNumbersAndShifts
        test <@ result <> 0UL @>
        let algNotations = result |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["a4"; "b4"; "d4"; "e4"; "f4"; "g4"; "c2"; "c3"; "c5"; "c6"; "c7"; "c8"; "a6"; "b5"; "d3"; "a2"; "b3"; "d5"; "e6"; "f7" ] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>
