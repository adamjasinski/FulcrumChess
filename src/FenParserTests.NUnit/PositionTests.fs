namespace FenParserTests.NUnit
open FenParser
open NUnit.Framework
open Swensen.Unquote

module PositionTests =

    let verifyOccupancy (expectedOccupancy:int array) (actualOccupancy:Bitboards.Bitboard) =
        let actualBitboardAsBitArray = (uint64(actualOccupancy) |> BitUtils.getSetBits)
        test <@ actualBitboardAsBitArray = expectedOccupancy @>

    [<TestCase>]
    [<BoardRef("8/5p2/p7/8/2B5/8/4P3/8 w - -", "https://lichess.org/editor/8/5p2/p7/8/2B5/3P4/8/8_w_-_-")>]
    let ``verify position of White Bishop at c4; a few other black and white pieces on the board`` () =
        let pos = Positions.fromFenString "8/5p2/p7/8/2B5/8/4P3/8 w - -"

        let opponentOccupancy = pos |> Positions.blackBitboard  //a6, f7
        let friendlyOccupancy = pos |> Positions.whiteBitboard    //e2
        let allOccupancy = pos |> Positions.bothSidesBitboard

        verifyOccupancy [|47; 50|] opponentOccupancy
        verifyOccupancy [|11; 29|] friendlyOccupancy
        verifyOccupancy [|11; 29; 47; 50|] allOccupancy

