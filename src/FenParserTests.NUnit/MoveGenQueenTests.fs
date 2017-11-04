namespace FenParserTests.NUnit.MoveGeneration
open NUnit.Framework
open Swensen.Unquote
open FenParser
open Bitboards
open FenParserTests.NUnit

module MoveGenQueenTests =
    open MoveGenTestHelper

    [<TestCase>]
    [<BoardRef("2r5/5p2/p7/8/2Q3b1/8/4P3/2R5 w - -", "https://lichess.org/editor/2r5/5p2/p7/8/2Q3b1/8/4P3/2R5_w_-_-")>]
    let ``verify moves of White Queen at c4; a few other black and white pieces on the board`` () =
        let lookups =  MoveGenerationLookupFunctions.bootstrapAll()
        let res = MoveGenerationLookupFunctions.generatePseudoMoves lookups 

        let startBitRef = 29    //c4
        let pos = Positions.fromFenString "2r5/5p2/p7/8/2Q3b1/8/4P3/2R5 w - -"

        let result = MoveGenerationLookupFunctions.generatePseudoMoves lookups pos startBitRef

        test <@ result <> 0UL @>
        let algNotations = result |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["a4"; "b4"; "d4"; "e4"; "f4"; "g4"; "c2"; "c3"; "c5"; "c6"; "c7"; "c8"; "a6"; "b5"; "d3"; "a2"; "b3"; "d5"; "e6"; "f7" ] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>



    [<TestCase>]
    [<BoardRef("2r5/5p2/p6p/8/2Q3b1/8/4P3/2R5 w - -", "https://lichess.org/editor/2r5/5p2/p7/8/2Q3b1/8/4P3/2R5_w_-_-")>]
    let ``verify captures of White Queen at c4; a few other black and white pieces on the board`` () =
        let lookups =  MoveGenerationLookupFunctions.bootstrapAll()
        let res = MoveGenerationLookupFunctions.generatePseudoMoves lookups 
 
        let startBitRef = 29    //c4
        let pos = Positions.fromFenString "2r5/5p2/p6p/8/2Q3b1/8/4P3/2R5 w - -"

        let moves = MoveGenerationLookupFunctions.generatePseudoMoves lookups pos startBitRef

        let attacks = pos |> Positions.getCapturesFromPseudoMoves moves startBitRef

        test <@ attacks <> 0UL @>
        let algNotations = attacks |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["a6"; "c8";  "f7"; "g4"] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>
