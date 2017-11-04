namespace FenParserTests.NUnit.MoveGeneration
open NUnit.Framework
open Swensen.Unquote
open FenParser
open Bitboards
open FenParserTests.NUnit

module MoveGenKingTests =
    open MoveGenTestHelper

    type MovesTestData() =
        static member TestCases():TestCaseData seq =
            seq {
                yield TestCaseData("8/6p1/8/8/8/n1Q5/1K6/1N6 w - -", 14, ["a1"; "a2"; "a3"; "b3"; "c1"; "c2"]);
                yield TestCaseData("8/8/8/8/8/8/8/K7 w - -", 7, ["a2"; "b2"; "b1"]);
                yield TestCaseData("4k3/8/8/8/8/8/8/8 w - -", 59, ["d8"; "f8"; "d7"; "e7"; "f7"]);
            }
           

    [<TestCase>]
    [<BoardRef("8/6p1/8/8/8/n1Q5/1K6/1N6 w - -", "https://lichess.org/editor/8/6p1/8/8/8/n1Q5/1K6/1N6_w_-_-")>]
    let ``verify moves of White King at c3; a few other black and white pieces on the board`` () =
        let lookups =  MoveGenerationLookupFunctions.bootstrapAll()
        let res = MoveGenerationLookupFunctions.generatePseudoMoves lookups 

        let startBitRef = 14    //b2
        let pos = Positions.fromFenString "8/6p1/8/8/8/n1Q5/1K6/1N6 w - -"

        let result = MoveGenerationLookupFunctions.generatePseudoMoves lookups pos startBitRef

        test <@ result <> 0UL @>
        let algNotations = result |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["a1"; "a2"; "a3"; "b3"; "c1"; "c2"] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>

    [<TestCase; TestCaseSource(typeof<MovesTestData>,"TestCases")>]
    let ``verify moves of White King (data bound)`` (fen:string, startBitRef:int, expectedSquaresList:string list) =
        let lookups =  MoveGenerationLookupFunctions.bootstrapAll()
        let res = MoveGenerationLookupFunctions.generatePseudoMoves lookups 

        let pos = Positions.fromFenString fen

        let result = MoveGenerationLookupFunctions.generatePseudoMoves lookups pos startBitRef

        test <@ result <> 0UL @>
        let algNotations = result |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = expectedSquaresList |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>
        ()

    
   //TODO - castling!


