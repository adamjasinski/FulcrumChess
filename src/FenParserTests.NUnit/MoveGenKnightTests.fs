namespace FenParserTests.NUnit.MoveGeneration
open NUnit.Framework
open Swensen.Unquote
open FenParser
open Bitboards

module MoveGenKnightTests =
    open MoveGenTestHelper

    type MovesTestData() =
        static member TestCases():TestCaseData seq =
            seq {
                yield TestCaseData("8/8/8/3n4/8/8/8/8 b - -", 36, ["b6"; "c7"; "e7"; "f6"; "f4"; "e3"; "c3"; "b4" ]);
                yield TestCaseData("8/6n1/8/8/8/8/8/8 b - -", 49, ["e8"; "h5"; "f5"; "e6"]);
                yield TestCaseData("2k5/8/1n6/3B4/8/8/8/8 b - -", 46, ["a8"; "d7"; "d5"; "c4"; "a4"]);
            }

    [<TestCase; TestCaseSource(typeof<MovesTestData>,"TestCases")>]
    let ``verify moves of Black Knight (data bound)`` (fen:string, startBitRef:int, expectedSquaresList:string list) =
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

