namespace FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open Swensen.Unquote
open FulcrumChess.Engine
open Bitboards
open FulcrumChess.Engine.Tests

type MoveGenQueenTests(magicGenerationSetupFixture:MagicGenerationSetupFixture, output:Xunit.Abstractions.ITestOutputHelper) =

    let lookups = magicGenerationSetupFixture.Lookups

    [<Fact>]
    [<BoardRef("2r5/5p2/p7/8/2Q3b1/8/4P3/2R5 w - -", "https://lichess.org/editor/2r5/5p2/p7/8/2Q3b1/8/4P3/2R5_w_-_-")>]
    let ``verify moves of White Queen at c4; a few other black and white pieces on the board`` () =
        let startBitRef = 29    //c4
        let pos = FenParsing.parseToPosition "2r5/5p2/p7/8/2Q3b1/8/4P3/2R5 w - -"

        let result = MoveGenerationLookupFunctions.generatePseudoMoves lookups pos startBitRef

        test <@ result <> 0UL @>
        let algNotations = result |> setBitsToAlgebraicNotations
        sprintf "%A" (algNotations) |> output.WriteLine
        let expectedSquares = ["a4"; "b4"; "d4"; "e4"; "f4"; "g4"; "c2"; "c3"; "c5"; "c6"; "c7"; "c8"; "a6"; "b5"; "d3"; "a2"; "b3"; "d5"; "e6"; "f7" ] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>



    [<Fact>]
    [<BoardRef("2r5/5p2/p6p/8/2Q3b1/8/4P3/2R5 w - -", "https://lichess.org/editor/2r5/5p2/p7/8/2Q3b1/8/4P3/2R5_w_-_-")>]
    let ``verify captures of White Queen at c4; a few other black and white pieces on the board`` () =
        let startBitRef = 29    //c4
        let pos = FenParsing.parseToPosition "2r5/5p2/p6p/8/2Q3b1/8/4P3/2R5 w - -"

        let moves = MoveGenerationLookupFunctions.generatePseudoMoves lookups pos startBitRef

        let attacks = pos |> Positions.getCapturesFromPseudoMoves moves startBitRef

        test <@ attacks <> 0UL @>
        let algNotations = attacks |> setBitsToAlgebraicNotations
        sprintf "%A" (algNotations) |> output.WriteLine
        let expectedSquares = ["a6"; "c8";  "f7"; "g4"] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>

    interface IAssemblyFixture<MagicGenerationSetupFixture>
