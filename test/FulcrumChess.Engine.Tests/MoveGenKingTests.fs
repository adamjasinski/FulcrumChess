namespace FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open Swensen.Unquote
open FulcrumChess.Engine
open Bitboards
open FulcrumChess.Engine.Tests

type MoveGenKingTests(magicGenerationSetupFixture:MagicGenerationSetupFixture) =

    let lookups = magicGenerationSetupFixture.Lookups

    interface IAssemblyFixture<MagicGenerationSetupFixture>

    static member TestCases() =
        seq {
            yield ("8/6p1/8/8/8/n1Q5/1K6/1N6 w - -", 14, ["a1"; "a2"; "a3"; "b3"; "c1"; "c2"]);
            yield ("8/8/8/8/8/8/8/K7 w - -", 7, ["a2"; "b2"; "b1"]);
            yield ("4k3/8/8/8/8/8/8/8 b - -", 59, ["d8"; "f8"; "d7"; "e7"; "f7"]);
        }

    [<Fact>]
    [<BoardRef("8/6p1/8/8/8/n1Q5/1K6/1N6 w - -", "https://lichess.org/editor/8/6p1/8/8/8/n1Q5/1K6/1N6_w_-_-")>]
    member __.``verify moves of White King at c3; a few other black and white pieces on the board`` () =
        let startBitRef = 14    //b2
        let pos = FenParsing.parseToPosition "8/6p1/8/8/8/n1Q5/1K6/1N6 w - -"

        let result = MoveGenerationLookupFunctions.generatePseudoMoves lookups pos startBitRef

        test <@ not (Array.isEmpty result) @>
        let algNotations = result |> movesToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["a1"; "a2"; "a3"; "b3"; "c1"; "c2"] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>

    [<Theory; MemberDataEx("TestCases")>]
    member __.``verify moves of King (data bound)`` (fen:string, startBitRef:int, expectedSquaresList:string list) =
        let pos = FenParsing.parseToPosition fen

        let result = MoveGenerationLookupFunctions.generatePseudoMoves lookups pos startBitRef

        test <@ not (Array.isEmpty result) @>
        let algNotations = result |> movesToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = expectedSquaresList |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>
        ()

   //TODO - castling!

    [<Fact>]
    [<Trait("Castling","true")>]
    [<BoardRef("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1")>]
    member __.``verify White Kingside castling`` () =
        let startBitRef = 3    //e1
        let pos = FenParsing.parseToPosition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1"

        let result = MoveGenerationLookupFunctions.generatePseudoMoves lookups pos startBitRef

        test <@ not (Array.isEmpty result) @>
        let algNotations = result |> movesToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["f1"; "g1";] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>