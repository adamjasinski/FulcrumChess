namespace FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open Swensen.Unquote
open FulcrumChess.Engine
open FulcrumChess.Engine.Tests
open Bitboards

type MoveGenKnightTests(magicGenerationSetupFixture:MagicGenerationSetupFixture) =

    let lookups = magicGenerationSetupFixture.Lookups

    interface IAssemblyFixture<MagicGenerationSetupFixture>

    static member TestCases =
        seq {
            ("8/8/8/3n4/8/8/8/8 b - -", 36, ["b6"; "c7"; "e7"; "f6"; "f4"; "e3"; "c3"; "b4" ]);
            ("8/6n1/8/8/8/8/8/8 b - -", 49, ["e8"; "h5"; "f5"; "e6"]);
            ("2k5/8/1n6/3B4/8/8/8/8 b - -", 46, ["a8"; "d7"; "d5"; "c4"; "a4"]);
            ("8/8/8/8/8/8/8/n7 b - -", 7, ["b3"; "c2"]);
            ("8/2r5/8/3n4/5P2/2N1b3/8/8 b - -", 36, ["b6"; "e7"; "f6"; "f4"; "c3"; "b4" ]);
            ("7Q/4b3/6n1/8/5p1P/8/8/8 b - -", 41, ["f8"; "h8";  "h4"; "e5"]);
            ("7Q/4b3/6n1/8/5p1P/8/8/8 b - -", 41, ["f8"; "h8";  "h4"; "e5"]);
        }
        |> Seq.map( fun (a,b,c) -> [|box a; box b; box c|])


    [<Theory; MemberData("TestCases")>]
    member __. ``verify moves of Black Knight (data bound)`` (fen:string, startBitRef:int, expectedSquaresList:string list) =
        let pos = FenParsing.parseToPosition fen

        let result = MoveGenerationLookupFunctions.generatePseudoMoves lookups pos startBitRef

        test <@ result <> 0UL @>
        let algNotations = result |> setBitsToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = expectedSquaresList |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>

