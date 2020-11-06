namespace FulcrumChess.Engine.Tests
open FulcrumChess.Engine
open FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open Swensen.Unquote

type PositionTests(magicGenerationSetupFixture:MagicGenerationSetupFixture) =

    let lookups = magicGenerationSetupFixture.Lookups

    let verifyOccupancy (expectedOccupancy:int array) (actualOccupancy:Bitboard) =
        let actualBitboardAsBitArray = (uint64(actualOccupancy) |> BitUtils.getSetBits)
        test <@ actualBitboardAsBitArray = expectedOccupancy @>

    interface IAssemblyFixture<MagicGenerationSetupFixture>

    [<Fact>]
    [<BoardRef("8/5p2/p7/8/2B5/8/4P3/8 w - -", "https://lichess.org/editor/8/5p2/p7/8/2B5/8/4P3/8_w_-_-")>]
    member __. ``verify position of White Bishop at c4; a few other black and white pieces on the board`` () =
        let pos = FenParsing.parseToPosition "8/5p2/p7/8/2B5/8/4P3/8 w - -"

        let opponentOccupancy = pos |> Positions.blackBitboard  //a6, f7
        let friendlyOccupancy = pos |> Positions.whiteBitboard    //e2, c4
        let allOccupancy = pos |> Positions.bothSidesBitboard

        verifyOccupancy [|47; 50|] opponentOccupancy
        verifyOccupancy [|11; 29|] friendlyOccupancy
        verifyOccupancy [|11; 29; 47; 50|] allOccupancy


    [<Fact>]
    [<BoardRef("8/5p2/p7/8/2B5/8/4P3/8 w - -", "https://lichess.org/editor/8/5p2/p7/8/2B5/8/4P3/8_w_-_-")>]
    member __. ``set position of Black Bishop;`` () =
        let pos = FenParsing.parseToPosition "8/5p2/p7/8/2B5/8/4P3/8 w - -"

        let pos' = pos |> Positions.setFenPiece 'b' 56
        
        let opponentOccupancy = pos' |> Positions.blackBitboard  //a6, f7, h8
        let friendlyOccupancy = pos' |> Positions.whiteBitboard    //e2, c4
        let allOccupancy = pos' |> Positions.bothSidesBitboard

        verifyOccupancy [|47; 50; 56|] opponentOccupancy
        verifyOccupancy [|11; 29|] friendlyOccupancy
        verifyOccupancy [|11; 29; 47; 50; 56|] allOccupancy

    [<Theory>]
    [<InlineDataEx("rnb1kbnr/pppp1ppp/8/4p3/3PP2q/8/PPP2PPP/RNBQKBNR w KQkq -", 10, [|"f3";"f4"|])>]
    member __. ``verify validation of a move of a pinned White Pawn`` (fen:string, startBitRef:int, expectedSquares:string array) =
        //Try to make a move. It should be rejected as illegal, as the pawn is pinned

        let pos = FenParsing.parseToPosition fen
        let move = Move.create (10,18)
        let generateAttacks = Bitboards.MoveGenerationLookupFunctions.generateAllPseudoMovesForSide lookups
        let pos' = pos |> Positions.makeMoveWithValidation generateAttacks move

        test <@ None = pos' @>

    [<Theory>]
    [<Trait("Castling","true")>]
    [<InlineDataEx("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1", "e1g1", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQ1RK1 b kq - 1 1")>]
    [<InlineDataEx("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R3KBNR w KQkq - 0 1", "e1c1", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/2KR1BNR b kq - 1 1")>]
    [<InlineDataEx("rnbqk2r/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1", "e8g8", "rnbq1rk1/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQ - 1 2")>]
    [<InlineDataEx("r3kbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1", "e8c8", "2kr1bnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQ - 1 2")>]
    member __. ``make move - castling`` (fen:string, kingMoveAlgNotation:string, expectedFen:string) =
        let pos = FenParsing.parseToPosition fen

        let actualMove = Notation.fromLongAlgebraicNotationToMove kingMoveAlgNotation
        printfn "Gota moove: %d - %d" (actualMove |> Move.getDestBitRef) (actualMove |> Move.getSrcBitRef)
        let generateAttacks = Bitboards.MoveGenerationLookupFunctions.generateAllPseudoMovesForSide lookups

        let positionAfterMove = pos |> Positions.makeMoveWithValidation generateAttacks actualMove
        test <@ positionAfterMove |> Option.isSome @>

        let posCharArray = positionAfterMove.Value |> FenParsing.dumpPosition
        printfn "%A" posCharArray
        printfn "-------------------------"
        let actualFenAfterMove = positionAfterMove.Value |> FenParsing.toFen
        printfn "%s" actualFenAfterMove
        <@ expectedFen.StartsWith(actualFenAfterMove) @>



