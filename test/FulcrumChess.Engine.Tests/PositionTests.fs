namespace FulcrumChess.Engine.Tests
open FulcrumChess.Engine
open FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open Swensen.Unquote

type PositionTests(magicGenerationSetupFixture:MagicGenerationSetupFixture) =

    let lookups = magicGenerationSetupFixture.Lookups

    let verifyOccupancy (expectedOccupancy:int[]) (actualOccupancy:Bitboard) =
        let actualBitboardAsBitArray = (uint64(actualOccupancy) |> BitUtils.getSetBits)
        test <@ actualBitboardAsBitArray = expectedOccupancy @>

    interface IAssemblyFixture<MagicGenerationSetupFixture>

    [<Fact>]
    [<BoardRef("8/5p2/p7/8/2B5/8/4P3/8 w - -", "https://lichess.org/editor/8/5p2/p7/8/2B5/8/4P3/8_w_-_-")>]
    member __. ``verify position of White Bishop at c4; a few other black and white pieces on the board`` () =
        let pos = FenParsing.parseToPosition "8/5p2/p7/8/2B5/8/4P3/8 w - -"

        let opponentOccupancy = pos |> Position.blackBitboard  //a6, f7
        let friendlyOccupancy = pos |> Position.whiteBitboard    //e2, c4
        let allOccupancy = pos |> Position.bothSidesBitboard

        verifyOccupancy [|47; 50|] opponentOccupancy
        verifyOccupancy [|11; 29|] friendlyOccupancy
        verifyOccupancy [|11; 29; 47; 50|] allOccupancy


    [<Fact>]
    [<BoardRef("8/5p2/p7/8/2B5/8/4P3/8 w - -", "https://lichess.org/editor/8/5p2/p7/8/2B5/8/4P3/8_w_-_-")>]
    member __. ``set position of Black Bishop;`` () =
        let pos = FenParsing.parseToPosition "8/5p2/p7/8/2B5/8/4P3/8 w - -"

        let pos' = pos |> Position.setFenPiece 'b' 56
        
        let opponentOccupancy = pos' |> Position.blackBitboard  //a6, f7, h8
        let friendlyOccupancy = pos' |> Position.whiteBitboard    //e2, c4
        let allOccupancy = pos' |> Position.bothSidesBitboard

        verifyOccupancy [|47; 50; 56|] opponentOccupancy
        verifyOccupancy [|11; 29|] friendlyOccupancy
        verifyOccupancy [|11; 29; 47; 50; 56|] allOccupancy

    [<Theory>]
    [<InlineDataEx("rnb1kbnr/pppp1ppp/8/4p3/3PP2q/8/PPP2PPP/RNBQKBNR w KQkq -", "f2f3")>]
    member __. ``illegal move - pinned White Pawn`` (fen:string, moveAlgNotation:string) =
        //Try to make a move. It should be rejected as illegal, as the pawn is pinned

        let pos = FenParsing.parseToPosition fen
        let move = UciMove.fromLongAlgebraicNotationToMove pos moveAlgNotation
        let generateAttacks = Bitboards.MoveGenerationLookupFunctions.generateAttacks lookups
        let pos' = pos |> Position.tryMakeMoveInternal generateAttacks move

        test <@ None = pos' @>

    [<Theory>]
    [<InlineDataEx("rnb1k1nr/ppppqppp/8/8/8/2b5/PPP3PP/RNBQKBNR w KQkq - 0 1", "b1c3")>]
    member __. ``illegal move - king still under check (double attack)`` (fen:string, moveAlgNotation:string) =
        //Try to make a move. It should be rejected as illegal, as the pawn is pinned

        let pos = FenParsing.parseToPosition fen
        let move = UciMove.fromLongAlgebraicNotationToMove pos moveAlgNotation
        let generateAttacks = Bitboards.MoveGenerationLookupFunctions.generateAttacks lookups
        let pos' = pos |> Position.tryMakeMoveInternal generateAttacks move

        test <@ None = pos' @>

    [<Fact>]
    [<Category("Zobrist")>]
    member __.``should calculate Zobrist hash for the initial position`` () =
        let pos = Position.initialPosition
        let hashKey = pos |> Position.calculateZobristHash
        printfn "Hash: %d" hashKey
        test <@ hashKey = 16566630060555062715UL @> //previously calculated value