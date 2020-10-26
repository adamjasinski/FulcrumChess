﻿namespace FulcrumChess.Engine.Tests
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


