﻿namespace FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open Swensen.Unquote
open FulcrumChess.Engine
open FulcrumChess.Engine.Tests

type MoveGenBishopTests(magicGenerationSetupFixture:MagicGenerationSetupFixture) =

    let magicNumbersAndShifts = magicGenerationSetupFixture.Lookups.MagicNumbersAndShifts.MagicNumbersAndShiftsBishop

    let bitboardToConventionalMoves startBitRef = Bitboards.bitboardToConventionalMoves startBitRef Position.initialPosition //ignore the actual position
    
    [<Fact>]
    [<Category("MagicGeneration")>]
    [<BoardRef("8/5p2/p7/8/2B5/8/4P3/8 w - -", "https://lichess.org/editor/8/5p2/p7/8/2B5/8/4P3/8_w_-_-")>]
    let ``verify moves of White Bishop at c4; a few other black and white pieces on the board - with fresh magic and FEN`` () =
        let pc = SlidingPiece.Bishop
        let occupancyMasks = Bitboards.getOccupancyMask pc
        let movesDb = 
            occupancyMasks  |>  
            (Bitboards.generateOccupancyVariations >> (Bitboards.generateMagicMoves pc) occupancyMasks magicNumbersAndShifts) 
        
        let startBitRef = 29    //c4
        let pos = FenParsing.parseToPosition "8/5p2/p7/8/2B5/8/4P3/8 w - -"
        let opponentOccupancy = pos |> Position.blackBitboard  //a6, f7
        let friendlyOccupancy = pos |> Position.whiteBitboard    //e2, c4
        let allOccupancy = opponentOccupancy ||| friendlyOccupancy

        let result = 
            Bitboards.generateMovesForPosition SlidingPiece.Bishop movesDb allOccupancy friendlyOccupancy startBitRef magicNumbersAndShifts
            |> bitboardToConventionalMoves startBitRef
        test <@ not (Array.isEmpty result) @>
        let algNotations = result |> movesToAlgebraicNotations
        printfn "%A" (algNotations)
        let expectedSquares = ["a2"; "b3"; "d5"; "e6"; "f7"; "a6"; "b5"; "d3"] |> Set.ofList
        test <@ expectedSquares = (algNotations |> Set.ofArray) @>

    [<Fact>]
    [<BoardRef("7b/6n1/8/8/8/8/8/8 w - -", "https://lichess.org/editor/8/8/8/8/8/8/8/8_w_-_-")>]
    let ``verify no moves of Black Bishop at h8; own piece blocking the only move`` () =
        let pc = SlidingPiece.Bishop
        let occupancyMasks = Bitboards.getOccupancyMask pc
        let movesDb = 
         occupancyMasks  |>  
         (Bitboards.generateOccupancyVariations >> (Bitboards.generateMagicMoves pc) occupancyMasks magicNumbersAndShifts) 

        let startBitRef = 56    //h8
        let pos = FenParsing.parseToPosition "7b/6n1/8/8/8/8/8/8 w - -"
        let opponentOccupancy = pos |> Position.whiteBitboard 
        let friendlyOccupancy = pos |> Position.blackBitboard
        let allOccupancy = opponentOccupancy ||| friendlyOccupancy
        let result = 
            Bitboards.generateMovesForPosition SlidingPiece.Bishop movesDb allOccupancy friendlyOccupancy startBitRef magicNumbersAndShifts
            |> bitboardToConventionalMoves startBitRef
        test <@ Array.isEmpty result @>

    interface IAssemblyFixture<MagicGenerationSetupFixture>