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
        let move = Notation.fromLongAlgebraicNotationToMove moveAlgNotation
        let generateAttacks = Bitboards.MoveGenerationLookupFunctions.generateAttacks lookups
        let pos' = pos |> Position.tryMakeMoveInternal generateAttacks move

        test <@ None = pos' @>

    [<Theory>]
    [<InlineDataEx("rnb1k1nr/ppppqppp/8/8/8/2b5/PPP3PP/RNBQKBNR w KQkq - 0 1", "b1c3")>]
    member __. ``illegal move - king still under check (double attack)`` (fen:string, moveAlgNotation:string) =
        //Try to make a move. It should be rejected as illegal, as the pawn is pinned

        let pos = FenParsing.parseToPosition fen
        let move = Notation.fromLongAlgebraicNotationToMove moveAlgNotation
        let generateAttacks = Bitboards.MoveGenerationLookupFunctions.generateAttacks lookups
        let pos' = pos |> Position.tryMakeMoveInternal generateAttacks move

        test <@ None = pos' @>

    [<Fact>]
    member __. ``play several moves`` () =

        let pos = Position.initialPosition
        let move1w = Notation.fromLongAlgebraicNotationToMove "h2h4"
        let move1b = Notation.fromLongAlgebraicNotationToMove "e7e5"
        let move2w = Notation.fromLongAlgebraicNotationToMove "g1h3"
        let generateAttacks = Bitboards.MoveGenerationLookupFunctions.generateAttacks lookups

        let pos' = 
            pos |> Some
            |> Option.bind (Position.tryMakeMoveInternal generateAttacks move1w)
            |> Option.bind (Position.tryMakeMoveInternal generateAttacks move1b)
            |> Option.bind (Position.tryMakeMoveInternal generateAttacks move2w)

        test <@ pos'.IsSome @>
        let actualFenAfterMove = pos'.Value |> FenParsing.toFen

        printfn "%A" pos'.Value.Board

        let prettyPrint = pos'.Value |> Position.prettyPrint
        printfn "%s" prettyPrint

        //let expectedFen = "rnbqkbnr/pppp1ppp/8/4p3/7P/7N/PPPPPPP1/RNBQKB1R b KQkq - 1 2"
        let expectedFen = "rnbqkbnr/pppp1ppp/8/4p3/7P/7N/PPPPPPP1/RNBQKB1R b -"
        Assert.StartsWith(actualFenAfterMove, expectedFen)


    [<Fact>]
    [<Trait("Repro", "true")>]
    member __. ``pesky redundant board bug repro`` () =

        let pos = Position.initialPosition
       
        let moves = ["h1g1"; "h5g4"; "g2g4"; "h7h5"; "g1h3"] |> List.rev |> List.map Notation.fromLongAlgebraicNotationToMove
        let generateAttacks = Bitboards.MoveGenerationLookupFunctions.generateAttacks lookups

        let maybePrettyPrint p = 
            match p with
            | Some(pp) -> printf "%s" ( pp |> Position.prettyPrint)
            | None -> ()
            p

        //let makeMoves = moves |> List.map ((Position.tryMakeMoveInternal generateAttacks) >> maybePrettyPrint)
        let makeMoves = moves |> List.map ( fun mv -> (Position.tryMakeMoveInternal generateAttacks mv) >> maybePrettyPrint)
        //let makeMoves = moves |> List.map (fun mv ->
            // let p' = Position.tryMakeMoveInternal generateAttacks p
            // if p'.
            // // match p' with
            // // | Some(pp) -> printf "%s" ( pp |> Position.prettyPrint)
            // // | None -> ()
            // p'
            // )
     
        let bindMany binders opt =
            let rec loop optAcc b =
                match b with
                | bs::tail -> loop (optAcc |> Option.bind bs) tail
                | [] -> optAcc
            loop  opt binders
        // let pos' = 
        //     pos |> Some
        //     |> Option.bind (Position.tryMakeMoveInternal generateAttacks move1w)
        //     |> Option.bind (Position.tryMakeMoveInternal generateAttacks move1b)
        //     |> Option.bind (Position.tryMakeMoveInternal generateAttacks move2w)

        let pos' = Some pos |> bindMany makeMoves
        test <@ pos'.IsSome @>
        let actualFenAfterMove = pos'.Value |> FenParsing.toFen

        printfn "%A" pos'.Value.Board

        let prettyPrint = pos'.Value |> Position.prettyPrint
        printfn "%s" prettyPrint

        //let expectedFen = "rnbqkbnr/ppppppp1/8/8/6p1/7N/PPPPPP1P/RNBQKBR1 b Qkq - 1 3"
        let expectedFen = "rnbqkbnr/ppppppp1/8/8/6p1/7N/PPPPPP1P/RNBQKBR1 b -"
        Assert.StartsWith(actualFenAfterMove, expectedFen)
