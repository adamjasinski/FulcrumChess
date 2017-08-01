namespace FenParserTests

open Xunit
open Swensen.Unquote
open Puzzles1
open System

module BoardTests = 
    
    [<Fact>]
    let ``cannot create from array other than of length of 64`` () =
        let exc = Record.Exception( fun () ->
            Board64.create [|0uy; 0uy|] |> ignore )
        Assert.IsType<ArgumentException>(exc)

    [<Fact>]
    let ``can create from array of length of 64`` () =
        let b:byte[] = Array.zeroCreate 64
        let board = Board64.create b
        test <@ board |> Board64.value = b @>


    type AllPieces = 
        |WhiteKing = 75uy
        |WhiteQueen = 81uy
        |WhiteRook = 82uy
        |WhiteBishop = 66uy
        |WhiteKnight = 78uy
        |WhitePawn = 80uy
        |BlackKing = 107uy
        |BlackQueen = 113uy
        |BlackRook = 114uy
        |BlackBishop = 98uy
        |BlackKnight = 110uy
        |BlackPawn = 112uy

    module PiecesBytes = 
        let WhiteKing = 75uy
        let WhiteQueen = 81uy
        let WhiteRook = 82uy
        let WhiteBishop = 66uy
        let WhiteKnight = 78uy
        let WhitePawn = 80uy
        let BlackKing = 107uy
        let BlackQueen = 113uy
        let BlackRook = 114uy
        let BlackBishop = 98uy
        let BlackKnight = 110uy
        let BlackPawn = 112uy
        let None = 0uy

 
    [<Fact>]
    let ``can dump as text - board with 2 kings only`` () =
        let b:byte[] = Array.zeroCreate 64
        b.[4] <- (byte)AllPieces.WhiteKing
        b.[60] <- (byte)AllPieces.BlackKing
        let board = Board64.create b
        let text = board |> Board64.dumpAsText
        //printfn "%s" text
        //TODO
        //Console.WriteLine(text)
        test <@ text.Length >= 64 @>

    [<Fact>]
    let ``can dump as text - initial board position`` () =
        let b = [|
            PiecesBytes.WhiteRook; PiecesBytes.WhiteKnight; PiecesBytes.WhiteBishop; PiecesBytes.WhiteQueen; PiecesBytes.WhiteKing; PiecesBytes.WhiteBishop; PiecesBytes.WhiteKnight; PiecesBytes.WhiteRook;
            0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 
            0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 
            0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 
            0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 
            0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 
            0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 
            PiecesBytes.BlackRook; PiecesBytes.BlackKnight; PiecesBytes.BlackBishop; PiecesBytes.BlackQueen; PiecesBytes.BlackKing; PiecesBytes.BlackBishop; PiecesBytes.BlackKnight; PiecesBytes.BlackRook
        |]
        let board = Board64.create b
        let text = board |> Board64.dumpAsText
        //printfn "%s" text
        //TODO
        //Console.WriteLine(text)
        test <@ text.Length >= 64 @>

    let trimFenWithPiecesOnly (s:string) =
        s.Substring(0, s.IndexOf(" "))

    [<Fact>]
    let ``can dump as FEN - board with 2 kings only`` () =
        let b:byte[] = Array.zeroCreate 64
        b.[4] <- (byte)AllPieces.WhiteKing
        b.[60] <- (byte)AllPieces.BlackKing
        let board = Board64.create b

        let fen = board |> Board64.dumpAsFenRecursive
        
        let expectedFen = "4k3/8/8/8/8/8/8/4K3 w KQkq - 0 1"
        let expectedFenPiecesOnly = expectedFen |> trimFenWithPiecesOnly
        //Console.WriteLine(fen)
        test <@ expectedFenPiecesOnly = fen @>

    [<Fact>]
    let ``can dump as FEN - board in initial position`` () =
        let b = [|
            PiecesBytes.WhiteRook; PiecesBytes.WhiteKnight; PiecesBytes.WhiteBishop; PiecesBytes.WhiteQueen; PiecesBytes.WhiteKing; PiecesBytes.WhiteBishop; PiecesBytes.WhiteKnight; PiecesBytes.WhiteRook;
            PiecesBytes.WhitePawn; PiecesBytes.WhitePawn; PiecesBytes.WhitePawn; PiecesBytes.WhitePawn; PiecesBytes.WhitePawn; PiecesBytes.WhitePawn; PiecesBytes.WhitePawn; PiecesBytes.WhitePawn; 
            0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 
            0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 
            0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 
            0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 
            PiecesBytes.BlackPawn; PiecesBytes.BlackPawn; PiecesBytes.BlackPawn; PiecesBytes.BlackPawn; PiecesBytes.BlackPawn; PiecesBytes.BlackPawn; PiecesBytes.BlackPawn; PiecesBytes.BlackPawn;
            PiecesBytes.BlackRook; PiecesBytes.BlackKnight; PiecesBytes.BlackBishop; PiecesBytes.BlackQueen; PiecesBytes.BlackKing; PiecesBytes.BlackBishop; PiecesBytes.BlackKnight; PiecesBytes.BlackRook
        |]
        let board = Board64.create b
        
        let fen = board |> Board64.dumpAsFenRecursive
        
        let expectedFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        let expectedFenPiecesOnly = expectedFen |> trimFenWithPiecesOnly
        //Console.WriteLine(fen)
        test <@ expectedFenPiecesOnly = fen @>
        

