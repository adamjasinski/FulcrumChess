namespace FulcrumChess.Engine.Tests
open System

type BoardRefAttribute(fen:string, url:string) =
    inherit Attribute()
    member __.FEN = fen
    member __.Url = url
    new(fen:string) = BoardRefAttribute(fen,"")

type SlowAttribute() =
    inherit Attribute()
