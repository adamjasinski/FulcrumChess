namespace FulcrumChess.Engine.Tests
open FulcrumChess.Engine
open Xunit
open Swensen.Unquote

module MovesTests =

    [<Fact>]
    let ``creates sets expected bit mask`` () =
        // e2-e4
        let move = Move.create (11,27) false

        test <@ 731us = uint16(move) @>       


    [<Fact>]
    let ``getDestBitRef returns expected bit`` () =
        // b8-a6 == 62--47
        let move = (62u <<< 6 ||| 47u)
        let (actualSrcBitRef,actualDestBitRef) = move |> Move.getSrcAndDestBitRefs
        test <@ 62 = actualSrcBitRef @>  
        test <@ 47 = actualDestBitRef @>       


    [<Fact>]
    let ``can roundtrip`` () =
        // e2-e4
        let move = Move.create (11,27) true

        let actualSrcBitRef = move |> Move.getSrcBitRef
        let actualDestBitRef = move |> Move.getDestBitRef
        let isCheck = move |> Move.isCapture
        test <@ 11 = actualSrcBitRef @>
        test <@ 27 = actualDestBitRef @>
        test <@ true = isCheck @>


    [<Fact>]
    let ``toAlgebraicNotation returns expected value`` () =
       // e2-e4
       let move = Move.create (11,27) false

       let actualNotation = move |> Notation.toAlgebraicNotation
       test <@ "e2e4" = actualNotation @>

    [<Fact>]
    let ``toAlgebraicNotation returns expected value with capture`` () =
       // e2-e4
       let move = Move.create (11,27) true

       let actualNotation = move |> Notation.toAlgebraicNotation
       test <@ "e2e4x" = actualNotation @>