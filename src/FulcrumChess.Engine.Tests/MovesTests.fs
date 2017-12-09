namespace FulcrumChess.Engine.Tests
open FulcrumChess.Engine
open NUnit.Framework
open Swensen.Unquote

module MovesTests =

    [<Test>]
    let ``creates sets expected bit mask`` () =
        // e2-e4
        let move = Moves.create (11,27) false

        test <@ 731us = uint16(move) @>       


    [<Test>]
    let ``getDestBitRef returns expected bit`` () =
        // b8-a6 == 62--47
        let move = (62us <<< 6 ||| 47us)
        let (actualSrcBitRef,actualDestBitRef) = move |> Moves.getSrcAndDestBitRefs
        test <@ 62 = actualSrcBitRef @>  
        test <@ 47 = actualDestBitRef @>       


    [<Test>]
    let ``can roundtrip`` () =
        // e2-e4
        let move = Moves.create (11,27) false

        let actualSrcBitRef = move |> Moves.getSrcBitRef
        let actualDestBitRef = move |> Moves.getDestBitRef
        test <@ 11 = actualSrcBitRef @>
        test <@ 27 = actualDestBitRef @>


    [<Test>]
    let ``getCoordinationNotation returns expected value`` () =
       // e2-e4
       let move = Moves.create (11,27) false

       let actualNotation = move |> Moves.toCoordinateNotation
       test <@ "e2-e4" = actualNotation @>

