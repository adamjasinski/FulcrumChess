let generateJaggedArray () =
    [
        let genInner () =
            [
                for j in 1..2 do
                            yield j
            ]
        for i in 1..10 do
            yield genInner()
                // [
                //     for j in 1..2 do
                //         yield j
                // ]
    ]

let result = generateJaggedArray ()
printfn "%d" result.Length
printfn "%A" <| result
