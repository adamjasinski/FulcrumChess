module Board8x8Funcs
open FenParser


let private bitboardToLerbefArray (bitboard:Bitboard) =
    let arr = Array.zeroCreate<byte> 64
    bitboard 
    |> BitUtils.getSetBits
    |> Array.iter( fun bitRef -> arr.[bitRef] <- 1uy)
    arr

let private bitboardToLerbefCharArray (bb:Bitboard) (pc:Chessmen, side:Side) =
    let arr = bb |> bitboardToLerbefArray
    let letter =  PieceFenLetters.getLetter (pc,side)
    arr
    |> Array.map (fun b ->
        if b = 0uy then ' '
        else letter)

let private lerbefArrayToBoard8x8 (arr:'a[]) =
    [
        for j = 0 to 7 do
            yield arr.[j*8 .. (j+1)*8-1] |> Array.rev |> List.ofArray
    ]
    |> List.rev

let private combineCharArrays (arr1:char[]) (arr2:char[]) =
    (arr1, arr2) ||> Array.map2( fun x y -> 
        match (x,y) with
        | (' ', ' ') -> ' '
        | (' ', y) -> y
        | (x, ' ') -> x
        | (_, _)->  failwith "Oops! Trying to combine board arrays, but both contain pieces at the same positions"
       ) 


let fromBitboard (bb:Bitboard) (pc:Chessmen, side:Side) =
    let letter =  PieceFenLetters.getLetter (pc,side)
    bb
    |> bitboardToLerbefArray
    |> Array.map (fun b ->
        if b = 0uy then ' '
        else letter)
    |> lerbefArrayToBoard8x8


let fromPosition (pos:Position) =
    let allBitboards = pos |> Positions.asBitboardSequence
    let allArrays = 
        allBitboards
        |> Seq.map (fun x -> x ||> bitboardToLerbefCharArray)
        |> Array.ofSeq

    let allArraysCombined = 
        allArrays 
        |>  Seq.reduce combineCharArrays

    allArraysCombined 
        |> lerbefArrayToBoard8x8