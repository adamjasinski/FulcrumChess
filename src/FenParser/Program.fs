// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
namespace FenParser
open System

module Program =
    [<EntryPoint>]
    let main argv = 
        printfn "%A" argv
        //System.Diagnostics.Debug.Assert(false, "Arbitrary assert failed")
        //let sampleRookBitboardDatabase = Bitboards.bootstrapRookMagicMoves()
        //sampleRookBitboardDatabase |> Array.collect id |> Array.length |> printfn "All rook variations: %d"
        //printfn "%A" sampleRookBitboardDatabase.Length
        printfn "64-bit process: %A " Environment.Is64BitProcess
        let startTime = DateTime.Now
        let magick = Bitboards.bootstrapMagicNumberGenerationForRook()
        let endTime = DateTime.Now
        printfn "%A" magick
        printfn "Elapsed: %A" (endTime - startTime)
        0 // return an integer exit code