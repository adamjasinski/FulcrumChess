// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
namespace FenParser

module Program =
    [<EntryPoint>]
    let main argv = 
        printfn "%A" argv
        System.Diagnostics.Debug.Assert(false, "Arbitrary assert failed")
        let sampleRookBitboardDatabase = Bitboards.bootstrapRookDatabase()
        sampleRookBitboardDatabase |> Array.collect id |> Array.length |> printfn "All rook variations: %d"
        printfn "%A" sampleRookBitboardDatabase.Length
        0 // return an integer exit code