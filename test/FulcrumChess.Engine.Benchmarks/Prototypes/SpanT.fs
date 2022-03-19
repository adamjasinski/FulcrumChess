namespace FulcrumChess.Engine.Benchmarks.Prototypes
open System

#nowarn "9"  //disable unsafe/unverifiable code warning

module SpanT =

    let inline createOnStack<'T> (count:int) =
        let mem = FSharp.NativeInterop.NativePtr.stackalloc<int>(count) |> FSharp.NativeInterop.NativePtr.toVoidPtr
        System.Span<'T>(mem, count)

    let iter f (span:Span<'T>) =
        for i in 0..span.Length-1 do
            span.[i] |> f

    let fold<'T,'State> (folder : 'State -> 'T -> 'State) (state: 'State) (span:Span<'T>) =
        let mutable state' = state
        for i in 0..span.Length-1 do
            state' <- folder state span.[i]
        state'

    let safeSum (bytes: Span<byte>) =
        let mutable sum = 0
        for i in 0..bytes.Length - 1 do
            sum <- sum + int bytes.[i]
        sum

    // let findMaxByIndex projection (arr:Span<'T>) =
    //     if arr.Length = 0 then invalidArg "arr" "arr needs to have at least 1 element"
    //     let mutable max_index = 0
    //     let mutable max_score = arr.[0] |> projection
    //     for i in 1..moves.Length-1 do
    //         let curr_score = arr.[i] |> projection
    //         if curr_score > max_score then
    //             max_score <- curr_score
    //             max_index <- i
    //     max_index   

    let swap<'T> i j (arr:Span<'T>) =
        let temp = arr.[i]
        arr.[i] <- arr.[j]
        arr.[j] <- temp

    // let fff () =
    //     let arrayMemory = Array.zeroCreate<byte>(100)
    //     let arraySpan = new Span<byte>(arrayMemory)

    //     let res = safeSum arraySpan 
    //     res |> printfn "res = %d"

     // // native memory
    // let nativeMemory = Marshal.AllocHGlobal(100);
    // let nativeSpan = new Span<byte>(nativeMemory.ToPointer(), 100)

    // safeSum(nativeSpan) |> printfn "res = %d"
    // Marshal.FreeHGlobal(nativeMemory)

    // // stack memory
    // let mem = NativePtr.stackalloc<byte>(100)
    // let mem2 = mem |> NativePtr.toVoidPtr
    // let stackSpan = Span<byte>(mem2, 100)

    // safeSum(stackSpan) |> printfn "res = %d"