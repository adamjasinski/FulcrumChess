namespace FulcrumChess.Engine.Benchmarks

open System
open BenchmarkDotNet.Attributes
open FulcrumChess.Engine
open BenchmarkDotNet.Running
open FulcrumChess.Engine.Benchmarks.Prototypes

[<SimpleJob(10, 1, 1, 10, null, false)>]
[<MemoryDiagnoser>]
type BitUtilsSuite() =

    let samples = [| 0x4402c00444840101UL; 10316UL; 0x80088020001002UL; 32803UL; 12281UL; 0x4004020805000806UL; 16645UL; 0x8022c0440004041UL |]
    let multiplicatedSamples = 
        samples
        |> Array.collect (fun x -> Array.replicate 1000 x)


    [<Benchmark>]
    member this.getSetBits_u64_anycpu_simple() = 
        multiplicatedSamples |> Array.iter (fun input ->
            input
            |> BitUtilsPrototypes.getSetBits_u64_anycpu
            |> Array.fold (fun acc x ->  acc ^^^ (x*2)) 0
            |> ignore
        )

    [<Benchmark>]
    member this.getSetBits_u64_intrinsic_simple() = 
        multiplicatedSamples |> Array.iter (fun input ->
            input
            |> BitUtilsPrototypes.getSetBits_u64_resizearray_intrinsic
            |> Array.fold (fun acc x ->  acc ^^^ (x*2)) 0
            |> ignore
        )

    [<Benchmark>]
    member this.getSetBits_u64_intrinsic_simple_plus_seq() = 
        multiplicatedSamples |> Array.iter (fun input ->
            input
            |> BitUtilsPrototypes.getSetBits_u64_resizearray_intrinsic
            |> Seq.fold (fun acc x ->  acc ^^^ (x*2)) 0
            |> ignore
        )

    [<Benchmark>]
    member this.getSetBits_u64_intrinsic_two_steps() = 
        multiplicatedSamples |> Array.iter (fun input ->
            input 
            |> BitUtilsPrototypes.getSetBits_u64_resizearray_intrinsic
            |> Array.map (fun x -> x*2)
            |> Array.reduce (^^^)
            |> ignore
        )

    [<Benchmark>]
    member this.getSetBits_u64_intrinsic_three_steps() = 
        multiplicatedSamples |> Array.iter (fun input ->
            input 
            |> BitUtilsPrototypes.getSetBits_u64_resizearray_intrinsic
            |> Array.map (fun x -> x*2)
            |> Array.map id
            |> Array.reduce (^^^)
            |> ignore
        )

    [<Benchmark>]
    member this.getSetBits_u64_seq_simple() = 
        multiplicatedSamples |> Array.iter (fun input ->
            input
            |> BitUtilsPrototypes.getSetBits_u64_seq
            |> Seq.fold (fun acc x ->  acc ^^^ (x*2)) 0
            |> ignore
        )

    [<Benchmark>]
    member this.getSetBits_u64_seq_two_steps() = 
        multiplicatedSamples |> Array.iter (fun input ->
            input
            |> BitUtilsPrototypes.getSetBits_u64_seq
            |> Seq.map (fun x -> x*2)
            |> Seq.reduce (^^^)
            |> ignore
        )

    [<Benchmark>]
    member this.getSetBits_u64_seq_three_steps() = 
        multiplicatedSamples |> Array.iter (fun input ->
            input
            |> BitUtilsPrototypes.getSetBits_u64_seq
            |> Seq.map (fun x -> x*2)
            |> Seq.map id
            |> Seq.reduce (^^^)
            |> ignore
        )

    [<Benchmark>]
    member this.getSetBits_u64_span_simple() = 
        multiplicatedSamples |> Array.iter (fun input ->
            let setBits:Span<int> = BitUtilsPrototypes.getSetBits_u64_span input
            let res = SpanT.fold (fun acc x ->  acc ^^^ (x*2)) 0 setBits
            res |> ignore
        )

    [<Benchmark>]
    member this.getSetBits_u64_leased_simple() = 
        multiplicatedSamples |> Array.iter (fun input ->
            let setBits = BitUtilsPrototypes.getSetBits_u64_leased input
            let res = Array.fold (fun acc x ->  acc ^^^ (x*2)) 0 setBits
            res |> ignore
        )

    [<Benchmark>]
    member this.getSetBits_u64_double_leased_simple() = 
        multiplicatedSamples |> Array.iter (fun input ->
            let setBits = BitUtilsPrototypes.getSetBits_u64_double_leased input
            let res = Array.fold (fun acc x ->  acc ^^^ (x*2)) 0 setBits
            System.Buffers.ArrayPool<int>.Shared.Return(setBits)
            res |> ignore
        )

    [<Benchmark>]
    member this.getSetBits_u64_stackalloc_simple() = 
        multiplicatedSamples |> Array.iter (fun input ->
            let setBits = BitUtilsPrototypes.getSetBits_u64_stackalloc input
            let res = Array.fold (fun acc x ->  acc ^^^ (x*2)) 0 setBits
            res |> ignore
        )

    [<Benchmark>]
    member this.getSetBits_u64_stackalloc_pur() = 
        multiplicatedSamples |> Array.iter (fun input ->
            let setBits:Span<int> = BitUtilsPrototypes.getSetBits_u64_stackalloc_pur input
            let res = SpanT.fold (fun acc x ->  acc ^^^ (x*2)) 0 setBits
            res |> ignore
        )