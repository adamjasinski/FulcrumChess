﻿namespace FulcrumChess.Engine.Benchmarks.Prototypes

module BitUtilsPrototypes =

    open System.Runtime.Intrinsics.X86

    module Hamming = 
        //https://en.wikipedia.org/wiki/Hamming_weight
        [<Literal>] 
        let private m1  = 0x5555555555555555UL //binary: 0101...
        [<Literal>] 
        let private m2  = 0x3333333333333333UL //binary: 00110011..
        [<Literal>] 
        let private m4  = 0x0f0f0f0f0f0f0f0fUL //binary:  4 zeros,  4 ones ...
        [<Literal>] 
        let private m8  = 0x00ff00ff00ff00ffUL //binary:  8 zeros,  8 ones ...
        [<Literal>] 
        let private m16 = 0x0000ffff0000ffffUL //binary: 16 zeros, 16 ones ...
        [<Literal>] 
        let private m32 = 0x00000000ffffffffUL //binary: 32 zeros, 32 ones
        [<Literal>] 
        let private hff = 0xffffffffffffffffUL //binary: all ones
        [<Literal>] 
        let private h01 = 0x0101010101010101UL //the sum of 256 to the power of 0,1,2,3...

        let inline popcount_64 (input:uint64) =
            let mutable x = input
            //x <- x - (x >>> 1) &&& m1;             //put count of each 2 bits into those 2 bits
            //x <- (x &&& m2) + ((x >>> 2) &&& m2); //put count of each 4 bits into those 4 bits 
            //x <- (x + (x >> >4)) &&& m4;        //put count of each 8 bits into those 8 bits 
            //x <- x + (x >>>  8;  //put count of each 16 bits into their lowest 8 bits
            //x += x >>> 16;  //put count of each 32 bits into their lowest 8 bits
            //x += x >>> 32;  //put count of each 64 bits into their lowest 8 bits
            //x &&& 0x7f
            x <- x - ((x >>> 1) &&& m1)            //put count of each 2 bits into those 2 bits
            x <- (x &&& m2) + ((x >>> 2) &&& m2)  //put count of each 4 bits into those 4 bits 
            x <- (x + (x >>> 4)) &&& m4         //put count of each 8 bits into those 8 bits 
            int ((x * h01) >>> 56)                    //returns left 8 bits of x + (x<<8) + (x<<16) + (x<<24) + ... 

        let inline popcount_32 (input:uint32) =
            //https://stackoverflow.com/questions/109023/how-to-count-the-number-of-set-bits-in-a-32-bit-integer
            let mutable x = input
            x <- x - ((x >>> 1) &&& 0x55555555u);
            x <- (x &&& 0x33333333u) + ((x >>> 2) &&& 0x33333333u)
            (((x + (x >>> 4)) &&& 0x0F0F0F0Fu) * 0x01010101u) >>> 24

        let inline popcount_32_signed (input:int) =
            //https://stackoverflow.com/questions/109023/how-to-count-the-number-of-set-bits-in-a-32-bit-integer
            let mutable x = input
            x <- x - ((x >>> 1) &&& 0x55555555);
            x <- (x &&& 0x33333333) + ((x >>> 2) &&& 0x33333333)
            (((x + (x >>> 4)) &&& 0x0F0F0F0F) * 0x01010101) >>> 24

        let popcount_64_alt (input:uint64) =
            //https://stackoverflow.com/questions/2709430/count-number-of-bits-in-a-64-bit-long-big-integer
            let mutable i = input
            i <- i - ((i >>> 1) &&& 0x5555555555555555UL)
            i <- (i &&& 0x3333333333333333UL) + ((i >>> 2) &&& 0x3333333333333333UL)
            let res = (((i + (i >>> 4)) &&& 0xF0F0F0F0F0F0F0FUL * 0x101010101010101UL) >>> 56)
            int(res)


    let inline countBits_slow (b:^a)  = 
        let zero:^a = LanguagePrimitives.GenericZero<_>
        let one:^a = LanguagePrimitives.GenericOne

        let rec loop (x:^a) count =
            if(x = zero) then count
            else
                if x &&& one = one then 
                    loop (x >>> 1) (count+1)
                else 
                    loop (x >>> 1) count
        loop b 0 

    let inline setBit (i:int) (b:^a) =
        let one:^a = LanguagePrimitives.GenericOne
        b ||| (one <<< i)

    let inline clearBit (i:int) (b:^a) =
        let setBitMask:^a = LanguagePrimitives.GenericOne <<< i
        b &&& (~~~setBitMask)

    let inline getSetBits (b:^a) =
        let zero:^a = LanguagePrimitives.GenericZero
        let one:^a = LanguagePrimitives.GenericOne
        let rec loop x i acc =
            if(x = zero) then acc
            else
                if x &&& one = one then 
                    loop (x >>> 1) (i+1) (i::acc)
                else 
                    loop (x >>> 1) (i+1) acc
        loop b 0 [] 
        |> List.rev
        |> Array.ofList

    let sharedBuffers = System.Buffers.ArrayPool<int>.Shared

    let inline getSetBits_i32_leased (b:int) =
        let arr = sharedBuffers.Rent(64)
        let mutable i = 0
        let mutable x = uint32(b)
        while x > 0u do
            let lsb = int(System.Runtime.Intrinsics.X86.Bmi1.TrailingZeroCount x)
            x <- x &&& (x-1u)
            arr.[i] <- lsb
            i <- i+1
        let res = Array.zeroCreate i
        System.Array.Copy(arr, res, i)
        sharedBuffers.Return(arr)
        res
    
    let inline getSetBits_i32_anycpu (b:int) =
        let res = ResizeArray<int>(32)
        let mutable x = b
        let mutable i = 0
        while x > 0 do
            if x &&& 1 = 1 then res.Add(i)
            x <- x >>> 1
            i <- i + 1
        res.ToArray()

    let inline getSetBits_32 (b:int) =
        #if USE_INTRINSIC_BMI
        getSetBits_i32_leased b
        #else
        getSetBits_i32_anycpu
        #endif

    let inline getSetBits_u64_anycpu (b:uint64) =
        let res = ResizeArray<int>(64)
        let mutable x = b
        let mutable i = 0
        while x > 0UL do
            if x &&& 1UL = 1UL then res.Add(i)
            x <- x >>> 1
            i <- i + 1
        res.ToArray()

    // let inline getLsb b =
    //     int(System.Runtime.Intrinsics.X86.Bmi1.X64.TrailingZeroCount b)

    // let inline extractLsb b =
    //     b &&& (b-1)

    let inline getSetBits_u64_resizearray_intrinsic(b:uint64) =
        let res = ResizeArray<int>(64)
        let mutable x = b
        while x > 0UL do
            let lsb = int(System.Runtime.Intrinsics.X86.Bmi1.X64.TrailingZeroCount x)
            res.Add(lsb)
            x <- x &&& (x-1UL)
        res.ToArray()

    let inline getSetBits_u64_seq (b:uint64) = seq {
        let mutable x = b
        while x > 0UL do
            let lsb = int(System.Runtime.Intrinsics.X86.Bmi1.X64.TrailingZeroCount x)
            x <- x &&& (x-1UL)
            yield lsb
        }

    let inline getSetBits_u64_span (b:uint64) =
        let arr = Array.zeroCreate 64
        let spaner = System.Span(arr)
        let mutable i = 0
        let mutable x = b
        while x > 0UL do
            let lsb = int(System.Runtime.Intrinsics.X86.Bmi1.X64.TrailingZeroCount x)
            x <- x &&& (x-1UL)
            arr.[i] <- lsb
            i <- i+1
        spaner.Slice(0, i)

    let inline getSetBits_u64_leased (b:uint64) =
        let arr = sharedBuffers.Rent(64)
        let mutable i = 0
        let mutable x = b
        while x > 0UL do
            let lsb = int(System.Runtime.Intrinsics.X86.Bmi1.X64.TrailingZeroCount x)
            x <- System.Runtime.Intrinsics.X86.Bmi1.X64.ResetLowestSetBit x
            arr.[i] <- lsb
            i <- i+1
        let res = Array.zeroCreate i
        System.Array.Copy(arr, res, i)
        sharedBuffers.Return(arr)
        res

    let inline getSetBits_u64_stackalloc (b:uint64) =
        let span = SpanT.createOnStack 64
        let mutable i = 0
        let mutable x = b
        while x > 0UL do
            let lsb = int(System.Runtime.Intrinsics.X86.Bmi1.X64.TrailingZeroCount x)
            x <- System.Runtime.Intrinsics.X86.Bmi1.X64.ResetLowestSetBit x
            span.[i] <- lsb
            i <- i+1
        span.Slice(0, i).ToArray()

    let inline getSetBits_u64_stackalloc_pur (b:uint64) =
        let span = SpanT.createOnStack 64
        let mutable i = 0
        let mutable x = b
        while x > 0UL do
            let lsb = int(System.Runtime.Intrinsics.X86.Bmi1.X64.TrailingZeroCount x)
            x <- System.Runtime.Intrinsics.X86.Bmi1.X64.ResetLowestSetBit x
            span.[i] <- lsb
            i <- i+1
        span.Slice(0, i)

    let inline getSetBits_u64_double_leased (b:uint64) =
        let arr = System.Buffers.ArrayPool<int>.Shared.Rent(64)
        let mutable i = 0
        let mutable x = b
        while x > 0UL do
            let lsb = int(System.Runtime.Intrinsics.X86.Bmi1.X64.TrailingZeroCount x)
            x <- x &&& (x-1UL)
            arr.[i] <- lsb
            i <- i+1
        let res = System.Buffers.ArrayPool<int>.Shared.Rent(i)
        System.Array.Copy(arr, res, i)
        System.Buffers.ArrayPool<int>.Shared.Return(arr)
        res
        
    let inline getSetBits_u64 (b:uint64) =
        #if USE_INTRINSIC_BMI
        getSetBits_u64_leased b
        #else
        getSetBits_u64_anycpu b
        #endif
        
    let inline countSetBits b = 
        #if USE_INTRINSIC_POPCNT
        int(System.Runtime.Intrinsics.X86.Popcnt.X64.PopCount b)
        #else
        Hamming.popcount_64 b
        #endif

    let inline countSetBits_32 b = 
        #if USE_INTRINSIC_POPCNT
        int(System.Runtime.Intrinsics.X86.Popcnt.PopCount b)
        #else
        Hamming.popcount_32 b
        #endif

    let inline hasBitSet (i:int) (b:^a) =
        let one:^a = LanguagePrimitives.GenericOne
        b &&& (one <<<  i) > LanguagePrimitives.GenericZero
