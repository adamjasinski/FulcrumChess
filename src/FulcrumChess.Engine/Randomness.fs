namespace FulcrumChess.Engine

module RandomExtensions = //no pun intended
    open System

    type Random with 
        member x.NextInt64 () = 
            let buffer = Array.zeroCreate sizeof<int64>
            x.NextBytes(buffer)
            BitConverter.ToInt64(buffer, 0)

        member x.NextUInt64 () = 
            let buffer = Array.zeroCreate sizeof<uint64>
            x.NextBytes(buffer)
            BitConverter.ToUInt64(buffer, 0)

module Randomness =
    open System
    open System.Threading
    open RandomExtensions

    module SequentialRandomness = 
        let randomLocks = [|for i in 0..7 -> obj()|]

        let initRandomUInt64Generator (seed:uint64) (rank:int)=
            let mutable m:uint64 = seed
            //let m = new System.Threading.ThreadLocal<uint64>(fun () -> seed)
            let mutable counter = 0
            let mylock = randomLocks.[rank]
            fun() -> 
                let s1 = m
                let s2 = s1 ^^^ (s1 >>> 12)
                let s3 = s2 ^^^ (s2 <<< 25)
                let s4 = s3 ^^^ (s3 >>> 27)
                //counter <- counter + 1
                //if(counter % 100000 = 0) then
                    //printfn "Total attempts: %d" counter
                let s5 = s4 * 2685821657736338717UL
                lock mylock (fun () -> m <- s5)
                //let refUint64:uint64 ref = ref 0UL
                //let cpl = (refUint64, s5)
                //System.Threading.Interlocked.Exchange(refUint64, s5) |> ignore
                //m <- s5
                //m.Value <- s5
                s5

        // NB - constant seed makes the 'random' sequence deterministic, but it also makes magic pre-generation and re-use possible
        //let generateRandomUInt64 = initRandomUInt64Generator 8977UL  //good seed for 32-bit

        let rankSeeds = [| 728UL; 10316UL; 55013UL; 32803UL; 12281UL; 15100UL; 16645UL; 255UL |]

        /// Generate a random number with not many bits set
        let generateSparseUInt64 rank = 
            let generateRandomUInt64 = initRandomUInt64Generator rankSeeds.[rank] rank//728UL     //good seed for 64-bit

            generateRandomUInt64() &&& generateRandomUInt64() &&& generateRandomUInt64()

    module SystemRandomness =
        let randomLocks = [|for i in 0..7 -> obj()|]

        // let rnd = 
        //     new ThreadLocal<Random>(
        //         fun () -> new Random())

        // let initThreadLocalRandom() = System.Threading.ThreadLocal<RandomExtensions>
        let rankSeeds = [| 728; 10316; 55013; 32803; 12281; 15100; 16645; 255 |]
        let rnds = rankSeeds |> Array.map Random
        
        let generateSparseUInt64 rank = 
            lock randomLocks.[rank] <| fun () -> 
                let rnd = rnds.[rank]
                rnd.NextUInt64() &&& rnd.NextUInt64() &&& rnd.NextUInt64() // generate a random number with not many bits set
        
        // let generateSparseUInt64 () = 
        //     let currentRnd = rnd.Value
        //     currentRnd.NextUInt64() &&& currentRnd.NextUInt64() &&& currentRnd.NextUInt64()  // generate a random number with not many bits set

    let infiniteSparseUInt64SequenceFor (rank:int) = 
        Seq.initInfinite (fun _ -> 
            SystemRandomness.generateSparseUInt64 rank)