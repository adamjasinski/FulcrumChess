namespace FenParser

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
    open RandomExtensions
    let initRandomUInt64Generator (seed:uint64) =
        let mutable m:uint64 = seed
        //let m = new System.Threading.ThreadLocal<uint64>(fun () -> seed)
        let mutable counter = 0
        let mylock = obj()
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

    let generateRandomUInt64 = initRandomUInt64Generator 8977UL //TODO

    //let generateSparseUInt64 () = generateRandomUInt64() &&& generateRandomUInt64() &&& generateRandomUInt64()

    //let randomLock = obj()

    let rnd = new System.Threading.ThreadLocal<System.Random>(fun () -> new System.Random())

    //let initThreadLocalRandom() = System.Threading.ThreadLocal<RandomExtensions>

    //let rnd = System.Random()
    //let generateSparseUInt64 () = lock randomLock (fun () -> rnd.NextUInt64() &&& rnd.NextUInt64() &&& rnd.NextUInt64()) // generate a random number with not many bits set
    let generateSparseUInt64 () = 
        let currentRnd = rnd.Value
        currentRnd.NextUInt64() &&& currentRnd.NextUInt64() &&& currentRnd.NextUInt64()  // generate a random number with not many bits set


    //let generateSparseUInt64 () = lock randomLock (fun () -> uint64(rnd.Next() &&& rnd.Next()) <<< 32)  // generate a random number with not many bits set
