module Bitboards
open FenParser
open RandomExtensions

type Bitboard = uint64


//Magic numbers taken from http://www.afewmorelines.com/understanding-magic-bitboards-in-chess-programming/ (also at http://www.rivalchess.com/magic-bitboards/)
let private magicNumberRook = [|
        0xa180022080400230UL; 0x40100040022000UL; 0x80088020001002UL; 0x80080280841000UL; 0x4200042010460008UL; 0x4800a0003040080UL; 0x400110082041008UL; 0x8000a041000880UL; 0x10138001a080c010UL; 0x804008200480UL; 0x10011012000c0UL; 0x22004128102200UL; 0x200081201200cUL; 0x202a001048460004UL; 0x81000100420004UL; 0x4000800380004500UL; 0x208002904001UL; 0x90004040026008UL; 0x208808010002001UL; 0x2002020020704940UL; 0x8048010008110005UL; 0x6820808004002200UL; 0xa80040008023011UL; 0xb1460000811044UL; 0x4204400080008ea0UL; 0xb002400180200184UL; 0x2020200080100380UL; 0x10080080100080UL; 0x2204080080800400UL; 0xa40080360080UL; 0x2040604002810b1UL; 0x8c218600004104UL; 0x8180004000402000UL; 0x488c402000401001UL; 0x4018a00080801004UL; 0x1230002105001008UL; 0x8904800800800400UL; 0x42000c42003810UL; 0x8408110400b012UL; 0x18086182000401UL; 0x2240088020c28000UL; 0x1001201040c004UL; 0xa02008010420020UL; 0x10003009010060UL; 0x4008008008014UL; 0x80020004008080UL; 0x282020001008080UL; 0x50000181204a0004UL; 0x102042111804200UL; 0x40002010004001c0UL; 0x19220045508200UL; 0x20030010060a900UL; 0x8018028040080UL; 0x88240002008080UL; 0x10301802830400UL; 0x332a4081140200UL; 0x8080010a601241UL; 0x1008010400021UL; 0x4082001007241UL; 0x211009001200509UL; 0x8015001002441801UL; 0x801000804000603UL; 0xc0900220024a401UL; 0x1000200608243UL
    |]

let private magicNumberBishop = [|
        0x2910054208004104UL; 0x2100630a7020180UL; 0x5822022042000000UL; 0x2ca804a100200020UL; 0x204042200000900UL; 0x2002121024000002UL; 0x80404104202000e8UL; 0x812a020205010840UL; 0x8005181184080048UL; 0x1001c20208010101UL; 0x1001080204002100UL; 0x1810080489021800UL; 0x62040420010a00UL; 0x5028043004300020UL; 0xc0080a4402605002UL; 0x8a00a0104220200UL; 0x940000410821212UL; 0x1808024a280210UL; 0x40c0422080a0598UL; 0x4228020082004050UL; 0x200800400e00100UL; 0x20b001230021040UL; 0x90a0201900c00UL; 0x4940120a0a0108UL; 0x20208050a42180UL; 0x1004804b280200UL; 0x2048020024040010UL; 0x102c04004010200UL; 0x20408204c002010UL; 0x2411100020080c1UL; 0x102a008084042100UL; 0x941030000a09846UL; 0x244100800400200UL; 0x4000901010080696UL; 0x280404180020UL; 0x800042008240100UL; 0x220008400088020UL; 0x4020182000904c9UL; 0x23010400020600UL; 0x41040020110302UL; 0x412101004020818UL; 0x8022080a09404208UL; 0x1401210240484800UL; 0x22244208010080UL; 0x1105040104000210UL; 0x2040088800c40081UL; 0x8184810252000400UL; 0x4004610041002200UL; 0x40201a444400810UL; 0x4611010802020008UL; 0x80000b0401040402UL; 0x20004821880a00UL; 0x8200002022440100UL; 0x9431801010068UL; 0x1040c20806108040UL; 0x804901403022a40UL; 0x2400202602104000UL; 0x208520209440204UL; 0x40c000022013020UL; 0x2000104000420600UL; 0x400000260142410UL; 0x800633408100500UL; 0x2404080a1410UL; 0x138200122002900UL    
    |]


let private occupancyMaskRook = [|
        0x101010101017eUL; 0x202020202027cUL; 0x404040404047aUL; 0x8080808080876UL; 0x1010101010106eUL; 0x2020202020205eUL; 0x4040404040403eUL; 0x8080808080807eUL; 0x1010101017e00UL; 0x2020202027c00UL; 0x4040404047a00UL; 0x8080808087600UL; 0x10101010106e00UL; 0x20202020205e00UL; 0x40404040403e00UL; 0x80808080807e00UL; 0x10101017e0100UL; 0x20202027c0200UL; 0x40404047a0400UL; 0x8080808760800UL; 0x101010106e1000UL; 0x202020205e2000UL; 0x404040403e4000UL; 0x808080807e8000UL; 0x101017e010100UL; 0x202027c020200UL; 0x404047a040400UL; 0x8080876080800UL; 0x1010106e101000UL; 0x2020205e202000UL; 0x4040403e404000UL; 0x8080807e808000UL; 0x1017e01010100UL; 0x2027c02020200UL; 0x4047a04040400UL; 0x8087608080800UL; 0x10106e10101000UL; 0x20205e20202000UL; 0x40403e40404000UL; 0x80807e80808000UL; 0x17e0101010100UL; 0x27c0202020200UL; 0x47a0404040400UL; 0x8760808080800UL; 0x106e1010101000UL; 0x205e2020202000UL; 0x403e4040404000UL; 0x807e8080808000UL; 0x7e010101010100UL; 0x7c020202020200UL; 0x7a040404040400UL; 0x76080808080800UL; 0x6e101010101000UL; 0x5e202020202000UL; 0x3e404040404000UL; 0x7e808080808000UL; 0x7e01010101010100UL; 0x7c02020202020200UL; 0x7a04040404040400UL; 0x7608080808080800UL; 0x6e10101010101000UL; 0x5e20202020202000UL; 0x3e40404040404000UL; 0x7e80808080808000UL 
    |]

let private occupancyMaskBishop = [|
        0x40201008040200UL; 0x402010080400UL; 0x4020100a00UL; 0x40221400UL; 0x2442800UL; 0x204085000UL; 0x20408102000UL; 0x2040810204000UL; 0x20100804020000UL; 0x40201008040000UL; 0x4020100a0000UL; 0x4022140000UL; 0x244280000UL; 0x20408500000UL; 0x2040810200000UL; 0x4081020400000UL; 0x10080402000200UL; 0x20100804000400UL; 0x4020100a000a00UL; 0x402214001400UL; 0x24428002800UL; 0x2040850005000UL; 0x4081020002000UL; 0x8102040004000UL; 0x8040200020400UL; 0x10080400040800UL; 0x20100a000a1000UL; 0x40221400142200UL; 0x2442800284400UL; 0x4085000500800UL; 0x8102000201000UL; 0x10204000402000UL; 0x4020002040800UL; 0x8040004081000UL; 0x100a000a102000UL; 0x22140014224000UL; 0x44280028440200UL; 0x8500050080400UL; 0x10200020100800UL; 0x20400040201000UL; 0x2000204081000UL; 0x4000408102000UL; 0xa000a10204000UL; 0x14001422400000UL; 0x28002844020000UL; 0x50005008040200UL; 0x20002010080400UL; 0x40004020100800UL; 0x20408102000UL; 0x40810204000UL; 0xa1020400000UL; 0x142240000000UL; 0x284402000000UL; 0x500804020000UL; 0x201008040200UL; 0x402010080400UL; 0x2040810204000UL; 0x4081020400000UL; 0xa102040000000UL; 0x14224000000000UL; 0x28440200000000UL; 0x50080402000000UL; 0x20100804020000UL; 0x40201008040200UL     
    |]

let private magicNumberShiftsRook = [|
        52; 53; 53; 53; 53; 53; 53; 52; 53; 54; 54; 54; 54; 54; 54; 53; 
        53; 54; 54; 54; 54; 54; 54; 53; 53; 54; 54; 54; 54; 54; 54; 53; 
        53; 54; 54; 54; 54; 54; 54; 53; 53; 54; 54; 54; 54; 54; 54; 53; 
        53; 54; 54; 54; 54; 54; 54; 53; 52; 53; 53; 53; 53; 53; 53; 52
    |]

let private magicNumberShiftsBishop = [|
        58; 59; 59; 59; 59; 59; 59; 58; 59; 59; 59; 59; 59; 59; 59; 59; 
        59; 59; 57; 57; 57; 57; 59; 59; 59; 59; 57; 55; 55; 57; 59; 59; 
        59; 59; 57; 55; 55; 57; 59; 59; 59; 59; 57; 57; 57; 57; 59; 59; 
        59; 59; 59; 59; 59; 59; 59; 59; 58; 59; 59; 59; 59; 59; 59; 58
    |]

let generateOccupancyVariations (occupancyMasks:uint64[]) =
    [|
        let generateVariationsForBitRef  variationCount (setBitsInOccupancyMask:int[]) =
            [|
                for i = 0 to variationCount-1 do
                    // find bits set in index "i" and map them to bits in the 64 bit "occupancyVariation"
                    let setBitsInIndex = BitUtils.getSetBits i 
                    let variation = 
                        setBitsInIndex
                        |>  Array.fold(fun acc setBitNumber ->
                            acc ||| (1UL <<< setBitsInOccupancyMask.[setBitNumber])
                            ) 0UL
                    yield variation
            |]
        for bitRef = 0 to 63 do
            let occupancyMask = occupancyMasks.[bitRef]
            let setBitsInOccupancyMask = BitUtils.getSetBits occupancyMask
            let variationCount = 1 <<< (BitUtils.countSetBits occupancyMask)
            #if DIAG
            let actualBitCount = BitUtils.countSetBits occupancyMask
            if actualBitCount > 12 then raise (invalidOp <| sprintf "Internal error: found occupancy mask with more than 12 bits set: %d" actualBitCount)
            #endif
            let variationsForBitRef = setBitsInOccupancyMask |> generateVariationsForBitRef variationCount 
            yield variationsForBitRef
    |]

let generateRookMagicMoves (occupancyMasks:uint64[]) (magicNumbers:uint64[]) (magicNumberShifts:int[]) (occupancyVariations:uint64[][])  =
    //Magic moves indexes may not be consecutitve
    let mutable magicMoves:uint64[][] = [| for i in 0 .. 63 -> Array.zeroCreate (1 <<< 12) |]   //TODO - is 12 upperbound sufficient?
    //if magicMoves.[0].Length < 100 then invalidOp "Expected something else"
    for bitRef = 0 to 63 do
        let occupancyMask = occupancyMasks.[bitRef]
        let variationCount = 1 <<< (BitUtils.countSetBits occupancyMask)
        for i = 0 to variationCount-1 do
            //System.Diagnostics.Debug.Assert(occupancyVariations |> Array.length >= bitRef, "message1")
            //System.Diagnostics.Debug.Assert(occupancyVariations.[bitRef] |> Array.length >= i, "message2")
       
            let magicIndex = int (( occupancyVariations.[bitRef].[i] * magicNumbers.[bitRef]) >>> magicNumberShifts.[bitRef])

            let matchesOccupancyVariation j =
                occupancyVariations.[bitRef].[i] &&& (1UL <<< j) <> 0UL

            let squaresN = seq { for j in bitRef+8 .. 8 .. 63 -> j}
            let squaresS = seq { for j in bitRef-8 .. -8 .. 0 -> j}
            let squaresE = seq { for j in bitRef-1 .. -1 .. (bitRef/8)*8 -> j}
            let squaresW = seq { for j in bitRef+1 .. 1 .. ((bitRef/8)+1)*8-1 -> j}

            let squaresAllDirs = [squaresN; squaresS; squaresE; squaresW;]
            let nonOccupiedSquaresAllDirs = squaresAllDirs |> Seq.map (Seq.takeUntilInclusive matchesOccupancyVariation)

            let combined = nonOccupiedSquaresAllDirs |> Seq.collect id
            let moves = combined |> Seq.fold (fun (updatedMoves:uint64) j ->  (updatedMoves |> BitUtils.setBit j)) 0UL 
            //if(magicMoves.[bitRef].[magicIndex] <> 0UL) then 
            //    invalidOp "Weird, wasn't supposed to happen"

            magicMoves.[bitRef].[magicIndex] <- magicMoves.[bitRef].[magicIndex] ||| moves   //TODO - not sure if OR is acceptable here

    magicMoves


let bootstrapRookMagicMoves () =
    //TEST
    //let res = occupancyMaskRook |> generateOccupancyVariations
    //System.Diagnostics.Trace.Write("Hello")
    //System.Diagnostics.Trace.Write(res.Length)
    //System.Diagnostics.Debug.Assert(res.Length = 64, "occupancy not of size 64 !")
    occupancyMaskRook  |>  
    (generateOccupancyVariations >> generateRookMagicMoves occupancyMaskRook magicNumberRook magicNumberShiftsRook) 

let generateRookMoves (magicMovesRook:uint64[][]) (bbAllPieces:Bitboard) (bbFriendlyPieces:Bitboard) (srcIndex:int) =
    let bbBlockers = bbAllPieces &&& occupancyMaskRook.[srcIndex]
    //let databaseIndex = int ((uint64(bbBlockers) * magicNumberRook.[srcIndex]) >>> magicNumberShiftsRook.[srcIndex])
    let databaseIndexUint64 = (uint64(bbBlockers) * magicNumberRook.[srcIndex]) >>> magicNumberShiftsRook.[srcIndex]
    let databaseIndex = (int)databaseIndexUint64
    let bbMoveSquares = magicMovesRook.[srcIndex].[databaseIndex] &&& ~~~bbFriendlyPieces
    bbMoveSquares

let private fileLetters = [|'a';'b';'c';'d';'e';'f';'g';'h'|]

let bitRefToAlgebraicNotation bitRef =
    let fileIndex = 7 - (bitRef % 8) //  = squareIndex & 7
    let rankIndex   = bitRef / 8  //= squareIndex >> 3 
    sprintf "%c%d" fileLetters.[fileIndex] (rankIndex+1)

// let fenCharListsToBitboard (pieces:Board8x8Array)=
//     pieces |> 

let generateAttackSets (occupancyVariations:uint64[][]) (occupancyMasks:uint64[]) =
    [|
        for bitRef = 0 to 63 do
            let occupancyMask = occupancyMasks.[bitRef]
            let variationCount = 1 <<< (BitUtils.countSetBits occupancyMask)

            yield [|
                    for i = 0 to variationCount-1 do
                        let matchesOccupancyVariation j =
                            occupancyVariations.[bitRef].[i] &&& (1UL <<< j) <> 0UL

                        let maxMatchingOccupancyVariationInclusive = Seq.takeUntilInclusive matchesOccupancyVariation >> Seq.max
                        let minMatchingOccupancyVariationInclusive = Seq.takeUntilInclusive matchesOccupancyVariation >> Seq.min

                        let squaresN = seq { for j in bitRef+8 .. 8 .. 63 -> j}
                        let squaresS = seq { for j in bitRef-8 .. -8 .. 0 -> j}
                        let squaresE = seq { for j in bitRef-1 .. -1 .. (bitRef/8)*8 -> j}
                        let squaresW = seq { for j in bitRef+1 .. 1 .. ((bitRef/8)+1)*8-1 -> j}

                      
                        let squaresAllDirs = [|(squaresN, Seq.max); (squaresS, Seq.min); (squaresE, Seq.min); (squaresW, Seq.max)|]
                        let maxAttackedOrEdgeInAllDirs = 
                            squaresAllDirs 
                            |> Seq.where (fst >> Seq.isEmpty >> not)
                            |> Seq.map (fun (squares, minOrMax) -> 
                                squares |> Seq.takeUntilInclusive matchesOccupancyVariation |> minOrMax)
                            
                        let attackSetCombined = 
                            maxAttackedOrEdgeInAllDirs 
                            |> Seq.fold (fun (attackSetInDir:uint64) j ->  (attackSetInDir |> BitUtils.setBit j)) 0UL 
                        yield attackSetCombined

                        //for (j=bitRef+8; j<=55 && (occupancyVariation[bitRef][i] & (1L << j)) == 0; j+=8);
                        //if (j>=0 && j<=63) occupancyAttackSet[bitRef][i] |= (1L << j);
                        //for (j=bitRef-8; j>=8 && (occupancyVariation[bitRef][i] & (1L << j)) == 0; j-=8);
                        //if (j>=0 && j<=63) occupancyAttackSet[bitRef][i] |= (1L << j);
                        //for (j=bitRef+1; j%8!=7 && j%8!=0 && (occupancyVariation[bitRef][i] & (1L << j)) == 0; j++);
                        //if (j>=0 && j<=63) occupancyAttackSet[bitRef][i] |= (1L << j);
                        //for (j=bitRef-1; j%8!=7 && j%8!=0 && j>=0 && (occupancyVariation[bitRef][i] & (1L << j)) == 0; j--);
                        //if (j>=0 && j<=63) occupancyAttackSet[bitRef][i] |= (1L << j);

                 |]
         
    |]


let initRandomUInt64Generator (seed:uint64) =
    let mutable s:uint64 = seed
    let mutable counter = 0
    fun() -> 
        s <- s ^^^ (s >>> 12)
        s <- s ^^^ (s <<< 25)
        s <- s ^^^ (s >>> 27)
        counter <- counter + 1
        if(counter % 100000 = 0) then
            printfn "Total attempts: %d" counter
        s * 2685821657736338717UL

let generateMagicNumbersAndShiftsRook (occupancyMasks:uint64[]) (occupancyVariations:uint64[][]) (occupancyAttackSets:uint64[][]) =

    let rnd = System.Random()
    let generateSparseUInt64 () = rnd.NextUInt64() &&& rnd.NextUInt64() &&& rnd.NextUInt64() // generate a random number with not many bits set
    //let generateRandomUInt64 = initRandomUInt64Generator 8977UL //TODO
    //let generateSparseUInt64 () = generateRandomUInt64() &&& generateRandomUInt64() &&& generateRandomUInt64()
    let infiniteMagicSequence = 
        Seq.initInfinite (fun i -> 
            //if i > (1 <<< 32) then invalidOp("Magic number generation: Sanity check failed")
            generateSparseUInt64())

    [|
        for bitRef = 0 to 63 do
            let bitCount = BitUtils.countSetBits occupancyMaskRook.[bitRef]
            let variationCount = 1 <<< bitCount;
            let magicShift = 64-bitCount
            //let mutable usedBy = Array.zeroCreate<uint64> (1 <<< bitCount)
            let currentBitRefOccupancyVariations = occupancyVariations.[bitRef]
            let currentBitRefOccupancyMask = occupancyMaskRook.[bitRef]
            let currentBitRefAttackSet = occupancyAttackSets.[bitRef]
            //let mutable fail = true
            let mutable candidateCount = 0

            let magicNumberDoesNotClashWithAnotherOccupancyVariationAttackSet (magicNumber:uint64) =
                #if DIAG
                candidateCount <- candidateCount + 1
                if(candidateCount % 100000 = 0) then
                    printfn "Total magic attempts: %d" candidateCount
                //magicAttemptsPerBitCount.[bitRef] <- candidateCount
                #endif

                let magic32Shifted = uint32(magicNumber>>>32)
                let magicTruncated = uint32(magicNumber)
                let bitsToShiftFrom32 = 32-bitCount
                let mutable usedBy = Array.zeroCreate<uint64> (1 <<< bitCount)
                let variations = [|0..variationCount-1|]
           
                let noClashes = 
                    variations |> Array.forall (fun i -> 
                        let attackSet = currentBitRefAttackSet.[i]
                        //32-bit mult
                        // (unsigned)((int)b*(int)magic ^ (int)(b>>32)*(int)(magic>>32)) >> (32-bits);
                        let b= currentBitRefOccupancyVariations.[i]
                        //let index:int = int(uint32(uint32(b)*uint32(magicNumber) ^^^ uint32(b>>>32)*(uint32(magicNumber>>>32))  >>> (32-bitCount)))
                        let index:int = int(uint32(uint32(b)*magicTruncated ^^^ uint32(b>>>32)*magic32Shifted)  >>> bitsToShiftFrom32)
                        //let index = int((currentBitRefOccupancyVariations.[i] * magicNumber) >>> magicShift)
                        // fail if this index is used by an attack set that is incorrect for this occupancy variation
                        let collision = usedBy.[index] <> 0UL && usedBy.[index] <> attackSet
                        usedBy.[index] <- attackSet
                        not collision )
                noClashes

            let mutable decentMagicAttemptCounter = 0
            let goodMagicPredicate m =
                //if countBits_slow ((m * currentBitRefOccupancyMask) >>> 56 )  >= 6 then
                if BitUtils.Hamming.popcount_3 (uint64(uint32(m)*uint32(currentBitRefOccupancyMask) ^^^ uint32(m>>>32)*uint32(currentBitRefOccupancyMask>>>32) >>> 24)) >= 6 then
                    true
                else
                    //System.Threading.Interlocked.Increment()
                    //let  g= decentMagicAttemptCounter + 1
                    #if DIAGPLUS
                    decentMagicAttemptCounter <- decentMagicAttemptCounter + 1
                    if(decentMagicAttemptCounter % 100000 = 0) then
                        printfn "Total decent magic  attempts: %d" decentMagicAttemptCounter
                    #endif
                    false

            let curatedInfiniteMagicSequence = 
                infiniteMagicSequence 
                //|> Seq.where (fun m -> countSetBits ((m * occupancyMaskRook.[bitRef]) &&&  0xFF00000000000000UL )  >= 6)
                //|> Seq.where (fun m -> countBits_slow ((m * currentBitRefOccupancyMask) >>> 56 )  >= 6)
                |> Seq.where goodMagicPredicate
            let magicNumber = curatedInfiniteMagicSequence |> Seq.find magicNumberDoesNotClashWithAnotherOccupancyVariationAttackSet
            //let magicNumber = curatedInfiniteMagicSequence |> Seq.take 500 |> Seq.last  //test only!!!
            printfn "Found magic number for bitref %d: %x" bitRef magicNumber
            printfn "Magic attempts: %d" candidateCount
            yield (magicNumber, magicShift)
     |]         
            

    //        while fail do
    //            let magicNumber = generateSparseUInt64()
    //            for (j=0; j<variationCount; j++) usedBy[j] = 0;
    //            //TODO
    //        yield bitRef
    //|]



let bootstrapMagicNumberGenerationForRook () =
    let occupancyVariations = occupancyMaskRook  |>  generateOccupancyVariations
    //if(occupancyVariations[0] |> Array.forall (fun x -> x > 0)))
    let attackSets = generateAttackSets occupancyVariations occupancyMaskRook |> Array.ofSeq
    let magick = generateMagicNumbersAndShiftsRook occupancyMaskRook occupancyVariations attackSets
    magick |> Array.ofSeq

//public void generateMagicNumbers(boolean isRook)
    //{
    //    int i, j, bitRef, variationCount;
        
    //    Random r = new Random();
    //    long magicNumber = 0;
    //    int index;
    //    long attackSet;
        
    //    for (bitRef=0; bitRef<=63; bitRef++)
    //    {
    //        int bitCount = Bitboards.countSetBits(isRook ? occupancyMaskRook[bitRef] : occupancyMaskBishop[bitRef]);
    //        variationCount = (int)(1L << bitCount);
    //        boolean fail;
    //        long usedBy[] = new long[(int)(1L << bitCount)];

    //        int attempts = 0;
            
    //        do
    //        {
    //            magicNumber = r.nextLong() & r.nextLong() & r.nextLong(); // generate a random number with not many bits set
    //            for (j=0; j<variationCount; j++) usedBy[j] = 0;
    //            attempts ++;
                
    //            for (i=0, fail=false; i<variationCount && !fail; i++)
    //            {
    //                index = (int)((occupancyVariation[bitRef][i] * magicNumber) >>> (64-bitCount));
                    
    //                // fail if this index is used by an attack set that is incorrect for this occupancy variation
    //                fail = usedBy[index] != 0 && usedBy[index] != occupancyAttackSet[bitRef][i];
                    
    //                usedBy[index] = attackSet;
    //            }
    //        } 
    //        while (fail);
            
    //        if (isRook)
    //        {
    //            magicNumberRook[bitRef] = magicNumber;
    //            magicNumberShiftsRook[bitRef] = (64-bitCount);
    //        }
    //        else
    //        {
    //            magicNumberBishop[bitRef] = magicNumber;
    //            magicNumberShiftsBishop[bitRef] = (64-bitCount);
    //        }
    //    }
    //}
