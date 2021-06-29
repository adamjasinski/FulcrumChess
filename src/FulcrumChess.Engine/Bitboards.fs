module Bitboards
open FulcrumChess.Engine

module Constants =
    let occupancyMaskRook = [|
            0x101010101017eUL; 0x202020202027cUL; 0x404040404047aUL; 0x8080808080876UL; 0x1010101010106eUL; 0x2020202020205eUL; 0x4040404040403eUL; 0x8080808080807eUL; 0x1010101017e00UL; 0x2020202027c00UL; 0x4040404047a00UL; 0x8080808087600UL; 0x10101010106e00UL; 0x20202020205e00UL; 0x40404040403e00UL; 0x80808080807e00UL; 0x10101017e0100UL; 0x20202027c0200UL; 0x40404047a0400UL; 0x8080808760800UL; 0x101010106e1000UL; 0x202020205e2000UL; 0x404040403e4000UL; 0x808080807e8000UL; 0x101017e010100UL; 0x202027c020200UL; 0x404047a040400UL; 0x8080876080800UL; 0x1010106e101000UL; 0x2020205e202000UL; 0x4040403e404000UL; 0x8080807e808000UL; 0x1017e01010100UL; 0x2027c02020200UL; 0x4047a04040400UL; 0x8087608080800UL; 0x10106e10101000UL; 0x20205e20202000UL; 0x40403e40404000UL; 0x80807e80808000UL; 0x17e0101010100UL; 0x27c0202020200UL; 0x47a0404040400UL; 0x8760808080800UL; 0x106e1010101000UL; 0x205e2020202000UL; 0x403e4040404000UL; 0x807e8080808000UL; 0x7e010101010100UL; 0x7c020202020200UL; 0x7a040404040400UL; 0x76080808080800UL; 0x6e101010101000UL; 0x5e202020202000UL; 0x3e404040404000UL; 0x7e808080808000UL; 0x7e01010101010100UL; 0x7c02020202020200UL; 0x7a04040404040400UL; 0x7608080808080800UL; 0x6e10101010101000UL; 0x5e20202020202000UL; 0x3e40404040404000UL; 0x7e80808080808000UL 
        |]

    let occupancyMaskBishop = [|
            0x40201008040200UL; 0x402010080400UL; 0x4020100a00UL; 0x40221400UL; 0x2442800UL; 0x204085000UL; 0x20408102000UL; 0x2040810204000UL; 0x20100804020000UL; 0x40201008040000UL; 0x4020100a0000UL; 0x4022140000UL; 0x244280000UL; 0x20408500000UL; 0x2040810200000UL; 0x4081020400000UL; 0x10080402000200UL; 0x20100804000400UL; 0x4020100a000a00UL; 0x402214001400UL; 0x24428002800UL; 0x2040850005000UL; 0x4081020002000UL; 0x8102040004000UL; 0x8040200020400UL; 0x10080400040800UL; 0x20100a000a1000UL; 0x40221400142200UL; 0x2442800284400UL; 0x4085000500800UL; 0x8102000201000UL; 0x10204000402000UL; 0x4020002040800UL; 0x8040004081000UL; 0x100a000a102000UL; 0x22140014224000UL; 0x44280028440200UL; 0x8500050080400UL; 0x10200020100800UL; 0x20400040201000UL; 0x2000204081000UL; 0x4000408102000UL; 0xa000a10204000UL; 0x14001422400000UL; 0x28002844020000UL; 0x50005008040200UL; 0x20002010080400UL; 0x40004020100800UL; 0x20408102000UL; 0x40810204000UL; 0xa1020400000UL; 0x142240000000UL; 0x284402000000UL; 0x500804020000UL; 0x201008040200UL; 0x402010080400UL; 0x2040810204000UL; 0x4081020400000UL; 0xa102040000000UL; 0x14224000000000UL; 0x28440200000000UL; 0x50080402000000UL; 0x20100804020000UL; 0x40201008040200UL     
        |]



let getOccupancyMask  = function
        | SlidingPiece.Rook -> Constants.occupancyMaskRook
        | SlidingPiece.Bishop -> Constants.occupancyMaskBishop

let generateOccupancyVariations (occupancyMasks:uint64[]) =
    [|
        let generateVariationsForBitRef  variationCount (setBitsInOccupancyMask:int[]) =
            [|
                for i = 0 to variationCount-1 do
                    // Find bits set in index "i" and map them to bits in the 64 bit "occupancyVariation"
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
            let variationsForBitRef = setBitsInOccupancyMask |> generateVariationsForBitRef variationCount 
            yield variationsForBitRef
    |]

/// <summary>The Magic Hashing function.</summary>
/// <remarks>See 'Magic Move-Bitboard Generation in Computer Chess' by Pradyumna Kannan; 'A Faster Magic Move Bitboard Generator?' by Grant Osborne</remarks>
let inline multiplyAndShift (occupancyVariation:uint64) (magicNumber:uint64) shift64 =
#if FAST_32BIT_MULT
    let shift32 = shift64-32
    let unsignedResultRaw = (uint32(occupancyVariation)*uint32(magicNumber)) ^^^ (uint32(occupancyVariation>>>32)*uint32(magicNumber>>>32))
    int(unsignedResultRaw >>> shift32)
#else
    int((occupancyVariation * magicNumber) >>> shift64)
#endif
   
let private generateSquaresInAllDirsForSlidingPieces (pc:SlidingPiece) (bitRef:int)=
    match pc with
    | SlidingPiece.Rook -> 
        let squaresN = seq { for j in bitRef+8 .. 8 .. 63 -> j}
        let squaresW = seq { for j in bitRef+1 .. 1 .. ((bitRef/8)+1)*8-1 -> j}
        let squaresS = seq { for j in bitRef-8 .. -8 .. 0 -> j}
        let squaresE = seq { for j in bitRef-1 .. -1 .. (bitRef/8)*8 -> j}
        [squaresN; squaresW; squaresS; squaresE]
    | SlidingPiece.Bishop ->
        let rec squaresNWgen j = seq { yield j; if (j+1)%8 <> 0 && j <= 54 then yield! squaresNWgen (j+9) }
        let rec squaresNEgen j = seq { yield j; if j%8 <> 0 && j <= 55 then yield! squaresNEgen (j+7) }
        let rec squaresSEgen j = seq { yield j; if j%8 <> 0 && j >= 9 then yield! squaresSEgen (j-9) }
        let rec squaresSWgen j = seq { yield j; if (j+1)%8 <> 0 && j >=8 then yield! squaresSWgen (j-7) }
        let applyAndSkipOne (f:int->int seq) = (f bitRef) |> Seq.skip 1
        [squaresNWgen; squaresNEgen; squaresSEgen; squaresSWgen] |> List.map ((fun f -> f bitRef) >> Seq.tail)


let private generateSquaresInAllDirsWithMixMaxFunctionsForSlidingPieces (piece:SlidingPiece) (bitRef:int)=
    let squares = generateSquaresInAllDirsForSlidingPieces piece bitRef
    let squaresAndFuns = 
        match squares with
        | [a;b;c;d] -> [(a,Seq.max); (b,Seq.max); (c,Seq.min); (d,Seq.min)]
        | _ -> invalidArg "piece" "unsupported value"
    squaresAndFuns

let createBitboardFromSetBitsSeq (setBits:int seq) =
    setBits |> Seq.fold (fun (updatedMoves:Bitboard) j ->  (updatedMoves |> BitUtils.setBit j)) 0UL 

let generateMagicMoves (pc:SlidingPiece) (occupancyMasks:uint64[]) (magicNumbersAndShifts:(uint64*int)[]) (occupancyVariations:uint64[][]) =
    let maxUpperBound = magicNumbersAndShifts |> Array.map (snd >> (-)64 ) |> Array.max //max 12 bits set - rook in a corner
    let mutable magicMoves:uint64[][] = [| for i in 0 .. 63 -> Array.zeroCreate (1 <<< maxUpperBound) |] 
    for bitRef = 0 to 63 do
        let occupancyMask = occupancyMasks.[bitRef]
        let variationCount = 1 <<< (BitUtils.countSetBits occupancyMask)
        for i = 0 to variationCount-1 do
            let (magicNumber, magicShift) = magicNumbersAndShifts.[bitRef]
            let magicIndex = multiplyAndShift occupancyVariations.[bitRef].[i] magicNumber magicShift

            let matchesOccupancyVariation j =
                occupancyVariations.[bitRef].[i] &&& (1UL <<< j) <> 0UL

            let squaresAllDirs = generateSquaresInAllDirsForSlidingPieces pc bitRef
            let nonOccupiedSquaresAllDirs = 
                squaresAllDirs 
                |> Seq.map (Seq.takeUntilInclusive matchesOccupancyVariation)

            let combined = nonOccupiedSquaresAllDirs |> Seq.collect id
            let moves = combined |> createBitboardFromSetBitsSeq

            if magicMoves.[bitRef].[magicIndex] <> 0UL && magicMoves.[bitRef].[magicIndex] <> moves then 
                invalidOp("attempt to change a previously set magic moves sequence; this indicates a problem with the magic generation")
            magicMoves.[bitRef].[magicIndex] <- moves
    magicMoves

let private generateSquaresKingMoves () =
    //Elementary King moves (no castling)
    [|
        for i = 0 to 63 do
            let setBits = seq {
                if i <= 55 then yield i+8       //N
                if (i+1)%8 <> 0  then yield i+1 //W
                if i >= 8 then yield i-8        //S
                if i%8 <> 0 then yield i-1      //E
                if (i+1)%8 <> 0 && i <= 54 then yield i+9   //NW
                if i%8 <> 0 && i <= 55 then yield i+7       //NE
                if i%8 <> 0 && i >= 9 then yield i-9        //SE
                if (i+1)%8 <> 0 && i >=8 then yield i-7     //SW
            } 
            yield setBits |> createBitboardFromSetBitsSeq
    |]

let private generateSquaresKnightMoves () =
    [|
        for i = 0 to 63 do
            let fileIndex = getFileIndex i
            let rankIndex = getRankIndex i

            let setBits = seq {
                if fileIndex >= 2 && rankIndex <= 6 then yield i+10
                if fileIndex >= 1 && rankIndex <= 5 then yield i+17
                if fileIndex <=6 && rankIndex <= 5 then yield i+15
                if fileIndex <=5 && rankIndex <= 6 then yield i+6
                if fileIndex <=5 && rankIndex >= 1 then yield i-10
                if fileIndex <=6 && rankIndex >= 2 then yield i-17
                if fileIndex >=1 && rankIndex >= 2 then yield i-15
                if fileIndex >=2 && rankIndex >= 1 then yield i-6

            }
            yield setBits |> createBitboardFromSetBitsSeq
    |]

let private generateSquaresAndCapturesForWhitePawn () =
    [|
        for i = 0 to 63 do
            let fileIndex = getFileIndex i
            let rankIndex = getRankIndex i

            let moves = seq {
                if rankIndex = 0 then yield -1 //white pawn cannot occupy the first rank
                if rankIndex >= 1 && rankIndex <= 6 then yield i+8
                if rankIndex = 1 then yield i+16
            }
            let captures = seq {
                if rankIndex = 0 then yield -1 //white pawn cannot occupy the first rank
                if rankIndex >= 1 && rankIndex <= 7 then
                    if fileIndex >=1 then yield i+9
                    if fileIndex <=6 then yield i+7
            }
            yield (moves, captures) |> Tuple2.map createBitboardFromSetBitsSeq
    |]

let private generateSquaresAndCapturesForBlackPawn () =
    [|
        for i = 0 to 63 do
            let fileIndex = getFileIndex i
            let rankIndex = getRankIndex i

            let moves = seq {
                if rankIndex = 7 then yield -1 //black pawn cannot occupy the last rank
                if rankIndex >= 1 && rankIndex <= 6 then yield i-8
                if rankIndex = 6 then yield i-16
            }
            let captures = seq {
                if rankIndex = 7 then yield -1 //black pawn cannot occupy the last rank
                if rankIndex >= 1 && rankIndex <= 7 then
                    if fileIndex >=1 then yield i-7
                    if fileIndex <=6 then yield i-9
            }
            yield (moves, captures) |> Tuple2.map createBitboardFromSetBitsSeq
    |]

let private generateEnPassantTargetsForWhite() =
    [|  
        //squares directly behind the opposing pawn (i.e. ones that a black pawn skips when moving 2 squares in the first move)
        for i = 0 to 63 do
            let rankIndex = getRankIndex i

            if rankIndex = 5 then BitUtils.setBit i 0UL else 0UL
    |]

let private generateEnPassantTargetsForBlack() =
    [|  
        //squares directly behind the opposing pawn (i.e. ones that a white pawn skips when moving 2 squares in the first move)
        for i = 0 to 63 do
            let rankIndex = getRankIndex i

            if rankIndex = 2 then BitUtils.setBit i 0UL else 0UL
    |]

let generateRookMagicMoves  = generateMagicMoves SlidingPiece.Rook
let generateBishopMagicMoves  = generateMagicMoves SlidingPiece.Bishop

let bootstrapRookMagicMoves (magicNumbersAndShifts:(uint64*int)[]) =
    let occupancyMasks = Constants.occupancyMaskRook
    occupancyMasks  |>  
    (generateOccupancyVariations >> generateRookMagicMoves occupancyMasks magicNumbersAndShifts) 

let bootstrapBishopMagicMoves (magicNumbersAndShifts:(uint64*int)[]) =
    let occupancyMasks = Constants.occupancyMaskBishop
    occupancyMasks  |>  
    (generateOccupancyVariations >> generateBishopMagicMoves occupancyMasks magicNumbersAndShifts) 

let bitboardToConventionalMoves (srcIndex:int) (moveSquares:Bitboard) =
    moveSquares 
    |> BitUtils.getSetBits_u64 
    |> Array.map (fun dst -> 
        //let isCapture = allPieces |> BitUtils.hasBitSet dst
        Move.create (srcIndex, dst))

let generateMovesForPosition (pc:SlidingPiece) (magicMoves:uint64[][]) (bbAllPieces:Bitboard) (bbFriendlyPieces:Bitboard) (srcIndex:int) (magicNumbersAndShifts:(uint64*int)[])=
    let occupancyMasks = getOccupancyMask pc
    let presentBlockers = bbAllPieces &&& occupancyMasks.[srcIndex]
    let magicNumber = magicNumbersAndShifts.[srcIndex] |> fst
    let magicShift = magicNumbersAndShifts.[srcIndex] |> snd
    let databaseIndexUint64 = multiplyAndShift presentBlockers magicNumber magicShift
    let databaseIndex = (int)databaseIndexUint64
    let bbMoveSquares = magicMoves.[srcIndex].[databaseIndex] &&& ~~~bbFriendlyPieces
    bbMoveSquares

let generateMovesForPositionViaLookups (pc:SlidingPiece) (bbAllPieces:Bitboard) (bbFriendlyPieces:Bitboard) (srcIndex:int) (lookups:MoveGenerationLookups) =
    match pc with 
    | Bishop ->  generateMovesForPosition Bishop lookups.BishopMovesDb bbAllPieces bbFriendlyPieces srcIndex lookups.MagicNumbersAndShifts.MagicNumbersAndShiftsBishop
    | Rook -> generateMovesForPosition Rook lookups.RookMovesDb bbAllPieces bbFriendlyPieces srcIndex lookups.MagicNumbersAndShifts.MagicNumbersAndShiftsRook

let generateAttackSets (pc:SlidingPiece) (occupancyVariations:uint64[][]) (occupancyMasks:uint64[]) =
    [|
        for bitRef = 0 to 63 do
            let occupancyMask = occupancyMasks.[bitRef]
            let variationCount = 1 <<< (BitUtils.countSetBits occupancyMask)

            yield [|
                    for i = 0 to variationCount-1 do
                        let matchesOccupancyVariation j =
                            occupancyVariations.[bitRef].[i] &&& (1UL <<< j) <> 0UL

                        let squaresAllDirs = generateSquaresInAllDirsWithMixMaxFunctionsForSlidingPieces pc bitRef
                        let maxAttackedOrEdgeInAllDirs = 
                            squaresAllDirs 
                            |> Seq.where (fst >> Seq.isEmpty >> not)
                            |> Seq.map (fun (squares, minOrMax) -> 
                                squares 
                                |> Seq.takeUntilInclusive matchesOccupancyVariation 
                                |> minOrMax)
                            
                        let attackSetCombined = 
                            maxAttackedOrEdgeInAllDirs 
                            |> Seq.fold (fun (attackSetInDir:uint64) j ->  (attackSetInDir |> BitUtils.setBit j)) 0UL 
                        yield attackSetCombined
                 |]
    |]
    

let generateMagicNumbersAndShifts (occupancyMasks:uint64[]) (occupancyVariations:uint64[][]) (occupancyAttackSets:uint64[][]) =
    [|
        for bitRef in 0..63 do 
            let bitCount = BitUtils.countSetBits occupancyMasks.[bitRef]
            let variationCount = 1 <<< bitCount;
            let magicShift = 64-bitCount
            let currentBitRefOccupancyVariations = occupancyVariations.[bitRef]
            let currentBitRefOccupancyMask = occupancyMasks.[bitRef]
            let currentBitRefAttackSet = occupancyAttackSets.[bitRef]
            let mutable candidateCount = 0

            let magicNumberDoesNotClashWithAnotherOccupancyVariationAttackSet (magicNumber:uint64) =
                candidateCount <- candidateCount + 1
                #if DIAG
                if(candidateCount % 100000 = 0) then
                    printfn "Total magic attempts: %d" candidateCount
                //magicAttemptsPerBitCount.[bitRef] <- candidateCount
                #endif

                let mutable usedBy = Array.zeroCreate<uint64> (1 <<< bitCount)
                let variations = [|0..variationCount-1|]

                let noClashes = 
                    variations |> Array.forall (fun i -> 
                        let attackSet = currentBitRefAttackSet.[i]
                        let index:int = multiplyAndShift currentBitRefOccupancyVariations.[i] magicNumber magicShift
                        let collision = usedBy.[index] <> 0UL && usedBy.[index] <> attackSet
                        usedBy.[index] <- attackSet
                        not collision )
                noClashes

            let goodMagicPredicate m =
                // The resulting index, derived from the magic, must be big enough to contain all the attacks for each possible subset of the occupancy mask (minus edges of the board)
                let bitCountInMostSignificant8 = BitUtils.countSetBits_32  (uint32(multiplyAndShift currentBitRefOccupancyMask m 56))
                // let g = ((multiplyAndShift currentBitRefOccupancyMask m 56))
                // if bitCountInMostSignificant8 < 6 then 
                //     printfn "Discarding magic, density was %d (%s) (Magic was %s)" 
                //         bitCountInMostSignificant8
                //         (System.Convert.ToString(g, 2)) 
                //         (System.Convert.ToString(int64(m), 2))
                bitCountInMostSignificant8 >= 6

            let magicNumber = 
                Randomness.infiniteSparseUInt64SequenceFor (bitRef / 8) 
                |> Seq.where goodMagicPredicate
                |> Seq.find magicNumberDoesNotClashWithAnotherOccupancyVariationAttackSet

            printfn "Found magic number for bitref %d: %x" bitRef magicNumber
            printfn "Magic attempts: %d" candidateCount
            yield (magicNumber, magicShift)
    |]

let bootstrapMagicNumberGeneration (pc:SlidingPiece) =
    let occupancyMask = getOccupancyMask pc
    let occupancyVariations = occupancyMask  |>  generateOccupancyVariations
    let attackSets = generateAttackSets pc occupancyVariations occupancyMask |> Array.ofSeq
    
    generateMagicNumbersAndShifts occupancyMask occupancyVariations attackSets

module MoveGenerationLookupFunctions =
    open Position

    let bootstrapAll (engineOptions:EngineOptions option) = 
        let maybeCache pc gen = 
            match (engineOptions, pc) with
            | (Some opts, Rook) -> MagicCache.resolveMagicNumbersWithPersistentCaching opts.RookMagicFilePath gen
            | (Some opts, Bishop) -> MagicCache.resolveMagicNumbersWithPersistentCaching opts.BishopMagicFilePath gen
            | (None, _) -> gen()

        let magicNumbersAndShiftsRook = maybeCache SlidingPiece.Rook ( fun () -> bootstrapMagicNumberGeneration SlidingPiece.Rook)
        let magicNumbersAndShiftsBishop = maybeCache SlidingPiece.Bishop (fun () -> bootstrapMagicNumberGeneration SlidingPiece.Bishop)
        let allMagic = {
            MagicValues.MagicNumbersAndShiftsRook = magicNumbersAndShiftsRook; 
            MagicValues.MagicNumbersAndShiftsBishop = magicNumbersAndShiftsBishop
        }

        { 
            MoveGenerationLookups.MagicNumbersAndShifts = allMagic;
            RookMovesDb = bootstrapRookMagicMoves  magicNumbersAndShiftsRook;
            BishopMovesDb = bootstrapBishopMagicMoves  magicNumbersAndShiftsBishop;
            KingMovesDb = generateSquaresKingMoves();
            KnightMovesDb = generateSquaresKnightMoves();
            WhitePawnMovesDb = generateSquaresAndCapturesForWhitePawn();
            BlackPawnMovesDb = generateSquaresAndCapturesForBlackPawn();
            WhitePawnEnpassantDb = generateEnPassantTargetsForWhite();
            BlackPawnEnpassantDb = generateEnPassantTargetsForBlack();
        }

    let private generatePawnPseudoMoves (pos:Position) (side:Side) (bitRef:int) (lookups:MoveGenerationLookups) =
        //let friendlyPieces = pos |> getBitboardForSide side
        let opposingPieces = pos |> getBitboardForSide (Common.opposite side)
        let allPieces = pos |> bothSidesBitboard
        let (moves,captures) = 
            match side with
            | White -> lookups.WhitePawnMovesDb.[bitRef]
            | Black -> lookups.BlackPawnMovesDb.[bitRef]
       
        let blockers =
            match side with
            | White -> if moves &&& Rows.FourthRow > 0UL && moves &&& allPieces &&& Rows.ThirdRow > 0UL then allPieces ||| Rows.FourthRow else allPieces
            | Black -> if moves &&& Rows.FifthRow > 0UL && moves &&& allPieces &&& Rows.SixthRow > 0UL then allPieces ||| Rows.FifthRow else allPieces

        let movesMinusBlockers = moves &&& ~~~blockers

        let capturesMinusFriendly = captures &&& opposingPieces
        movesMinusBlockers ||| capturesMinusFriendly

    let private generateKnightPseudoMoves (allPieces:Bitboard) (friendlyPieces:Bitboard) (bitRef:int) (lookups:MoveGenerationLookups) =
        lookups.KnightMovesDb.[bitRef] &&& ~~~friendlyPieces

    let private generateQueenPseudoMoves (allPieces:Bitboard) (friendlyPieces:Bitboard) (bitRef:int) (lookups:MoveGenerationLookups) =
        let bishopMoves = generateMovesForPositionViaLookups Bishop allPieces friendlyPieces bitRef lookups
        let rookMoves = generateMovesForPositionViaLookups Rook allPieces friendlyPieces bitRef lookups
        bishopMoves ||| rookMoves

    let private generateKingPseudoMoves (pos:Position) (allPieces:Bitboard) (friendlyPieces:Bitboard) (side:Side) (bitRef:int) (lookups:MoveGenerationLookups) =
        let castlingSpecial = 
            let castlingLookup = Position.castlingLookups.[side]
            let kingBitboard = Position.getKingBitboard side pos
            if (kingBitboard &&&  castlingLookup.InitialPositionKing) > 0UL then
                let rooksBitBoard = Position.getRooksBitboard side pos
                let kingSideCastling = 
                    (rooksBitBoard &&& castlingLookup.InitialPositionKingsRook) > 0UL && 
                    (allPieces &&& castlingLookup.BlockersKingsRook) = 0UL &&
                    pos |> Position.hasCastlingRights CastlingType.KingSide side
                let queensideCastling = 
                    (rooksBitBoard &&& castlingLookup.InitialPositionQueensRook) > 0UL && 
                    (allPieces &&& castlingLookup.BlockersQueensRook) = 0UL &&
                    pos |> Position.hasCastlingRights CastlingType.QueenSide side
                match(kingSideCastling, queensideCastling) with
                | (true, false) -> castlingLookup.DestinationKingSideCastling
                | (false, true) -> castlingLookup.DestinationQueenSideCastling
                | (true, true) -> castlingLookup.DestinationKingSideCastling ||| castlingLookup.DestinationQueenSideCastling
                | (false, false) -> 0UL
            else
                0UL

        let conventionalMoves = (lookups.KingMovesDb.[bitRef] &&& ~~~friendlyPieces)
        conventionalMoves ||| castlingSpecial

    let private generatePseudoMovesBitboard (lookups:MoveGenerationLookups) (pos:Position) (bitRef:int) =
        let (chessman, side) = pos |> getChessmanAndSide bitRef |> Option.get
        let friendlyPieces = pos |> getBitboardForSide side
        let allPieces = pos |> bothSidesBitboard

        let generator = 
            match chessman with
            | Pawn -> generatePawnPseudoMoves pos side 
            | Knight -> generateKnightPseudoMoves allPieces friendlyPieces
            | Chessmen.Bishop -> generateMovesForPositionViaLookups Bishop allPieces friendlyPieces
            | Chessmen.Rook -> generateMovesForPositionViaLookups Rook allPieces friendlyPieces
            | Queen -> generateQueenPseudoMoves allPieces friendlyPieces
            | King -> generateKingPseudoMoves pos allPieces friendlyPieces side 
        generator bitRef lookups

    let generatePseudoMoves (lookups:MoveGenerationLookups) (pos:Position) (bitRef:int) =
        generatePseudoMovesBitboard lookups pos bitRef 
        |> bitboardToConventionalMoves bitRef

    let private AllPromotionTypes = [|
            SpecialMoveType.Promotion(PromotionType.QueenProm); 
            SpecialMoveType.Promotion(PromotionType.RookProm); 
            SpecialMoveType.Promotion(PromotionType.BishopProm); 
            SpecialMoveType.Promotion(PromotionType.KnightProm); 
        |]

    let generatePseudoMovesWithSpecial (lookups:MoveGenerationLookups) (pos:Position) (bitRef:int) =
        let generateEnPassantMoves side enPassantTarget =
            let ((_, diagonalMoves), enPassantTargets) = 
                match side with 
                | White -> lookups.WhitePawnMovesDb.[bitRef], lookups.WhitePawnEnpassantDb.[enPassantTarget]
                | Black -> lookups.BlackPawnMovesDb.[bitRef], lookups.BlackPawnEnpassantDb.[enPassantTarget]

            diagonalMoves &&& enPassantTargets
            |> BitUtils.getSetBits_u64 
            |> Array.map (fun dst -> Move.createSpecial (bitRef, dst) SpecialMoveType.EnPassant)

        let convertLastRankPawnMovesToPromotionIfApplicable side moves =
            let isLastRankMovePredicate m =
                let dst = m |> Move.getDestBitRef 
                match side with
                | White -> dst >= 56 && dst <= 63
                | Black -> dst >= 0 && dst <= 7
            
            match moves |> Array.partition isLastRankMovePredicate with
            | ([||], nonPromotables) -> nonPromotables
            | (promotables, [||])    -> promotables |> Array.collect (fun m -> 
                    AllPromotionTypes |> Array.map (Move.createSpecialFromExisting m))
            | (_, _)                -> failwithf "Invalid case: generated both promotions and non-promotions moves for the same pawn: %d" bitRef

        let (chessman, side) = pos |> getChessmanAndSide bitRef |> Option.get
        let moves = generatePseudoMoves lookups pos bitRef

        match chessman with
        | Pawn -> 
            let movesWithMaybeEnPassant =
                if pos.EnPassantTarget > 0 then
                    let enpassants = generateEnPassantMoves side pos.EnPassantTarget
                    Array.append moves enpassants
                else moves
            movesWithMaybeEnPassant |> convertLastRankPawnMovesToPromotionIfApplicable side
        | _ -> moves

    let generateAttacks (lookups:MoveGenerationLookups) (side:Side) (pos:Position) =
        let bbForSide = getBitboardForSide side pos
        let srcBitRefs = bbForSide |> BitUtils.getSetBits_u64
        srcBitRefs 
        |> Array.map (generatePseudoMovesBitboard lookups pos)
        |> Array.reduce (|||)
    

    let generateAllPseudoMovesForSide (lookups:MoveGenerationLookups) (side:Side) (pos:Position) =
        let bbForSide = getBitboardForSide side pos
        let srcBitRefs = bbForSide |> BitUtils.getSetBits_u64
        srcBitRefs 
        |> Array.collect (generatePseudoMovesWithSpecial lookups pos)
        // |> Array.map (generatePseudoMoves lookups pos)
        // |> Array.concat
        //|> Array.reduce (|||)

    // let generatePseudoMovesFullInfo (lookups:MoveGenerationLookups) (pos:Position) (bitRef:int) =
    //     let side = pos.SideToPlay
    //     let opponentPieces = pos |> getBitboardForSide (opposite side)
    //     let moves = generatePseudoMoves lookups pos bitRef
    //     moves
    //     // bitboardResult 
    //     // |> BitUtils.getSetBits
    //     // |> Array.map (fun dstBitRef ->
    //     //     let isCapture = bitboardResult |> BitUtils.hasBitSet dstBitRef
    //     //     Move.create (bitRef, dstBitRef) isCapture)

