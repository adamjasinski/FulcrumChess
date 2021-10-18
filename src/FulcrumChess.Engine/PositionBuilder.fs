namespace FulcrumChess.Engine


// type PositionBuilder(pos:Position) =
//     let mutable whiteKing = pos.WhiteKing
//     let mutable whiteQueen = pos.WhiteQueen
//     let mutable whiteRooks = pos.WhiteRooks
//     let mutable whiteBishops = pos.WhiteBishops
//     let mutable whiteKnights = pos.WhiteKnights
//     let mutable whitePawns = pos.WhitePawns
//     let mutable blackKing = pos.BlackKing
//     let mutable blackQueen = pos.BlackQueen
//     let mutable blackRooks = pos.BlackRooksc
//     let mutable blackBishops = pos.BlackBishops
//     let mutable blackKnights = pos.BlackKnights
//     let mutable blackPawns = pos.BlackPawns
//     let mutable whiteCastlingRigths = pos.WhiteCastlingRights
//     let mutable blackCastlingRigths = pos.BlackCastlingRights
//     let mutable sideToPlay = pos.SideToPlay
//     let mutable enPassantTarget = pos.EnPassantTarget
//     let mutable halfMoveClock = pos.HalfMoveClock
//     let mutable fullMoveNumber = pos.FullMoveNumber
//     let mutable hashKey = pos.HashKey

//     member __.ToPosition() = {
//         WhiteKing = whiteKing
//         WhiteQueen = whiteQueen
//         WhiteRooks = whiteRooks
//         WhiteBishops = whiteBishops
//         WhiteKnights = whiteKnights
//         WhitePawns = whitePawns
//         BlackKing = blackKing
//         BlackQueen = blackQueen
//         BlackRooks = blackRooks
//         BlackBishops = blackBishops
//         BlackKnights = blackKnights 
//         BlackPawns = blackPawns
//         WhiteCastlingRights = whiteCastlingRigths
//         BlackCastlingRights = blackCastlingRigths
//         SideToPlay = sideToPlay
//         EnPassantTarget = enPassantTarget
//         HalfMoveClock = halfMoveClock
//         FullMoveNumber = fullMoveNumber
//         HashKey = hashKey }

//     // let updateWithTargetChessmanHash pcAndSide = 
//     //     //printfn "XORing with pc hash %A %d" (pc, pos.SideToPlay) bitRef
//     //     hashKey <- hashKey ^^^ Zobrist.getChessmanHash pcAndSide bitRef

//     // member this.BlackKing
//     //     with get() = blackKing
//     //     with set(value) = 
//     //         blackKing <- value
//     //         update

//     member __.ClearPiece struct(piece:Chessmen, side:Side) (bitRef:int) =
//         if bitRef < 0 || bitRef > 63 then invalidArg "bitRef" ("parameter has invalid value: " + bitRef.ToString())

//         // let updateWithTargetChessmanHash pcAndSide = 
//         //     //printfn "XORing with pc hash %A %d" (pc, pos.SideToPlay) bitRef
//         //     hashKey <- hashKey ^^^ Zobrist.getChessmanHash pcAndSide bitRef
//         // //let updatedHash = pos.HashKey ^^^ Zobrist.getEmptyFieldHash bitRef
//         // //printfn "XORing with pc hash %A %d" (piece, side) bitRef
//         //let updatedHash = hashKey ^^^ Zobrist.getChessmanHash struct(piece, side) bitRef

//         match piece,side with
//         | (Chessmen.Pawn, Side.Black) -> clearBitRef &blackPawns bitRef
//         | (Chessmen.Knight, Side.Black) -> clearBitRef &blackKnights bitRef
//         | (Chessmen.Bishop, Side.Black) -> clearBitRef &blackBishops bitRef
//         | (Chessmen.Rook, Side.Black) -> clearBitRef &blackRooks bitRef
//         | (Chessmen.Queen, Side.Black) -> clearBitRef &blackQueen bitRef
//         | (Chessmen.King, Side.Black) -> clearBitRef &blackKing bitRef
//         | (Chessmen.Pawn, Side.White) -> clearBitRef &whitePawns bitRef
//         | (Chessmen.Knight, Side.White) -> clearBitRef &whiteKnights bitRef
//         | (Chessmen.Bishop, Side.White) -> clearBitRef &whiteBishops bitRef
//         | (Chessmen.Rook, Side.White) -> clearBitRef &whiteRooks bitRef
//         | (Chessmen.Queen, Side.White) -> clearBitRef &whiteQueen bitRef
//         | (Chessmen.King, Side.White) -> clearBitRef &whiteKing bitRef

//         hashKey <- hashKey ^^^ Zobrist.getChessmanHash struct(piece, side) bitRef


//     member this.SetPiece struct(piece:Chessmen, side:Side) (bitRef:int) =
//         let fenLetter = struct(piece, side) |> PieceFenLetters.getLetter
//         this.SetFenPiece fenLetter bitRef

//     member __.SetFenPiece (piece:char) (bitRef:int) =
//         if bitRef < 0 || bitRef > 63 then invalidArg "bitRef" ("parameter has invalid value: " + bitRef.ToString())
//         let candidate:Bitboard = 0UL |> BitUtils.setBit bitRef

//         let updateWithTargetChessmanHash pcAndSide = 
//             //printfn "XORing with pc hash %A %d" (pc, pos.SideToPlay) bitRef
//             hashKey <- hashKey ^^^ Zobrist.getChessmanHash pcAndSide bitRef
       
//         //let a= struct(Chessmen.Pawn, Side.Black)
//         match piece with
//         | 'p' -> xorv &blackPawns candidate; updateWithTargetChessmanHash struct(Chessmen.Pawn, Side.Black)
//         | 'n' -> xorv &blackKnights candidate; updateWithTargetChessmanHash struct(Chessmen.Knight, Side.Black)
//         | 'b' -> xorv &blackBishops candidate; updateWithTargetChessmanHash struct(Chessmen.Bishop, Side.Black)
//         | 'r' -> xorv &blackRooks candidate; updateWithTargetChessmanHash struct(Chessmen.Rook, Side.Black)
//         | 'q' -> xorv &blackQueen candidate; updateWithTargetChessmanHash struct(Chessmen.Queen, Side.Black)
//         | 'k' -> xorv &blackKing candidate; updateWithTargetChessmanHash struct(Chessmen.King, Side.Black)
//         | 'P' -> xorv &whitePawns candidate; updateWithTargetChessmanHash struct(Chessmen.Pawn, Side.White)
//         | 'N' -> xorv &whiteKnights candidate; updateWithTargetChessmanHash struct(Chessmen.Knight, Side.White)
//         | 'B' -> xorv &whiteBishops candidate; updateWithTargetChessmanHash struct(Chessmen.Bishop, Side.White)
//         | 'R' -> xorv &whiteRooks candidate; updateWithTargetChessmanHash struct(Chessmen.Rook, Side.White)
//         | 'Q' -> xorv &whiteQueen candidate; updateWithTargetChessmanHash struct(Chessmen.Queen, Side.White)
//         | 'K' -> xorv &whiteKing candidate; updateWithTargetChessmanHash struct(Chessmen.King, Side.White)
//         | _ -> invalidArg "piece" ("parameter has invalid value: " + piece.ToString())

//     member __.SetCastlingRights (castlingRights:CastlingRights) (side:Side) =
//         let newCastlingRightsPair = 
//             match side with
//             | White -> (castlingRights, blackCastlingRights)
//             | Black -> (whiteCastlingRights, castlingRights)
//         let updatedHashKey = 
//                 hashKey ^^^
//                 ((whiteCastlingRights, blackCastlingRights) |> Zobrist.getCastlingRightsHash) ^^^ //clear previous
//                 (newCastlingRightsPair |> Zobrist.getCastlingRightsHash)   //set current

//         match side with
//             | White -> WhiteCastlingRights <- castlingRights; HashKey <- updatedHashKey
//             | Black -> BlackCastlingRights <- castlingRights; HashKey <- updatedHashKey

//     member __.UpdatePostMoveValidationAndFlipSide(enPassantTarget, nextMoveNumber, nextHalfMoveClock) =

//             //let previousEnPassantHash = pos.EnPassantTarget |> Zobrist.getEnPassantHash
//             let enPassantHashUpd = 
//                 (EnPassantTarget |> Zobrist.getEnPassantHash) ^^^  //clear previous en passant target hash
//                 (enPassantTarget |> Option.map Zobrist.getEnPassantHash |> Option.defaultValue 0UL) //set current

//             let updatedHash = 
//                 HashKey ^^^ 
//                 Zobrist.sideHash ^^^  //flipping the side
//                 enPassantHashUpd

//             SideToPlay <- opposite SideToPlay
//             EnPassantTarget <- enPassantTarget |> Option.defaultValue 0
//             FullMoveNumber <- nextMoveNumber 
//             HalfMoveClock <- nextHalfMoveClock
//             HashKey = updatedHash



// type PositionMutable = {
//     mutable WhiteKing:Bitboard;
//     mutable WhiteQueen:Bitboard;
//     mutable WhiteRooks:Bitboard;
//     mutable WhiteBishops:Bitboard;
//     mutable WhiteKnights:Bitboard;
//     mutable WhitePawns:Bitboard;
//     mutable BlackKing:Bitboard;
//     mutable BlackQueen:Bitboard;
//     mutable BlackRooks:Bitboard;
//     mutable BlackBishops:Bitboard;
//     mutable BlackKnights:Bitboard;
//     mutable BlackPawns:Bitboard;

//     mutable WhiteCastlingRights:CastlingRights;
//     mutable BlackCastlingRights:CastlingRights;
//     mutable SideToPlay:Side;
//     mutable EnPassantTarget:int;
//     mutable HalfMoveClock:int;
//     mutable FullMoveNumber:int;
//     mutable HashKey:uint64; } with

//     member this.FlipSideToPlay() = this.SideToPlay <- opposite this.SideToPlay

//     member this.ClearPiece struct(piece:Chessmen, side:Side) (bitRef:int) =
//         if bitRef < 0 || bitRef > 63 then invalidArg "bitRef" ("parameter has invalid value: " + bitRef.ToString())

//         // let updateWithTargetChessmanHash pcAndSide = 
//         //     //printfn "XORing with pc hash %A %d" (pc, pos.SideToPlay) bitRef
//         //     hashKey <- hashKey ^^^ Zobrist.getChessmanHash pcAndSide bitRef
//         // //let updatedHash = pos.HashKey ^^^ Zobrist.getEmptyFieldHash bitRef
//         // //printfn "XORing with pc hash %A %d" (piece, side) bitRef
//         //let updatedHash = hashKey ^^^ Zobrist.getChessmanHash struct(piece, side) bitRef

//         match piece,side with
//         | (Chessmen.Pawn, Side.Black) -> clearBitRef &this.BlackPawns bitRef
//         | (Chessmen.Knight, Side.Black) -> clearBitRef &this.BlackKnights bitRef
//         | (Chessmen.Bishop, Side.Black) -> clearBitRef &this.BlackBishops bitRef
//         | (Chessmen.Rook, Side.Black) -> clearBitRef &this.BlackRooks bitRef
//         | (Chessmen.Queen, Side.Black) -> clearBitRef &this.BlackQueen bitRef
//         | (Chessmen.King, Side.Black) -> clearBitRef &this.BlackKing bitRef
//         | (Chessmen.Pawn, Side.White) -> clearBitRef &this.WhitePawns bitRef
//         | (Chessmen.Knight, Side.White) -> clearBitRef &this.WhiteKnights bitRef
//         | (Chessmen.Bishop, Side.White) -> clearBitRef &this.WhiteBishops bitRef
//         | (Chessmen.Rook, Side.White) -> clearBitRef &this.WhiteRooks bitRef
//         | (Chessmen.Queen, Side.White) -> clearBitRef &this.WhiteQueen bitRef
//         | (Chessmen.King, Side.White) -> clearBitRef &this.WhiteKing bitRef

//         this.HashKey <- this.HashKey ^^^ Zobrist.getChessmanHash struct(piece, side) bitRef


//     member this.SetPiece struct(piece:Chessmen, side:Side) (bitRef:int) =
//         let fenLetter = struct(piece, side) |> PieceFenLetters.getLetter
//         this.SetFenPiece fenLetter bitRef

//     member this.SetFenPiece (piece:char) (bitRef:int) =
//         if bitRef < 0 || bitRef > 63 then invalidArg "bitRef" ("parameter has invalid value: " + bitRef.ToString())
//         let candidate:Bitboard = 0UL |> BitUtils.setBit bitRef

//         let updateWithTargetChessmanHash pcAndSide = 
//             //printfn "XORing with pc hash %A %d" (pc, pos.SideToPlay) bitRef
//             this.HashKey <- this.HashKey ^^^ Zobrist.getChessmanHash pcAndSide bitRef
       
//         //let a= struct(Chessmen.Pawn, Side.Black)
//         match piece with
//         | 'p' -> xorv &this.BlackPawns candidate; updateWithTargetChessmanHash struct(Chessmen.Pawn, Side.Black)
//         | 'n' -> xorv &this.BlackKnights candidate; updateWithTargetChessmanHash struct(Chessmen.Knight, Side.Black)
//         | 'b' -> xorv &this.BlackBishops candidate; updateWithTargetChessmanHash struct(Chessmen.Bishop, Side.Black)
//         | 'r' -> xorv &this.BlackRooks candidate; updateWithTargetChessmanHash struct(Chessmen.Rook, Side.Black)
//         | 'q' -> xorv &this.BlackQueen candidate; updateWithTargetChessmanHash struct(Chessmen.Queen, Side.Black)
//         | 'k' -> xorv &this.BlackKing candidate; updateWithTargetChessmanHash struct(Chessmen.King, Side.Black)
//         | 'P' -> xorv &this.WhitePawns candidate; updateWithTargetChessmanHash struct(Chessmen.Pawn, Side.White)
//         | 'N' -> xorv &this.WhiteKnights candidate; updateWithTargetChessmanHash struct(Chessmen.Knight, Side.White)
//         | 'B' -> xorv &this.WhiteBishops candidate; updateWithTargetChessmanHash struct(Chessmen.Bishop, Side.White)
//         | 'R' -> xorv &this.WhiteRooks candidate; updateWithTargetChessmanHash struct(Chessmen.Rook, Side.White)
//         | 'Q' -> xorv &this.WhiteQueen candidate; updateWithTargetChessmanHash struct(Chessmen.Queen, Side.White)
//         | 'K' -> xorv &this.WhiteKing candidate; updateWithTargetChessmanHash struct(Chessmen.King, Side.White)
//         | _ -> invalidArg "piece" ("parameter has invalid value: " + piece.ToString())

//     member this.SetCastlingRights (castlingRights:CastlingRights) (side:Side) =
//         let newCastlingRightsPair = 
//             match side with
//             | White -> (castlingRights, this.BlackCastlingRights)
//             | Black -> (this.WhiteCastlingRights, castlingRights)
//         let updatedHashKey = 
//                 this.HashKey ^^^
//                 ((this.WhiteCastlingRights, this.BlackCastlingRights) |> Zobrist.getCastlingRightsHash) ^^^ //clear previous
//                 (newCastlingRightsPair |> Zobrist.getCastlingRightsHash)   //set current

//         match side with
//             | White -> this.WhiteCastlingRights <- castlingRights; this.HashKey <- updatedHashKey
//             | Black -> this.BlackCastlingRights <- castlingRights; this.HashKey <- updatedHashKey

//     member this.UpdatePostMoveValidationAndFlipSide(enPassantTarget, nextMoveNumber, nextHalfMoveClock) =

//             //let previousEnPassantHash = pos.EnPassantTarget |> Zobrist.getEnPassantHash
//             let enPassantHashUpd = 
//                 (this.EnPassantTarget |> Zobrist.getEnPassantHash) ^^^  //clear previous en passant target hash
//                 (enPassantTarget |> Option.map Zobrist.getEnPassantHash |> Option.defaultValue 0UL) //set current

//             let updatedHash = 
//                 this.HashKey ^^^ 
//                 Zobrist.sideHash ^^^  //flipping the side
//                 enPassantHashUpd

//             this.SideToPlay <- opposite this.SideToPlay
//             this.EnPassantTarget <- enPassantTarget |> Option.defaultValue 0
//             this.FullMoveNumber <- nextMoveNumber 
//             this.HalfMoveClock <- nextHalfMoveClock
//             this.HashKey = updatedHash




    
