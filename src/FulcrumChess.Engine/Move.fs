namespace FulcrumChess.Engine

type Move = uint16

module Move =

    // Follows Stockfish convention
    /// bit  0- 5: destination square (from 0 to 63)
    /// bit  6-11: origin square (from 0 to 63)
    /// bit 12-13: promotion piece type - 2 (from KNIGHT-2 to QUEEN-2)
    /// bit 14-15: special move flag: promotion (1), en passant (2), castling (3)
    /// NOTE: EN-PASSANT bit is set only when a pawn can be captured

    let create (srcBitRef:int, destBitRef:int) (isCapture:bool) : Move =
        uint16 ((srcBitRef <<< 6) ||| destBitRef)

    let getDestBitRef (move:Move) =
        int(move &&& 0x3Fus)

    let getSrcBitRef (move:Move) =
        int((move &&& 0xFC0us) >>> 6)

    let getSrcAndDestBitRefs = 
        Arrow.onSingle getSrcBitRef getDestBitRef 


    