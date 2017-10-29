namespace FenParserTests.NUnit.MoveGeneration

open NUnit.Framework

[<SetUpFixture>]
type MagicGenerationSetupFixture() = 
    static let mutable currentMagic:Magic.MagicValues option = None

    static member getCurrentMagic() = 
        match currentMagic with
        | Some m -> m
        | None -> invalidOp "Drat! Magic was supposed to have been initialized already by NUnit!" 


    [<OneTimeSetUp>]
    member this.SetupMagic() = 
        let magicNumbersAndShiftsRook = Bitboards.bootstrapMagicNumberGeneration Pieces.SlidingPiece.Rook
        let magicNumbersAndShiftsBishop = Bitboards.bootstrapMagicNumberGeneration Pieces.SlidingPiece.Bishop
        currentMagic <- Some {
            Magic.MagicValues.MagicNumbersAndShiftsRook=magicNumbersAndShiftsRook; 
            Magic.MagicValues.MagicNumbersAndShiftsBishop=magicNumbersAndShiftsBishop} 


