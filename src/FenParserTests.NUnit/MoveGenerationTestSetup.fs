namespace FenParserTests.NUnit.MoveGeneration

open NUnit.Framework

[<SetUpFixture>]
type MagicGenerationSetupFixture() = 
    let mutable currentMagic:Magic.MagicValues option = None

    //static member getCurrentMagic() = currentMagic

    [<OneTimeSetUp>]
    member this.SetupMagic() = 
        let magicNumbersAndShiftsRook = Bitboards.bootstrapMagicNumberGeneration Pieces.SlidingPiece.Rook
        let magicNumbersAndShiftsBishop = Bitboards.bootstrapMagicNumberGeneration Pieces.SlidingPiece.Bishop
        currentMagic <- Some {
            Magic.MagicValues.MagicNumbersAndShiftsRook=magicNumbersAndShiftsRook; 
            Magic.MagicValues.MagicNumbersAndShiftsBishop=magicNumbersAndShiftsBishop} 


