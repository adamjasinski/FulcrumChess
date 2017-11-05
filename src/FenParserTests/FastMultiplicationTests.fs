namespace FenParserTests

module FastMultiplicationTests =

    open Xunit
    //open TestHelpers
    open FenParser
    open Swensen.Unquote

    module Multiplier =
        let inline multiplyAndShiftWith64BitChecked (occupancyVariation:uint64) (magicNumber:uint64) bitCount =
            let prod =  Checked.(*) occupancyVariation magicNumber
            int(prod >>> (64-bitCount))

        let inline multiplyAndShiftWith64Bit (occupancyVariation:uint64) (magicNumber:uint64) bitCount =
            int((occupancyVariation * magicNumber) >>> (64-bitCount))
        
        let inline multiplyAndShiftWith32Bit (occupancyVariation:uint64) (magicNumber:uint64) bitCount =
            //TODO - in fact, this is not equivalent to 64-bit multiplication
            let bitsToShiftFrom32 = 32-bitCount
            let unsignedResultRaw = (uint32(occupancyVariation)*uint32(magicNumber)) ^^^ (uint32(occupancyVariation>>>32)*uint32(magicNumber>>>32))
            int(unsignedResultRaw >>> bitsToShiftFrom32)

        let inline multiplyAndShiftWith32BitAlt (occupancyVariation:uint64) (magicNumber:uint64) bitCount =
            //https://stackoverflow.com/questions/19601852/assembly-64-bit-multiplication-with-32-bit-registers
            // x_l*y_l + (x_h*y_l + x_l*y_h)*2^32
            let x_l = uint32(occupancyVariation)
            let y_l = uint32(magicNumber)
            let x_h = uint32(occupancyVariation>>>32)
            let y_h = uint32(magicNumber>>>32)
            let bitsToShiftFrom32 = 32-bitCount
            let unsignedResultRaw = x_l*y_l + (x_h*y_l + x_l*y_h)*(1u <<< 32)
            int(unsignedResultRaw >>> bitsToShiftFrom32)

    //[<TestCase>]
    //let ``64-bit multiplication and 32-bit multiplication should yield identical results`` () =
        //let occupancyVariations = Bitboards.generateOccupancyVariations Bitboards.Constants.occupancyMaskRook
        //let magic = Magic.PregeneratedMagic.magicNumberRook
        //let shiftsFrom64 = Magic.PregeneratedMagic.magicNumberShiftsRook |> Array.map  (fun x -> 64 - x)
        //let shiftsFrom32 = Magic.PregeneratedMagic.magicNumberShiftsRook |> Array.map  (fun x -> 64 - x)
        ////let inputs = Array.zip3 occupancyVariations magic shifts

        //let lastVariationInEachBitref = occupancyVariations |> Array.map Array.last
        //let results64 = (lastVariationInEachBitref, magic, shiftsFrom64) |||> Array.map3 Multiplier.multiplyAndShiftWith64Bit
        //let results32 = (lastVariationInEachBitref, magic, shiftsFrom32) |||> Array.map3 Multiplier.multiplyAndShiftWith32BitAlt
        //test <@ results64.Length > 0 @>
        //test <@ results32.Length > 0 @>
        //test <@ results64 = results32 @>
        ()

