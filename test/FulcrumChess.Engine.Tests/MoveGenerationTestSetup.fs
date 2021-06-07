namespace FulcrumChess.Engine.Tests.MoveGeneration
open FulcrumChess.Engine

type MagicGenerationSetupFixture() = 
    let testProjectPathRelativeToVsTest = "../../../data"
    let options = 
        { EngineOptions.RookMagicFilePath = $"{testProjectPathRelativeToVsTest}/RookMagicNumbers.json"
          BishopMagicFilePath = $"{testProjectPathRelativeToVsTest}/BishopMagicNumbers.json" }
    let currentLookups = Bitboards.MoveGenerationLookupFunctions.bootstrapAll <| Some options

    member __.Lookups = currentLookups