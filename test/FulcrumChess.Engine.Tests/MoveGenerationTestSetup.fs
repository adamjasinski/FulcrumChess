namespace FulcrumChess.Engine.Tests.MoveGeneration
open FulcrumChess.Engine

type MagicGenerationSetupFixture() = 
    let currentLookups = Bitboards.MoveGenerationLookupFunctions.bootstrapAll None

    member __.Lookups = currentLookups