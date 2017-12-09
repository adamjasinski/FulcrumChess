namespace FulcrumChess.Engine.Tests.MoveGeneration

open NUnit.Framework
open FulcrumChess.Engine

[<SetUpFixture>]
type MagicGenerationSetupFixture() = 
    static let mutable currentLookups:MoveGenerationLookups option = None

    static member getCurrentMagic() = 
        match currentLookups with
        | Some m -> m.MagicNumbersAndShifts
        | None -> invalidOp "Drat! Magic was supposed to have been initialized already by NUnit!" 

    static member getCurrentLookups() =
        match currentLookups with
        | Some m -> m
        | None -> invalidOp "Drat! Lookups were supposed to have been initialized already by NUnit!" 

    [<OneTimeSetUp>]
    member this.SetupMagic() = 
        currentLookups <- Bitboards.MoveGenerationLookupFunctions.bootstrapAll() |> Some


