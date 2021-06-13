namespace FulcrumChess.Engine.Tests
open FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open FulcrumChess.Engine.Tests.PositionTestHelper

type PositionPromotionTests(magicGenerationSetupFixture:MagicGenerationSetupFixture) =

    let lookups = magicGenerationSetupFixture.Lookups

    interface IAssemblyFixture<MagicGenerationSetupFixture>

    [<Theory>]
    [<Category("Promotion2")>]
    [<InlineDataEx("4k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", "c7c8q", "2Q1k3/8/8/8/6p1/8/8/4K3 b - - 0 1")>] //standard promotion
    [<InlineDataEx("4k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", "c7c8r", "2R1k3/8/8/8/6p1/8/8/4K3 b - - 0 1")>] //underpromotion
    [<InlineDataEx("4k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", "c7c8b", "2B1k3/8/8/8/6p1/8/8/4K3 b - - 0 1")>] //underpromotion
    [<InlineDataEx("4k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", "c7c8n", "2N1k3/8/8/8/6p1/8/8/4K3 b - - 0 1")>] //underpromotion
    //[<InlineDataEx("1n2k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", "c7b8q", "1Q2k3/8/8/8/6p1/8/8/4K3 b - - 0 1")>] //promotion and capture
    member __. ``make move - promotion`` (fen:string, pawnMoveAlgNotation:string, expectedFen:string) =
        verifyPositionAfterMoveWithFullValidation lookups fen pawnMoveAlgNotation expectedFen