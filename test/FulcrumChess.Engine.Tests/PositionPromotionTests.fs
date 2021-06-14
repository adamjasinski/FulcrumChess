namespace FulcrumChess.Engine.Tests
open FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open FulcrumChess.Engine.Tests.PositionTestHelper

type PositionPromotionTests(magicGenerationSetupFixture:MagicGenerationSetupFixture) =

    let lookups = magicGenerationSetupFixture.Lookups

    interface IAssemblyFixture<MagicGenerationSetupFixture>

    [<Theory>]
    [<Category("Promotion")>]
    [<InlineDataEx("4k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", "c7c8q", "2Q1k3/8/8/8/6p1/8/8/4K3 b - - 0 1")>] //standard promotion
    [<InlineDataEx("4k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", "c7c8r", "2R1k3/8/8/8/6p1/8/8/4K3 b - - 0 1")>] //underpromotion
    [<InlineDataEx("4k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", "c7c8b", "2B1k3/8/8/8/6p1/8/8/4K3 b - - 0 1")>] //underpromotion
    [<InlineDataEx("4k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", "c7c8n", "2N1k3/8/8/8/6p1/8/8/4K3 b - - 0 1")>] //underpromotion
    [<InlineDataEx("1n2k3/2P5/8/8/6p1/8/8/4K3 w - - 0 1", "c7b8q", "1Q2k3/8/8/8/6p1/8/8/4K3 b - - 0 1")>] //promotion and capture
    [<InlineDataEx("4k3/8/8/6P1/8/8/4p3/7K b - - 0 1", "e2e1q", "4k3/8/8/6P1/8/8/8/4q2K w - - 0 2")>] //standard promotion
    [<InlineDataEx("4k3/8/8/6P1/8/8/4p3/7K b - - 0 1", "e2e1r", "4k3/8/8/6P1/8/8/8/4r2K w - - 0 2")>] //underpromotion
    [<InlineDataEx("4k3/8/8/6P1/8/8/4p3/7K b - - 0 1", "e2e1b", "4k3/8/8/6P1/8/8/8/4b2K w - - 0 2")>] //underpromotion
    [<InlineDataEx("4k3/8/8/6P1/8/8/4p3/7K b - - 0 1", "e2e1n", "4k3/8/8/6P1/8/8/8/4n2K w - - 0 2")>] //underpromotion
    [<InlineDataEx("4k3/8/8/6P1/8/8/4p3/5R1K b - - 0 1", "e2f1q", "4k3/8/8/6P1/8/8/8/5q1K w - - 0 2")>] //promotion and capture
    member __. ``make move - promotion`` (fen:string, pawnMoveAlgNotation:string, expectedFen:string) =
        verifyPositionAfterMoveWithFullValidation lookups fen pawnMoveAlgNotation expectedFen