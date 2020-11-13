namespace FulcrumChess.Engine.Tests.MoveGeneration
open Xunit
open Xunit.Extensions.AssemblyFixture
open Swensen.Unquote
open FulcrumChess.Engine
open FulcrumChess.Engine.Tests

type PerftTests(magicGenerationSetupFixture:MagicGenerationSetupFixture) =

    let lookups = magicGenerationSetupFixture.Lookups

    interface IAssemblyFixture<MagicGenerationSetupFixture>

    // Known expected values taken from http://www.rocechess.ch/perft.html
    // Also see https://github.com/official-stockfish/Stockfish/blob/master/tests/perft.sh
    [<Theory>]
    [<Category("Perft")>]
    [<InlineDataEx(1, 20UL)>]
    [<InlineDataEx(2, 400UL)>]
    member __.``shallow perft in initial position should return known values`` (depth:int, expectedNodes:uint64) =
        let perftReport = Perft.generatePerftReport lookups (0us, Positions.initialPosition) (1, depth)
        test <@ perftReport.TotalNodes = expectedNodes @>

    [<Theory(Skip="Pending...")>]
    [<Category("Perft")>]
    [<InlineDataEx(3, 8902)>] //pending
    member __.``slightly deeper perft in initial position should return known values`` (depth:int, expectedNodes:uint64) =
        let perftReport = Perft.generatePerftReport lookups (0us, Positions.initialPosition) (1, depth)
        test <@ perftReport.TotalNodes = expectedNodes @>


    [<Theory(Skip="Pending...")>]
    [<Category("Perft")>]
    [<InlineDataEx("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -", 5, 193690690)>] //pending
    member __.``slightly deeper perft in specific positions should return known values`` (fen:string, depth:int, expectedNodes:uint64) =
        let pos = FenParsing.parseToPosition fen
        let perftReport = Perft.generatePerftReport lookups (0us, pos) (1, depth)
        test <@ perftReport.TotalNodes = expectedNodes @>