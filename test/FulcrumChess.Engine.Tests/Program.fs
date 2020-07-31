namespace FulcrumChess.Engine.Tests
open Xunit
open Xunit.Extensions.AssemblyFixture

// TODO: Change type of project to .NET Standard 
module Program =
    [<assembly: TestFramework(AssemblyFixtureFramework.TypeName, AssemblyFixtureFramework.AssemblyName)>]
    do()
    
    // Dummy EntryPoint to make compiler happy and not emit [FS0988] Main module of program is empty: nothing will happen when it is run” in Tests Project?
    [<EntryPoint>] 
    let main argv =
        0