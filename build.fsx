#r "src/packages/FAKE/tools/FakeLib.dll" // include Fake lib
open Fake 
open Fake.Testing

// Directories
let buildDir  = "./build/bin/"
let testDir   = "./build/test/"
let deployDir = "./deploy/"

// Filesets
let appReferences  = 
    !! "src/FenParser/FenParser.fsproj"

let testReferences =
    !! "src/FenParserTests.NUnit/*.fsproj"

// Targets
Target "Clean" (fun _ -> 
    CleanDirs [buildDir; testDir; deployDir]
)

Target "BuildApp" (fun _ -> 
    MSBuildDebug buildDir "Build" appReferences
    |> Log "AppBuild-Output: "
)

Target "BuildTest" (fun _ -> 
    MSBuildDebug testDir "Build" testReferences
    |> Log "TestBuild-Output: "
)

Target "xUnitTest" (fun _ ->  
    !! (testDir + "/*Tests.dll")
        |> xUnit2 (fun p -> 
            {p with 
                ShadowCopy = false;
                ToolPath = "./src/packages/xunit.runner.console.2.2.0/tools/xunit.console.exe" })
)

// Target "xUnitTestDebug" (fun _ ->  
//     Shell.Exec("mono", "./src/packages/xunit.runner.console.2.2.0/tools/xunit.console.exe FenParserTests.dll", "testDir") |> ignore
// )

Target "NUnitTest" (fun _ ->  
    !! (testDir + "/*Tests.NUnit.dll")
        |> NUnit3 (fun p -> 
            {p with 
                ShadowCopy = false;
                ToolPath = "./src/packages/NUnit.ConsoleRunner.3.7.0/tools/nunit3-console.exe" })
)

Target "Deploy" (fun _ ->
    trace "Heavy deploy action"
)

"Clean"
   ==> "BuildApp"
   ==> "BuildTest"
   =?> ("NUnitTest",hasBuildParam "NUnitTest")  // only if FAKE was called with parameter NUnitTest
   ==> "Deploy"

//Run "Deploy"
RunParameterTargetOrDefault "target" "Deploy"

