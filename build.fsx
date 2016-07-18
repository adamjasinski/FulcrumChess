#r "packages/FAKE/tools/FakeLib.dll" // include Fake lib
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
    !! "src/FenParserTests/*.fsproj"

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
                ToolPath = "./packages/xunit.runner.console.2.1.0/tools/xunit.console.exe" })
)

// Target "xUnitTestDebug" (fun _ ->  
//     Shell.Exec("mono", "./packages/xunit.runner.console.2.1.0/tools/xunit.console.exe FenParserTests.dll", "testDir") |> ignore
// )

Target "Deploy" (fun _ ->
    trace "Heavy deploy action"
)

"Clean"
   ==> "BuildApp"
   ==> "BuildTest"
   =?> ("xUnitTest",hasBuildParam "xUnitTest")  // only if FAKE was called with parameter xUnitTest
   ==> "Deploy"

//Run "Deploy"
RunParameterTargetOrDefault "target" "Deploy"

