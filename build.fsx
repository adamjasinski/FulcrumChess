#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

Target.initEnvironment ()
//Pre-defined environment
let publishRuntime = "linux-x64"

Target.create "Clean" (fun _ ->
    Trace.trace " --- Cleaning the artifacts --- "
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "test/**/bin"
    ++ "test/**/obj"
    |> Shell.cleanDirs 
)

Target.create "Build" (fun _ ->
    Trace.trace " --- Building the app --- "
    !! "src/**/*.*proj"
    |> Seq.iter (DotNet.build id)
)

Target.create "Test" (fun _ ->
    Trace.trace " --- Running standard test suite --- "
    !! "test/**/*.*proj"
    |> Seq.iter (DotNet.test (fun opt -> { 
        opt with Filter=Some("Category!=Slow")}))
)

// Explicit "SlowTest" target
Target.create "SlowTest" (fun _ ->
    Trace.trace " --- Running slow test suite --- "
    !! "test/**/*.*proj"
    |> Seq.iter (DotNet.test (fun opt -> { 
        opt with Filter=Some("Category=Slow")}))
)

// Explicit "Benchmark" target
Target.create "Benchmark" (fun _ ->
    Trace.trace " --- Running benchmarks --- "
    !! "test/*Benchmarks/*.*proj"
    |> Seq.iter (fun proj -> 
        let args = $"-c Release -p {proj} --filter *Suite*"
        DotNet.exec (fun opt -> opt) "run" args |> ignore)
)

Target.create "Publish" (fun _ ->
    Trace.trace " --- Publishing the app artifacts --- "
    !! "test/**/*.*proj"
    |> Seq.iter (DotNet.publish (fun opt -> { 
        opt with Runtime=Some(publishRuntime); SelfContained=Some(false); }))
)

Target.create "QuickBVT" ignore
Target.create "All" ignore

"Clean"
  ==> "Build"
  ==> "Test"
  ==> "QuickBVT"

"Clean"
  ==> "Build"
  ==> "Test"
  ==> "Publish"
  ==> "All"

"Clean"
  ==> "Build"
  ==> "Test"
  ==> "Benchmark"

Target.runOrDefault "QuickBVT"
