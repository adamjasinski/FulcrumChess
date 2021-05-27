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
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "test/**/bin"
    ++ "test/**/obj"
    |> Shell.cleanDirs 
)

Target.create "Build" (fun _ ->
    !! "src/**/*.*proj"
    |> Seq.iter (DotNet.build id)
)

Target.create "Test" (fun _ ->
    !! "test/**/*.*proj"
    |> Seq.iter (DotNet.test (fun opt -> { 
        opt with Filter=Some("Category!=Slow")}))
)

Target.create "Publish" (fun _ ->
    !! "test/**/*.*proj"
    |> Seq.iter (DotNet.publish (fun opt -> { 
        opt with Runtime=Some(publishRuntime); SelfContained=Some(false) }))
)

Target.create "All" ignore

"Clean"
  ==> "Build"
  ==> "Test"
  ==> "All"

Target.runOrDefault "All"
