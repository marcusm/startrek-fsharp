// --------------------------------------------------------------------------------------
// FAKE build script 
// --------------------------------------------------------------------------------------
#I @"packages/FAKE/tools"
#r @"packages/FAKE/tools/FakeLib.dll"
open System
open System.IO
open Fake
open Fake.AssemblyInfoFile
open Fake.FileUtils
open Fake.ReleaseNotesHelper
open Fake.TaskRunnerHelper
open Fake.Git

RestorePackages()

// --------------------------------------------------------------------------------------
// Build script standard paths and settings
// --------------------------------------------------------------------------------------
let buildDir = "./build/"
let testDir = "./test/"
let deployDir = "./deploy/"
let nugetDir = buildDir @@ "/nuget"

let authors = ["Marcus Martin"]
let projectName = "StarTrek"
let projectDescription = "A port of the old star trek game to F#."

let release =
    File.ReadLines "Release_Notes.md"
    |> ReleaseNotesHelper.parseReleaseNotes

// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------
Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir;]
)

Target "BuildApp" (fun _ ->
    !! "source/StarTrek/*.fsproj"
    |> MSBuildRelease buildDir "Build"
        |> Log "AppBuild-Output:  "
)

Target "BuildTest" (fun _ ->
    !! "tests/**/*.fsproj"
        |> MSBuildRelease testDir "Build"
        |> Log "TestBuild-Output:  "
)

Target "Test" (fun _ ->
    !! (testDir + "./GeneratorTests.dll")
        |> NUnitParallel (fun p ->
                {p with
                    DisableShadowCopy = true
                    OutputFile = testDir + "TestResults.xml" })
)

Target "Default" DoNothing

// Dependencies

"Clean"
    ==> "BuildApp"
    (* ==> "BuildTest" *)
    (* ==> "Test" *)
    ==> "Default"

// start build
RunTargetOrDefault "Default"
