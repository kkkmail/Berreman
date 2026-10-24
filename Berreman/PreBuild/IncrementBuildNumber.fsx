// Increments the app's build number and stamps it everywhere it is recorded (spec 0046) — the STL
// slow-ring scheme (C:\GitHub\Softellect\PreBuild\IncrementBuildNumber.fsx) reduced to ONE ring for the
// whole app: no families, no package re-pin sweep (every project references the others by project).
//
//   dotnet fsi Berreman/PreBuild/IncrementBuildNumber.fsx
//
//   counter  Berreman/Berreman/BuildInfo.fs           BuildNumber + VersionPrefix literals (F# code)
//   mirror   Berreman/Berreman/BuildInfo.ps1          read into $(Version) by the root Directory.Build.props
//   lock     Berreman/Berreman/.version-update-lock   the bump is SKIPPED while it exists
//
// Deleting the lock file is how a bump is requested; this script re-creates it after bumping, so one
// request is one bump. Berreman.fsproj runs this script before it builds whenever the lock is missing,
// but MSBuild has already read $(Version) by then — so take the bump BEFORE the build:
//
//   rm Berreman/Berreman/.version-update-lock
//   dotnet fsi Berreman/PreBuild/IncrementBuildNumber.fsx
//   dotnet build Berreman/Berreman.slnx -c Release
//
// The prefix is DERIVED, never chosen: it is the central FSharp.Core pin
// (FSharpCoreImplicitPackageVersion in the root Directory.Build.props), re-read on every bump.

open System
open System.IO
open System.Text.RegularExpressions
open System.Xml.Linq

/// The regex that finds the counter in BuildInfo.fs.
[<Literal>]
let BuildNumberPattern = @"let\s+BuildNumber\s+=\s+(\d+)"

/// The regex that finds the major.minor.patch literal in BuildInfo.fs.
[<Literal>]
let VersionPrefixPattern = "let\\s+VersionPrefix\\s+=\\s+\"([^\"]+)\""

/// The regex that finds the version string in the PowerShell mirror.
[<Literal>]
let MirrorPattern = "(\\[string\\]\\s+\\$global:\\w+\\s+=\\s+\")([^\"]+)(\")"

let repoRoot = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))
let buildInfoPath = Path.Combine(repoRoot, "Berreman", "Berreman", "BuildInfo.fs")
let mirrorPath = Path.Combine(repoRoot, "Berreman", "Berreman", "BuildInfo.ps1")
let lockFilePath = Path.Combine(repoRoot, "Berreman", "Berreman", ".version-update-lock")
let centralPropsPath = Path.Combine(repoRoot, "Directory.Build.props")

/// Why a bump could not be taken — each case names the file it could not read or write.
type BumpFailure =
    | MissingPattern of file : string * what : string
    | MissingFSharpCorePin of file : string
    | IoFailure of file : string * reason : string

    member this.text : string =
        match this with
        | MissingPattern (file, what) -> $"Cannot find {what} in {file}."
        | MissingFSharpCorePin file -> $"Cannot read a three-part FSharpCoreImplicitPackageVersion from {file}."
        | IoFailure (file, reason) -> $"Cannot access {file}: {reason}"

let readText (path : string) : Result<string, BumpFailure> =
    try Ok (File.ReadAllText path)
    with e -> Error (IoFailure (path, e.Message))

let writeText (path : string) (text : string) : Result<unit, BumpFailure> =
    try Ok (File.WriteAllText(path, text))
    with e -> Error (IoFailure (path, e.Message))

/// The central FSharp.Core pin's major.minor.patch — the version prefix.
let centralVersionPrefix () : Result<string, BumpFailure> =
    try
        XDocument.Load(centralPropsPath).Descendants(XName.Get "FSharpCoreImplicitPackageVersion")
        |> Seq.tryHead
        |> Option.map (fun e -> e.Value.Trim().Split('.'))
        |> Option.filter (fun parts -> parts.Length >= 3)
        |> Option.map (fun parts -> $"{parts.[0]}.{parts.[1]}.{parts.[2]}")
        |> function
            | Some prefix -> Ok prefix
            | None -> Error (MissingFSharpCorePin centralPropsPath)
    with e -> Error (IoFailure (centralPropsPath, e.Message))

/// Replace the first capture group of `pattern` in `text`, keeping everything around it.
let replaceGroup (pattern : string) (replacement : string) (text : string) : string =
    Regex(pattern).Replace(text, (fun (m : Match) -> m.Value.Substring(0, m.Groups.[1].Index - m.Index) + replacement + m.Value.Substring(m.Groups.[1].Index - m.Index + m.Groups.[1].Length)), 1)

/// Bump the counter, stamp the prefix, and write the mirror. Returns the new version.
let bump () : Result<string, BumpFailure> =
    readText buildInfoPath
    |> Result.bind (fun source ->
        match Regex(BuildNumberPattern).Match source with
        | m when m.Success -> Ok (source, Int32.Parse m.Groups.[1].Value)
        | _ -> Error (MissingPattern (buildInfoPath, "the BuildNumber literal")))
    |> Result.bind (fun (source, current) ->
        match Regex(VersionPrefixPattern).Match source with
        | m when m.Success -> Ok (source, current)
        | _ -> Error (MissingPattern (buildInfoPath, "the VersionPrefix literal")))
    |> Result.bind (fun (source, current) ->
        centralVersionPrefix () |> Result.map (fun prefix -> source, current + 1, prefix))
    |> Result.bind (fun (source, next, prefix) ->
        let stamped =
            source
            |> replaceGroup BuildNumberPattern (string next)
            |> replaceGroup VersionPrefixPattern prefix
        let version = $"{prefix}.{next}"
        writeText buildInfoPath stamped
        |> Result.bind (fun () -> readText mirrorPath)
        |> Result.bind (fun mirror ->
            match Regex(MirrorPattern).Match mirror with
            | m when m.Success -> writeText mirrorPath (Regex(MirrorPattern).Replace(mirror, $"${{1}}{version}${{3}}", 1))
            | _ -> Error (MissingPattern (mirrorPath, "the version string")))
        |> Result.map (fun () -> version))

match File.Exists lockFilePath with
| true ->
    printfn $"Version lock {lockFilePath} exists; the build number is unchanged (delete the lock to bump it)."
| false ->
    match bump () with
    | Ok version ->
        match writeText lockFilePath (DateTime.Now.ToString "o") with
        | Ok () -> printfn $"App version bumped to {version} (BuildInfo.fs, BuildInfo.ps1); the version lock is re-created."
        | Error failure ->
            printfn $"App version bumped to {version}, but the version lock was not re-created. {failure.text}"
            exit 1
    | Error failure ->
        printfn $"The app version was NOT bumped. {failure.text}"
        exit 1
