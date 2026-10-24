namespace Berreman

/// Spec 0046 — the app's version identity, and the ONE place F# code reads it from. The scheme is
/// STL's slow ring (`C:\GitHub\Softellect\Sys\BuildInfo.fs`) with a single ring for the whole app:
/// every assembly, package and window carries `<VersionPrefix>.<BuildNumber>`, e.g. `10.1.402.7`.
///
/// Both literals are stamped by `Berreman/PreBuild/IncrementBuildNumber.fsx` on every bump: the
/// counter is incremented, and the prefix is written from the central `FSharp.Core` pin
/// (`FSharpCoreImplicitPackageVersion` in the root `Directory.Build.props`), which is what decides it.
/// `BuildInfo.ps1` beside this file is the mirror of the same version that `Directory.Build.props`
/// reads into `$(Version)` (MSBuild and PowerShell cannot read this assembly). Do not hand-edit.
module BuildInfo =

    /// The app's build number — the fourth component of the version.
    [<Literal>]
    let BuildNumber = 7

    /// The app's `major.minor.patch` — its `FSharp.Core` package version.
    [<Literal>]
    let VersionPrefix = "10.1.402"

    /// The app's full version, `<VersionPrefix>.<BuildNumber>`.
    type AppVersion =
        | AppVersion of string

        member this.value = let (AppVersion v) = this in v

    /// The app's full version, e.g. `10.1.402.7`.
    let version : AppVersion = AppVersion $"{VersionPrefix}.{BuildNumber}"
