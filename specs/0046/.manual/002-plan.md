# Spec 0046 — plan and design decisions

Restart point for the work requested in `001-task.md`. The live status is in `003-implementation-log.md`.

## Baseline (measured before any change, 2026-10-24)

- `dotnet build Berreman.slnx -c Release` — green; warnings: 4 × `NU1701` (Wolfram.NETLink), 40 × `NU1902`
  (System.Security.Cryptography.Pkcs 10.0.0 / 10.0.9), 4 × `SYSLIB0051` (vendored MathNet C#).
- **`dotnet test` was already broken.** xunit.v3 4.0.0 (bumped in 0044) makes every test project a
  Microsoft Testing Platform app (`IsTestingPlatformApplication=true`), and the .NET 10 SDK refuses
  the VSTest path for those: `error : Testing with VSTest target is no longer supported by
  Microsoft.Testing.Platform on .NET 10 SDK and later`. With `-p:IsTestingPlatformApplication=false`
  VSTest runs them again: `OpticalConstructor.Tests` 693/693, `OpticalConstructor.Ui.Tests` 685/685.

## 1 — Central package versions (the STL pattern)

- `Directory.Build.props` / `Directory.Build.targets` move to the **repository root**, so they also
  govern the vendored `MathNetNumerics/` projects (a sibling of `Berreman/`, which the old
  `Berreman/Directory.Build.props` never reached).
- `Directory.Build.targets` holds the ONE `PackageVersion` item per third-party package plus the STL
  `PackageReference Update="@(PackageVersion)"` line; projects declare `PackageReference` without a
  version. FSharp.Core is pinned once via `FSharpCoreImplicitPackageVersion` (props); the per-project
  `PackageReference Update="FSharp.Core"` pins go.
- Versions come from STL's list wherever STL has the package (Avalonia 12.1.4, FluentAssertions
  [7.2.0], FsCheck 3.5.0, Microsoft.NET.Test.Sdk 18.11.0, xunit.runner.visualstudio 4.0.1,
  coverlet.collector 10.1.0, EF Core Sqlite 10.0.13, Microsoft.Extensions.Hosting(.WindowsServices)
  10.0.13, FSharp.Core 10.1.402, …). Packages STL does not use keep their current versions
  (FsCheck.Xunit follows FsCheck to 3.5.0 because the two ship in lockstep).
- **Decision — Softellect.\* stay at 10.1.301.54.** STL's current Softellect.Sys (10.1.402.120) lives only
  in STL's local `../Packages` feed; nuget.org's newest is 10.1.301.54 and this repo restores from
  nuget.org only. STL itself keeps Softellect.\* pins out of the central list for the same reason.

## 2 — Release only

As STL: `Configurations=Release` + default `Configuration=Release` in props, an `InitialTargets` guard
in targets that fails any non-Release build (`RELEASEONLY`), and every `Debug|x64` property group is
removed from the project files (incl. the vendored MathNet projects).

## 3 — Version numbering (STL slow ring, one ring for the whole app)

- Version = `<FSharp.Core major.minor.patch>.<build>`, e.g. `10.1.402.7` (the counter continues from the
  packages' old `10.0.100.6`).
- `Berreman/Berreman/BuildInfo.fs` (in `Softellect.Berreman.Core`, the bottom of the graph) holds the
  `BuildNumber` / `VersionPrefix` literals and the elevated `AppVersion`; `BuildInfo.ps1` beside it is
  the PowerShell/MSBuild mirror that `Directory.Build.props` reads into `$(Version)` for every assembly.
- `Berreman/PreBuild/IncrementBuildNumber.fsx` bumps: it is skipped while
  `Berreman/Berreman/.version-update-lock` exists, so deleting that file is how a bump is requested.
  `Berreman.fsproj` builds `PreBuild` first (the STL `EnsurePreBuildIsBuilt` shape).
- The Launcher shows `Version <x>` in a footer line (stable id `LauncherVersionText`).

## 4 — Reflected rays and attaching elements to them

- New pure Domain module `BeamLayout` (beam TREE): every Main-scene element carries a
  `BeamHost` = `OnMainBeam | OnReflectedBeamOf of ElementId`. A polarizer / lens / sample that emits
  BOTH branches opens a reflected SIDE beam; elements hosted on it snap onto it (along-beam spacing is
  the x difference, exactly as on the main beam). Hosts that vanish (element removed, R switched off,
  a cycle) resolve to the main beam.
- The right-click element menu returns as a model-driven overlay (not an Avalonia `ContextMenu`, so it
  is deterministic and headless-testable): toggle **Reflected ray** / **Transmitted ray** (the
  `Emission` smart setters, so both-off stays unrepresentable; mirrors are fixed to R, a bound
  ThinFilm sample to R), **Place on main beam / Place on reflected ray of …**, **Remove**.

## 5 — Experiments capture what the detectors see; the window scrolls

- The capture choices are derived from the scene: a detector whose beam path leaves the measured
  element (the bound sample, else the varied element) through T offers T, through R offers R; both
  offer T, R and Both; none offers nothing (Add disabled, with a hint). The draft's request is coerced
  to what is available.
- The Main window's content is wrapped in a `ScrollViewer`, so a tall bay never pushes content out of
  reach.

## 6 — One crossing law for every element

`RayModel.crossElement`: the incoming ray meets the element's FRONT face, refracts inside (Snell,
n = 1.0 → 1.5), leaves the BACK face and continues PARALLEL to the incoming ray (laterally shifted);
the reflected ray leaves the front-face entry point. The faces are the drawn ones (half-depth =
zoom × B / 2), and the face normal is the drawn element's N1 — every non-source element is oriented
relative to its incoming beam, mirrors included — so ray and drawing can never disagree.
`outgoingDirection Transmitted` is therefore the incoming direction (the inside direction is separate).
