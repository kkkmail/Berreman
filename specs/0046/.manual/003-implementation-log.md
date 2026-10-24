# Spec 0046 — implementation log

Implements `001-task.md` per the design in `002-plan.md`. This is the restart point: read **Status**,
then **Next action**.

## Status

| # | Item | State |
|---|---|---|
| 1 | Central package versions (STL pattern), versions aligned with STL | **DONE** |
| 2 | Release only (Debug removed) | **DONE** |
| 3 | App version `<FSharp.Core>.<build>` + Launcher footer | **DONE** — `10.1.402.7` |
| 4 | Reflected rays: right-click element menu, attach elements to a reflected ray | **DONE** |
| 5 | Experiment captures follow the detectors; Main window scrolls | **DONE** |
| 6 | One crossing law: front face → Snell inside → exits parallel, for every element | **DONE** |

**Next action:** none.

## Gate state (final, after the follow-up)

- `dotnet build Berreman.slnx -c Release --no-incremental` — 0 errors; the ONLY warnings left are
  4 × `SYSLIB0051` from the vendored MathNet C# project (pre-existing, untouched). The baseline's 40 ×
  `NU1902` are gone (Softellect.Sys 10.1.402.120 raises the crypto floors that CoreWCF dragged in) and the
  4 × `NU1701` left with BerremanRunner (Wolfram.NETLink). No "error" text in the output.
- `OpticalConstructor.Tests` **706 / 706** (baseline 693 → +13 `BeamLayoutTests`).
- `OpticalConstructor.Ui.Tests` **700 / 700** — ui-smoke 196, the rest 504 (baseline 685 → +2 launcher,
  +13 `ElementMenuTests`).
- `BerremanTests` — 203 tests listed; see the last section.

## What changed, by item

### 1 — Central package versions

- `Directory.Build.props` / `Directory.Build.targets` now live at the repository ROOT (the old
  `Berreman/Directory.Build.props` is gone — MSBuild imports only the nearest one, and the root reaches
  the vendored `MathNetNumerics/` projects too).
- `Directory.Build.targets`: one `PackageVersion` per package + the STL
  `PackageReference Update="@(PackageVersion)"` line. All `Version="…"` attributes and all
  `PackageReference Update="FSharp.Core"` pins left the project files; FSharp.Core is pinned once
  (`FSharpCoreImplicitPackageVersion` = 10.1.402, props).
- Upgraded to STL's versions: Avalonia (+Desktop, Themes.Fluent, Headless) 12.1.1 → 12.1.4,
  FluentAssertions [7.0.0] → [7.2.0], FsCheck 3.3.4 → 3.5.0 (FsCheck.Xunit with it — lockstep),
  Microsoft.NET.Test.Sdk 18.9.0 → 18.11.0, xunit.runner.visualstudio 4.0.0 → 4.0.1, coverlet.collector
  10.0.1 → 10.1.0, Microsoft.EntityFrameworkCore.Sqlite 10.0.11 → 10.0.13, Microsoft.Extensions.Hosting
  (+WindowsServices) 9.0.0 → 10.0.13, FSharp.Core 10.1.400 → 10.1.402. Already equal: alglib.net, Elmish,
  Avalonia.FuncUI(+Elmish), FSharp.Data, Plotly.NET.
- STL packages: see "Follow-up (operator, same day)" below — Softellect.Sys is pinned to STL's current
  slow ring 10.1.402.120 and restores from the local feed AND nuget.org.
- **Found and fixed (pre-existing):** `dotnet test` was broken for all three test projects on the .NET 10
  SDK — xunit.v3 4.0.0 makes them Microsoft Testing Platform apps and the SDK refuses the VSTest path.
  The root props set `IsTestingPlatformApplication=false`, so `dotnet test` runs VSTest again and prints
  the `Passed: N` summary the gates parse.

### 2 — Release only

`Configurations=Release` + default `Configuration=Release` (props); `InitialTargets` guard
`BerremanForbidNonReleaseConfiguration` fails anything else with `RELEASEONLY` (verified:
`dotnet build -c Debug` → `error RELEASEONLY`). Every `Debug|x64` property group is removed (all 21
project files, incl. vendored MathNet).

### 3 — Version numbering

- `Berreman/Berreman/BuildInfo.fs` (compiled first in `Softellect.Berreman.Core`): `BuildNumber = 7`,
  `VersionPrefix = "10.1.402"`, `AppVersion`, `version`.
- `Berreman/Berreman/BuildInfo.ps1`: `[string] $global:buildNumber = "10.1.402.7"` — read by the root
  props into `$(Version)` for every non-vendored project. `GenerateAssemblyInfo=false` was removed from
  our projects so assemblies carry it (the STL `BerremanDropCoreWcfGeneratorAttribute` target handles
  CoreWCF's unparsable attribute); verified `Softellect.Berreman.Core.dll` / `…App.dll` file version
  10.1.402.7 and package `Softellect.Berreman.Core.10.1.402.7.nupkg`.
- `Berreman/PreBuild/IncrementBuildNumber.fsx` + lock `Berreman/Berreman/.version-update-lock`; the
  `IncrementBuildNumber` target in `Berreman.fsproj` runs the script only while the lock is missing.
  The script itself produced the shipped version: seeded with the old `10.0.100.6`, it bumped to
  `10.1.402.7`, deriving the prefix from the FSharp.Core pin.
- Launcher: a right-aligned `Version 10.1.402.7` footer docked at the bottom (`LauncherIds.versionText`).

### 4 — Reflected rays

- New Domain module `BeamLayout.fs` (after `Experiments.fs`): `BeamHost` (`OnMainBeam |
  OnReflectedBeamOf ElementId`), `sideBeamOf`, `rayControlOf` / `toggleRay`, `effectiveHosts`
  (memoized; vanished / switched-off / looping hosts fall back to the main beam), `layout`,
  `hostedUnder` / `attachableHosts`.
- `TestElement.beam` and `Scene.SceneElement.beam` carry the host, so scenes round-trip it.
- Right-click on the Main canvas opens a model-driven element menu (`Model.elementMenu`; ids in
  `UiIds.ElementMenu`): ☑/☐ Reflected ray, ☑/☐ Transmitted ray (fixed, with the reason, for mirrors and
  reflect-only bound samples; a source / detector only shows the reason), "Sits on" ● Main beam / ○
  Reflected ray of …, Remove element, Close. Messages: `OpenElementMenuAt`, `CloseElementMenu`,
  `ToggleElementRay`, `PlaceOnBeam`. Removing a host moves its reflected-ray elements to the main beam;
  switching a host's reflected ray off parks them on the main beam until it is switched back on.

### 5 — Experiments and scrolling

- `availableCaptures` (Ui) / `BeamLayout.detectedBranches` + `capturesFor` + `effectiveCapture`:
  T / R are the bound sample's branches (else the varied element's) on the light paths to the detectors
  that also see the varied element. Only T detector → only T; only R → only R; both → T, R, Both; none →
  nothing (Add disabled, hint `ExperimentNoCapture`). Without a bound sample only T is offered (the run is
  a pass-through with nothing to reflect). The draft keeps its request; the chart, the Ψ/Δ readout and the
  committed experiment use the coerced capture. `ExperimentControls.State.measurementChoices` hides the
  rest.
- The Main view sits in a vertical `ScrollViewer` (`TableElementScrollViewer`). Horizontal scrolling is
  DISABLED on purpose: measured at infinite width, the wrapping tab strip pushed the right-docked
  Materials… / Library… buttons off-screen (caught by the window acceptance tests).

### 6 — The crossing law

- `RayModel.crossElement` (new): entry on the front face (the incoming line through the centre),
  `inside` by Snell (1.0 → 1.5), exit on the back face, `transmitted` = incoming direction (parallel,
  laterally shifted), `reflected` off the front face from the entry. Zero depth / grazing collapses to the
  centre.
- `RayModel.orientedToBeam` (new): R2/R3 measured from the incoming beam; `faceNormal` = its N1 (identical
  to the old in-plane re-basing for in-plane beams). `outgoingDirection Transmitted` is now the incoming
  direction — the old code returned the INSIDE direction, which is exactly the rotating second segment the
  operator saw.
- The Main scene draws `BeamLayout` segments (approach, inside, …), with the faces at the DRAWN half-depth
  (zoom × B / 2) and every non-source element — mirrors included — oriented on its beam
  (`autoOrientsToBeam`), so the ray crosses exactly the drawn surfaces.

## Tests changed to the new behaviour (operator-requested semantics)

- `RayModelTests` AC-B6: Snell is asserted on the INSIDE direction; the exit is asserted parallel.
- `ExperimentControlsTests` acceptance (012) and "capturing BOTH…", `PlacedSampleEmissionTests` (Plate):
  a single transmitted detector now yields T; Both needs a second detector on the reflected ray.
- `TableAndElementRotationTests` auto-orient: mirrors are now oriented on their beam too.
- `SceneRoundTripTests`: one element sits on a reflected ray, and the host round-trips.

## Follow-up (operator, same day)

- **BerremanRunner removed from `Berreman.slnx`** (folder kept). It was STL's DistrProc sample solver
  runner copied verbatim (generic ODE demos, `solverName = "Berreman"`), referencing no Berreman project;
  it was the solution's only reason to reference `Softellect.DistributedProcessing.SolverRunner`, whose
  central pin now serves only the on-disk project.
- **STL packages restore from BOTH feeds.** Root `nuget.config`: `local-packages` = `../Packages`
  (C:\GitHub\Packages) and nuget.org, with `Softellect.*` mapped to BOTH at the same specificity (STL's
  own configs map it to the local feed alone); everything else from nuget.org. Proven with an empty
  throw-away packages folder: Softellect.Sys 10.1.402.120 installed from `C:\GitHub\Packages`, the
  10.1.301.54 Softellect packages (BerremanRunner's closure) from nuget.org.
- **Softellect.Sys pinned to 10.1.402.120**, STL's current slow ring. Its API moved since 10.1.301.54,
  adapted at the byte / settings boundary: `serialize` returns `SerializedData` (`FitQuality.toBinz` /
  `fromBinz`, `Sidecar.writeSidecar` / `readSidecar` wrap and unwrap it; the public `byte[]` signatures
  and the .binz format are unchanged), and `AppSettingsProvider` reads every type through the generic
  `getOrDefault key default parse render` (`AppConfig.fs` got bool / int parse + render doors; the test
  uses `tryGet` and the elevated `ConnectionString`).
- A mistaken STL slow-ring BUMP to 10.1.402.121 was reverted by the operator; every `.121` artifact was
  then deleted (18 packages in `C:\GitHub\Packages`, 18 `.nupkg` + 18 `.nuspec` in STL's bin/obj). STL is
  at 10.1.402.120, clean. Rule (CLAUDE.md): never bump STL's ring from this repository unasked.

## Rays past the faces (operator `.manual/004` error, `005` expected)

Reproduced by rendering the REAL `mainView` headlessly with Skia to PNG (a scratch renderer outside the
repo: `AppBuilder.Configure<App>().UseSkia().UseHeadless(UseHeadlessDrawing = false)`, then
`CaptureRenderedFrame`). The operator's scene: source, LP (-0.3), sample (-0.1) at R2 = -50°, LP (0.1),
detector. Neither stray line in 004 was a ray:

- **The "ray 1 runs past the sample" piece was the first LP's N1 normal marker** — drawn from its centre for
  1.8 × its face half-size (0.225 m) in exactly the ray's blue (30, 90, 200); at normal incidence it lies on
  the beam and crossed the sample's face. The detector's marker likewise ran on past the detector.
- **The "ray 3 starts far too early" piece was the second LP's open reflected ray** — at normal incidence it
  goes straight back, and was drawn a fixed 0.5 m, through the sample; the first LP's ran back into the
  source box.

Fixes:

- `BeamLayout.layout`: an OPEN ray (a reflected ray with nothing on it, a beam with no detector at its end)
  stops at the first DRAWN element box it meets (slab test against every other element's oriented box) and
  is at most `openBeamLength`. A back-reflection now ends on the element it travels back to (the second LP's
  at the sample's exit point, the first LP's on the source's face). `LayoutElement` carries the draw `zoom`
  (the whole drawn box), not just a half-depth.
- `ElementRenderer`: the N1 marker has its own colour (`normalAxisColor`, teal) — no marking shares the ray
  colour — and `Drawable.normals` (`ShowNormals | HideNormals`) lets a host hide them; the Main view shows
  them on the SELECTED element only (the test windows keep them on every element).
- Rendered result: `006-rotated-sample-fixed.png` (the 004 view) and `007-full-table-fixed.png` (the whole
  table — the beam starts on the source's face and ends on the detector's face).
- Tests: `BeamLayoutTests` +3 (back-reflections stop on the element they meet, nothing behind the source /
  past the detector, an unobstructed open ray keeps its full length); `ElementRendererBindingTests` +2
  (normal colour ≠ ray colour; Hide / ShowNormals in both renderers); `ElementMenuTests` +1 (the Main view
  draws one N1 marker — the selected element's). Constructor 709, ui-smoke 197, ui-tests 506.

## Findings

- F1: `dotnet test` broken on the .NET 10 SDK since the xunit.v3 4.0.0 bump (see item 1).
- F2: `BerremanModelGenerator/ModelGenerator.fsproj` (not in the solution) does not compile and did not
  before this spec: it uses Argu without referencing it and links an `appsettings.json` from another
  repository. Left as is.
- F3: `BerremanTests` takes 13–25 minutes, not the gate's 180 s estimate; xunit.v3 runs it in a child
  process (the `testhost` itself shows almost no CPU), so it is easy to mistake for a hang. Killing the
  host does NOT fail the run: VSTest still prints "Passed!" over whatever finished (seen twice: 177 and
  197 of the 203 listed tests), so only an uninterrupted run's count is evidence.
