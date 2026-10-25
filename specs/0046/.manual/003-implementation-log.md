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

## Rotation pivot, R / T defaults, detector placement (operator, same day)

- **Global rotation pivot** (`BeamLayout.RotationPivot`: `PivotAtCentre` default | `PivotAtIncidentFace`),
  a checkbox in the Rotation bay (`RotateAboutIncidentFaceCheck`), held in `Model.rotationPivot` and
  `Scene.SceneSnapshot.pivot`. About the incident face, an element's beam point is the middle of its
  incident face (its centre sits half a depth behind it), so the incident ray always enters there; inside
  it bends by Snell and leaves the back face off its middle at non-normal incidence. Mirrors reflect at the
  beam point at every tilt. `RayModel.crossElementThrough` generalises the crossing to any line point
  (`crossElement` = through the centre). The source keeps rotating about its centre (it has no incident
  face). Render: `008-pivot-incident-face.png`.
- **R / T defaults.** New sample geometry `Mirror` — the ONLY one pinned to locked R-only; a `ThinFilm` now
  reflects AND transmits (default `EmitBoth`, constrainable) like a `Plate`; the seeded EUV Mo/Si multilayer
  is the one `Mirror` seed. The sample editor offers Thin film / Plate / Mirror (R fixed + no T box only for
  a Mirror; leaving Mirror restores R + T). Lenses carry T only, structurally (`defaultEmission`,
  `outgoingGroups`, `sideBeamOf`, `rayControlOf`), as mirrors carry R only.
- **Detectors.** A detector now TERMINATES its beam wherever it sits: elements further along are laid out
  but `Unlit` (no ray drawn to them, no captures). `SceneLayout.rays` lists the lit rays in walk order
  (main beam first) with how each ends; an added detector goes to the END of the first ray no detector
  terminates (`BeamLayout.firstFreeRay`, `rayEndX`), else onto the main beam at the usual position, where it
  terminates the beam. Renders: `009-added-detector-free-ray.png`, `010-added-detector-no-free-ray.png`.
- Tests updated to the new rules (Mirror is the reflect-only geometry; lens T-only) and added: pivot entry
  point / exit off the back-face middle / mirror reflection point, detector termination and captures,
  free-ray order, detector placement, the pivot checkbox, the pivot surviving a scene round-trip.
  Constructor 714, ui-smoke 198, ui-tests 509; full no-incremental build 0 errors, only vendored SYSLIB0051.
  BerremanTests was stopped by the operator and not re-run.

## Free light source, uncapped detectors, Add first, menu Choose… (operator, same day)

- **Light source.** It was already selectable by a click (now proven by a test); what it lacked was a way
  to place it. `Placement` gains `TableAxis` (x / y / z), `MoveLock` / `MoveLocks`, `Mobility`
  (`MovesFreely` for the light source only, `MovesAlongBeam` for everything snapped to a beam) and two
  placement fields, `height` (table z) and `moveLocks`; `ElementPlacement.create LightSource` locks every
  move AND every rotation (R1 / R2 / R3). The lock-respecting `withPositionOn` mirrors `withR1..3`. The beam
  root is the source's 3-D centre (`centreOf`), so raising, shifting or steering the source carries the whole
  beam and every snapped element with it. Both placement schemas (project, groups) carry the new fields.
- **Move bay.** The source selected → the new `TablePositionControls` bar: X / Y / Z rows, each with − / +,
  a metres field and Lock / Unlock, plus Reset position (back to the default source point, height 0 —
  bypassing the locks like the rotations' Reset). X / Y stay on the plate, Z is not capped. Any other
  element → the along-beam bar as before. Each bar is keyed, so switching the selection recreates it.
- **Rotation bay.** `RotationControls` offers R1 / R2 Lock / Unlock toggles (`AxisLockOffer`) next to their
  axis; elements offer them, the table does not. The slots are always present (hidden when not offered) so
  FuncUI never patches a named field into another axis' slot. The Main screen uses the new
  `wrappingView` (units wrap between axes; the Main window is 820 px wide); the rotation test windows keep
  the single-row `view`, whose layout their coordinate-based headless tests pin.
- **Detectors.** Uncapped along their ray (`alongBeamRange` → `UnboundedRange`); every other element still
  stops at ±half the plate. Reset puts a detector 1 m along (the default detector point's x). The
  along-beam readout states the range ("uncapped" or the bounds).
- **Ribbon.** Add is the first bay; Rotation stays the bay open at start.
- **Element menu.** A Choose… row under the title runs the Selector bay's Choose… (`chooseFromLibrary`) for
  the element the menu serves, after closing the menu.
- Renders (real Main window size): `011-source-locked-rotation.png`, `012-source-moved-beam-follows.png`,
  `013-detector-uncapped.png`, `014-source-menu-choose.png`.
- Tests: new `SourceAndDetectorMoveTests` (11), two `PlacementTests`, one `LibraryWindowTests` acceptance
  (right-click the source → Choose… → bind `src-600`); bay-order assertions moved to Add first.
  Constructor 716, ui-smoke 201, ui-tests 719 in all; full no-incremental build 0 errors, only vendored
  SYSLIB0051. BerremanTests not run (operator stop).

## Source editor, seeded sources, elements held to the ray (operator, same day)

- **Elements are held to their ray — one rule, the table plays no part** (operator clarification). A ray
  starts at the element it leaves (the source for the main beam, the host for a reflected ray) and ends
  at its first detector, or 1 m along (`openRayEndX`, the default detector point) when it has none. Every
  non-detector element slides within that span (`raySpan`); a detector slides anywhere past the start,
  which extends the ray (`RayPositionControls.AtLeast`, readout "from … m, uncapped"). Adding an element,
  moving it onto a reflected ray and Reset all respect the span; an added detector lands at the open
  ray's end (1 m) or just past the last element on it. The plate clamps are gone, including the source's
  x / y limit from the previous round — the source now moves freely in all three directions.
- **Seeded sources** (`Library.SeedSources`, protected presets beside the 600 nm reference): lasers — HeNe
  632.8 / 543.5, Ar-ion 488 / 514.5, HeCd 441.6 / 325, GaN diode 405, ruby 694.3, Ti:sapphire 800,
  Nd:YAG 1064 and its harmonics 532 / 355 / 266, erbium fibre 1550, CO₂ 10.6 µm; lithography — mercury g /
  h / i lines 435.8 / 404.7 / 365, KrF 248.4, ArF 193.4, F₂ 157.6, EUV 13.5 (Sn plasma), BEUV 6.7. The
  seeded tree groups them under Sources → Lasers / Lithography (UV / EUV).
- **Source editor.** User light sources live in the new `Library.SourceProxy` (in-memory, unversioned —
  an experiment binds a source by entry id; validation refuses a blank name, a non-positive wavelength
  and a protected source). `LightSourceEditorView` / `LightSourceEditorWindow` (the Sample editor's
  sibling) edit the name and the wavelength, typed in nm / µm / Å / eV / cm⁻¹ through the `Units` seam
  (a unit switch never moves the value), with the Sample editor's Save / Cancel and dirty exit gate. The
  Library window gains Add source, Edit for a user source, and confirm-gated Remove for a user source;
  the seeded sources keep no Edit and refuse Remove. Window key `SourceEditorKey`.
- **The Main scene's Library is live** (`Library.liveLibrary`, composed in `AppContext.create` and the
  test defaults): the seeded presets over the samples and user-sources stores. This also closes a gap
  found on the way: a sample created in the Library window could be chosen for a table element but never
  resolved there (the workbench read only the fixed seeded Library), so it showed as unbound.
- Threading: `AppContext.sources`; the workbench `Model.sources` (defaulted, threaded by record update like
  `experimentData`); `LibraryWindow` and `EditorLaunchers` take the store.
- Renders: `015-source-editor.png`, `016-library-sources.png`, `017-element-held-to-ray.png`.
- Tests: new `SourceLibraryTests` (6) and `LightSourceEditorTests` (9); ray-rule tests in
  `SourceAndDetectorMoveTests`; the Library-window corpus counts now derive from `seedEntries`, and its
  click helper scrolls a row into view first (the longer tree put rows below the window). Constructor
  722, ui-smoke 203, ui-tests 730 in all; full no-incremental build 0 errors, only vendored SYSLIB0051.
  BerremanTests not run (operator stop).

## Moving the source on x carries the ray (operator, same day)

- An element's along-beam x is measured from the source (the beam lays each element at its x minus
  the previous one's, starting from the source's x), so moving the source on x used to stretch or cut
  the ray. Now an x move of the source — and its Reset — shifts every other element by the same amount
  (`carryRayWithRoot`), exactly as its y / z moves and rotations already carried them through the beam.
  The default scene's positions are read against the source (`alongFromSource`): the open-ray end (1 m
  in the default scene, i.e. 2 m past the source), a detector's Reset point, the others' Reset point,
  and where new elements land. A locked x moves nothing.
- Test: `SourceAndDetectorMoveTests` — after a 0.4 m x move every gap is kept and every element (a
  detector on a reflected ray included) is drawn exactly 0.4 m further along; Reset carries it back.
  Constructor 722, ui-smoke 203, ui-tests 731. BerremanTests not run (operator stop).

## Element info, polarizers T-only, Library preselect, tunable sources, detector-decided captures, FSX acceptance (operator, same day)

1. **What an element is.** `elementInfo` (kind #n, the bound entry's name and full description, or
   "Unbound"): an invisible hover disc over each element on the table carries it as a tooltip
   (`ElementHover_<id>`), and the element menu shows it under its title (`ElementMenuInfo`).
2. **Menu width.** The element menu sizes to its text between 260 and 460 px and every line wraps.
3. **Polarizers transmit only — locked** (`defaultEmission`, `outgoingGroups`, `sideBeamOf`,
   `rayControlOf`); tests that leaned on a reflecting polarizer now use a sample.
4. **The Library shows the chosen entry.** `SelectionContext.currentEntryIdOpt`: Choose… (Selector or menu)
   passes the element's binding; the Library window opens (or is re-targeted) with that entry selected,
   highlighted, and the entries group expanded.
5. **Tunable sources.** `SourcePreset.tuning = FixedLine | TunableBetween (min, max)`; `waveLength` is the
   line or the tunable setting. Seeds: xenon lamp + monochromator 190–1000 nm, Ti:sapphire 700–1000 nm,
   supercontinuum 450–2400 nm, EUV beamline 5–20 nm. Only a tunable source varies its wavelength (a fixed
   line or an unbound source varies nothing); the sweep defaults to and stays within its limits; while
   something else varies, the bay shows the source wavelength — editable within the limits for a tunable
   source (`Experiment.sourceWaveLengthOpt`), read only for a fixed line. The source editor gained
   Fixed line / Tunable with the limits.
6–7. **The detector decides what is captured.** Each capture branch is read by the detector on it
   (`captureDetectors`, `detectorKindOf`): an intensity detector gives "Intensity", an ellipsometer "Ψ" and
   "Δ"; the bay's offers say so ("Reflected (R) — Ψ, Δ (ellipsometer)"), and step 3 (and the range) appear
   only once something to vary is chosen.
   **The run was rebuilt on the light's real path** (`ExperimentRun`, `chartForRun`): previously the FIRST
   polarizer in the scene served as both input polarizer and analyzer, wavelength / R1 runs solved the
   sample at normal incidence whatever its tilt, and one detector kind applied to every branch. Now the
   detector's own path is walked: polarizers before the sample prepare the input, the sample is solved at
   its drawn incidence and spin, polarizers after it analyze.
8. **FSX acceptance** (`FsxAcceptanceTests`, 14 tests): Glass (film, plate; vs incidence), Glass_Dispersive
   (film, plate; vs λ at 79°), LangasiteOnSilicon (normal, 79°; vs λ), MultilayerThinFilm (vs incidence,
   vs λ), MultilayerThinFilm_02 (vs incidence, vs λ), MultilayerThinFilm_EUV and _EUV__002 (vs incidence
   at 10 nm, vs λ 5–20 nm). Each stores the script's materials / sample / source in the app's stores,
   builds the table and the experiment through the app's messages, mounts the Main workbench headless and
   clicks Export data (the new `ExperimentExportProxy`; in-memory store in tests, CSV into
   Documents/Optical Constructor/Exports in the app), and compares T and R with the script's
   `Variables.calculate` — all within 1e-6. Excluded: Wedge*, Work_01, References (loader), TestChart,
   TotalRefl_Glass (incident medium glass), ActiveCrystal (varies a material constant),
   ActiveCrystalComparison (plots the transmitted polarization azimuth / ellipticity, which no detector
   reads).
   **Two real bugs found by these tests:**
   - the constructor's T was wrong for any exit medium other than vacuum: the engine's Mueller matrices are
     built from |E|², its T is the energy flux — the run now corrects the transmitted matrix with the
     engine's own s/p flux-to-amplitude ratios (exact for s / p input, also in absorbing silicon at 79°);
   - **every wavelength sweep in the Analytics scripts was wrong**: `Variables.getWaveLengthValue` wrapped
     the range value (metres) as nanometres, so each point was solved at ~10⁻⁷ nm while the x axis showed
     the right nm. Fixed in `Analytics/Variables.fs`. The one solver test sweeping wavelength
     (`OptimizationTests`, total internal reflection, R = 1 at any λ) is unaffected; BerremanTests not run
     (operator stop).
- Tests: constructor 727, ui-smoke 206, ui-tests 757 in all (the UI suite now takes ~5 minutes with the
  acceptance tests); full no-incremental build 0 errors, only vendored SYSLIB0051.

## Findings

- F1: `dotnet test` broken on the .NET 10 SDK since the xunit.v3 4.0.0 bump (see item 1).
- F2: `BerremanModelGenerator/ModelGenerator.fsproj` (not in the solution) does not compile and did not
  before this spec: it uses Argu without referencing it and links an `appsettings.json` from another
  repository. Left as is.
- F3: `BerremanTests` takes 13–25 minutes, not the gate's 180 s estimate; xunit.v3 runs it in a child
  process (the `testhost` itself shows almost no CPU), so it is easy to mistake for a hang. Killing the
  host does NOT fail the run: VSTest still prints "Passed!" over whatever finished (seen twice: 177 and
  197 of the 203 listed tests), so only an uninterrupted run's count is evidence.
