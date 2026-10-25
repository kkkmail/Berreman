# Berreman

F# implementation of the Berreman 4x4 transfer-matrix method for electromagnetic
wave propagation through stratified (layered), anisotropic and magneto-optic
optical media — plus **Optical Constructor**, a desktop app for building and
fitting optical systems on top of the solver.

This file is the primary restart point when context is reset or compacted. Read
it first.

---

## What this repo is

Two things share one solution (`Berreman/Berreman.slnx`, **x64 only**):

1. **The solver** (`Softellect.Berreman.Core` and friends) — a pure F# numerical
   optics library: 4x4 Berreman matrices, eigen-decomposition, matrix exponential,
   FFT for Gaussian beams, reflectance/transmittance solvers, dispersion, full
   polarization analysis (Stokes / Jones / Mueller). Shipped as NuGet packages.
2. **Optical Constructor** (`OpticalConstructor.*`) — an Avalonia FuncUI desktop
   app (Elmish MVU) that drives the solver: build optical stacks, run parametric
   sweeps, plot with Plotly.NET, fit/optimize material and layer parameters.

The README has the physics and the example catalog. Version numbers in the
README may lag the `.fsproj` files — trust the `.fsproj`.

---

## Build & test

Everything targets **.NET 10 (`net10.0`), x64**, F# 10. Use `dotnet`, **not**
MSBuild — the `.slnx` builds directly. Run build/test from the `Berreman/`
subdirectory (that is where the gates set `cwd`).

```bash
# Build the whole solution (this is exactly what the `build` gate runs)
cd Berreman
dotnet build Berreman.slnx -c Release

# Core/solver unit tests (cwd = Berreman/BerremanTests)
dotnet test --no-build -c Release

# Optical Constructor — domain/logic tests, and headless UI tests
dotnet test Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj -c Release
dotnet test Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj -c Release --filter Category=ui-smoke
```

Test stack: **xUnit v3**, **FsCheck** (property-based), **FluentAssertions**,
and **Avalonia.Headless** for UI tests. Run the relevant tests after every
change; if your local build/tests are green, the matching gate is green too.
`BerremanTests` takes ~13 minutes (the inverse-fit tests); the two constructor
suites take under a minute.

**Release only.** There is no Debug configuration: the root
`Directory.Build.props` defaults `Configuration` to Release and the root
`Directory.Build.targets` fails any other configuration (`RELEASEONLY`). Never
add a `Debug|x64` property group back.

**Central package versions** (the STL pattern). Every third-party package
version lives ONCE, as a `PackageVersion` item in the root
`Directory.Build.targets`; projects write `<PackageReference Include="X" />`
with no version. FSharp.Core is pinned once by `FSharpCoreImplicitPackageVersion`
in the root `Directory.Build.props` — no project pins it. Versions follow STL's
list (`C:\GitHub\Softellect\Directory.Build.targets`) wherever STL uses the
package.

**STL packages restore from BOTH feeds.** The root `nuget.config` maps
`Softellect.*` to the local `../Packages` feed (`C:\GitHub\Packages`, which STL's
slow-ring build publishes into) AND to nuget.org; everything else comes from
nuget.org. Softellect.Sys is pinned to STL's current slow ring (the version in
STL's `Sys/BuildInfo.ps1`, local-feed only until STL uploads it). Moving that pin
means building STL's slow ring at its CURRENT version — **never bump STL's
version ring from this repository** unless the operator explicitly says "bump".

**`dotnet test` runs on VSTest.** xUnit v3 4.x marks test projects as Microsoft
Testing Platform apps, which the .NET 10 SDK refuses on the VSTest path; the root
`Directory.Build.props` sets `IsTestingPlatformApplication=false` so `dotnet test`
works and prints the `Passed: N` summary the gates read. Do not remove it.

---

## Versioning

One version for the whole app, STL's slow-ring scheme:
`<FSharp.Core major.minor.patch>.<build>`, e.g. `10.1.402.7`. Every assembly and
package carries it (vendored MathNet keeps its own); the Launcher shows it in its
footer.

- `Berreman/Berreman/BuildInfo.fs` — `BuildNumber` / `VersionPrefix` literals and
  `BuildInfo.version`, the ONE place F# code reads the version from.
- `Berreman/Berreman/BuildInfo.ps1` — the mirror the root `Directory.Build.props`
  reads into `$(Version)`.
- `Berreman/PreBuild/IncrementBuildNumber.fsx` — the bump: increments the
  counter, re-derives the prefix from the FSharp.Core pin, rewrites both files.

The bump is skipped while `Berreman/Berreman/.version-update-lock` exists, so
**deleting the lock is how a bump is requested**; the script re-creates it.
Take the bump before building (MSBuild reads `$(Version)` at evaluation time):

```bash
rm Berreman/Berreman/.version-update-lock
dotnet fsi Berreman/PreBuild/IncrementBuildNumber.fsx
```

**A green build is non-negotiable:** core projects compile with
`--warnaserror+:25` (uninstantiated-generic warnings are errors).

**Zero warnings from our code.** Every compiler warning that originates in *our*
code must be cleared — not suppressed, not ignored. This covers F# warnings
(`FS####`) and MSBuild warnings that stem from our project configuration or
references — notably **`MSB3277`** (conflicting assembly versions, e.g. two
`WindowsBase` versions): resolve the underlying reference conflict, do not leave
it. The ONLY warnings exempt from this rule are NuGet advisories about
third-party packages themselves, which are outside our control and tracked
separately: **`NU1701`** (fallback-framework restore) and the vulnerability
advisories **`NU1901` / `NU1902` / `NU1903` / `NU1904`**. Anything else is ours
to fix.

---

## Project layout

```
Berreman/                              # solution root (Berreman.slnx, x64)
  Berreman/        Softellect.Berreman.Core         # solver: matrices, fields, media, solvers, dispersion
  OpticalProperties/  ...OpticalProperties          # material definitions (standard / active / dispersive)
  Analytics/       ...Analytics                      # Plotly.NET charting, ranged variables, Examples/*.fsx
  BerremanTests/                                     # solver unit tests (xUnit v3 + FsCheck)
  BerremanRunner/  / BerremanModelGenerator/         # stale console apps kept on disk, NOT in Berreman.slnx
  OpticalConstructor/
    OpticalConstructor.Domain         # pure domain (units, rays, placement, materials, project model)
    OpticalConstructor.Optimization   # fitting / inverse design (Alglib adapter, merit functions)
    OpticalConstructor.Storage        # project files, JSON (de)serialization, schema validation, export
    OpticalConstructor.Ui             # Avalonia FuncUI views (Elmish MVU)
    OpticalConstructor.App            # composition root that hosts the FuncUI shell
    OpticalConstructor.Tests          # domain/logic tests
    OpticalConstructor.Ui.Tests       # Avalonia.Headless UI smoke/structure tests
MathNetNumerics/                       # customized MathNet.Numerics built FROM SOURCE (referenced, not a NuGet)
```

`Softellect.Berreman.Core` references `MathNetNumerics/FSharp` and
`MathNetNumerics/Numerics` as **project references** to a vendored, customized
MathNet.Numerics. Do **not** replace these with the public `MathNet.Numerics`
NuGet package.

**One `.fsproj` per folder.** Never put two project files in one directory.
Share sources by file-linking (`<Compile Include="..\X.fs" Link="X.fs" />`),
not by colocation.

---

## F# code style (MUST follow)

- `camelCase` for values and functions, `PascalCase` for types and modules.
- Opening `{` on its **own line**.
- A **space before every colon** in an annotation: `name : Type`, never `name: Type`.
- **Every record field is `camelCase`** — both data fields and function-valued
  fields (`{ data : CustomerData; saveChanges : unit -> int }`).
- **Interpolated strings, never `sprintf`**: `$"value: {x}"`, not
  `sprintf "value: %d" x`. Applies to logs, errors, ids, and all formatted strings
  (a typed format specifier still works inside the hole: `$"%08x{h}"`, `$"%A{x}"`).
- **Pattern-match.** Never `.IsSome` / `.IsNone` / `.Value` on options, and never
  reach into a single-case DU outside its accessor.
- **Immutable by default.** Mutation only at the IO boundary or in a measured
  hot numerical kernel.
- **Errors are values**: return `Result<'T, 'E>`, do not throw across a public
  boundary. Platform APIs that throw (.NET IO, native interop) are caught AT the
  boundary and mapped to typed errors — F# above the boundary never handles
  exceptions.
- **Explicit concrete types on every public function parameter and return type.**
  No inference reliance at a module-boundary surface; only genuinely generic
  functions keep their type parameters (`Result<'T, 'E>` stays generic), named
  meaningfully (`'Item`, `'Config`) — not bare `'a` / `'b`.
- **Pure functions by default**; side effects live only at the edges.
- Prefer `async` computation expressions for IO.
- Simplicity and correctness over cleverness.
- Four-space indent. For floating-point comparisons in tests, reuse the
  project's tolerance helpers (see `BerremanTests/MatrixComparison.fs`) — do not
  hand-roll new epsilon logic.

---

## Elevate every primitive (the load-bearing discipline)

The single most important rule. **A bare primitive (`string`, `int`, `float`,
`bool`, `byte[]`, `Guid`, `DateTime`) must not appear in a domain type, a public
signature, or a serialized contract.** Every quantity is *elevated* to a named
type that says what it is. Primitives are permitted only at genuine IO
boundaries: the serialization/storage seam and low-level interop.

The solver already lives this — `Angle`, `WaveLength`, `Thickness`,
`RefractionIndex`, `IncidenceAngle`, `Polarization`, `Ellipticity`, `RealVector2`,
etc. are single-case DUs, not raw `double`s. Match that everywhere you add code.

- **Single-case DU per primitive**, with a `.value` accessor, plus a
  `static member tryCreate : … -> Result<_, _>` (or `Option`) wherever the value
  is parsed from untrusted/external input. The `.value` accessor is reached only
  at the IO boundary.

  ```fsharp
  type Angle =
      | Angle of double

      member this.value = let (Angle a) = this in a
      static member degree d = d * degree |> Angle
      static member tryCreate (a : double) : Result<Angle, ...> = ...
  ```

- **No `enum` types.** An enumerated set is a discriminated union with a `.value`
  member mapping to the wire/disk integer (or string) and a `tryCreate` that maps
  an unknown stored value to a *typed error*, never a throw.

- **No naked `bool`** in a domain record or signature. Use a two-case
  named-condition DU (`type LockoutState = LockedOut | NotLockedOut`) so a match
  site reads as prose and a third state is a non-breaking addition.

- **Explicit, concrete signatures on every public function.** Spell out the
  argument and return types; do not rely on inference for a public surface.
  Generic parameters appear only in `Result<'T, 'E>` and genuinely generic
  combinators.

- **Error DUs carry diagnostic payload.** Each `*Error` case carries an elevated
  id, a typed state, or a `reason : string` — never a secret. A bare error case is
  useless in a log.

The compiler is the enforcement mechanism: if a public signature type-checks, it
is guaranteed to carry only elevated types.

---

## Model IO as functional proxies (keep logic pure)

IO is modeled as **pure data describing behavior**, so the logic that consumes it
stays referentially transparent and unit-testable without real IO.

- A **`*Proxy`** is a record whose fields are pre-curried, `Result`-returning
  functions that bake in the actual IO (file access, persistence, external
  calls). It is constructed once, at the composition root, via a
  `static member create` that captures the real resources into each field.
- A **`*Context`** bundles a proxy with its data and any injected providers (e.g.
  a clock), and is what the logic actually receives.
- After construction the IO is invisible: logic holds the proxy, never a live
  connection or file handle. **A test substitutes in-memory stub functions** for
  the proxy fields and exercises the exact same logic.
- Inject ambient effects (time, randomness) as provider records rather than
  calling `DateTime.UtcNow` / `Random` directly, so behavior is deterministic
  under test.

Use the shared naming vocabulary for these shapes: `Param` (inputs), `Data`
(domain data), `Info` (metadata), `State` (runtime state), `Context` (data +
functions), `Proxy` (remote/IO boundary), `Provider` (resource supplier),
`Delegate`/`Generator` (a record of functions), `Factory` (dynamic creation).
Collections take plural names, not a suffix. Encode optionality in the name
(`tryFind…`) or a `…Opt` field, not a `bool` flag.

This is the target shape when you introduce IO (notably in
`OpticalConstructor.Storage`); the numerical core is already pure and should stay
that way.

---

## UI — Optical Constructor

The app is **Avalonia FuncUI with Elmish MVU**. WinForms is the alternative
desktop stack under consideration (Avalonia may be dropped); write UI guidance
and automation that applies to **both**, and never assume Avalonia is permanent.
(There is no Kotlin / Compose anywhere in this repo — never introduce it.)

**State is separable from the UI.** The Elmish model *is* the state; views are a
pure projection of it. Behavior must be expressible and testable without a window
present — push logic into `OpticalConstructor.Domain` / `.Storage` /
`.Optimization` and test it there, not through the UI.

**Automate by meaning, not by pixels.** Priority order for any UI action:
semantic command by stable ID → native accessibility/control pattern → click the
element's bounding-rectangle center → relative coordinates inside a *named*
canvas → screenshots/absolute coordinates only as a forced last resort. A control
that cannot be found is a missing automation contract — fix the control, do not
click harder.

- **Stable IDs that describe intent, not layout** (`ApplyChangesButton`, not
  `Button1` / `TopRightButton`). IDs must survive restyling.
  - *Avalonia:* set `AutomationProperties.AutomationId` and
    `AutomationProperties.Name` on every interactive/test-relevant control.
  - *WinForms:* set a stable `Name` plus `AccessibleName` / `AccessibleRole`.
- **Centralize IDs** in one F# constants module (`[<Literal>]` strings); never
  scatter duplicate ID strings through the codebase.
- **Three test layers, in this order:** (1) domain/model unit tests for behavior;
  (2) `Avalonia.Headless` tests for UI structure, input, layout, and rendered
  frames (the `ui-smoke` gate opens the host and renders one frame per view);
  (3) external Windows UI-Automation (FlaUI / Appium) smoke tests only where a
  packaged-app path genuinely needs them. Do not start with desktop mouse driving.
- **Wait for deterministic conditions** (visible / enabled / text / render-idle),
  never a fixed `sleep`.

---

## Working in this repo

This is a spec-driven, gated repository.

- **Specs** live under `specs/<id>/`; each spec's gates are declared under
  `.gates/`. Work is implemented to satisfy those gates (build, unit-tests,
  constructor-unit-tests, ui-smoke, ui-tests). First gate failure short-circuits;
  `count_at_least` test gates must not regress the baseline count.
- Implementation is driven by an **arc-runner** (see `.user-md/`), not by a
  watching human. If a slice is ambiguous, pick the interpretation most
  consistent with the surrounding code and record the choice — do not block on a
  prompt that waits for user input.
- Route persistent worker output only into the per-arc `.artifacts/` folder,
  never `%TEMP%` / `%LOCALAPPDATA%`.

---

## Gotchas

- **Line endings: LF.** `.gitattributes` stores all text as LF and `core.autocrlf`
  is `false`. After editing, verify you did not introduce CRLF churn
  (`git diff --numstat` vs `--ignore-cr-at-eol`) and convert back to LF if it
  appears.
- **x64 only.** The solution declares x64 as its single platform; the root
  `Directory.Build.props` defaults `Platform` to x64 so `dotnet test --no-build`
  resolves the same `bin\x64\Release\` tree the build produces. Do not add
  `AnyCPU`.
- **`Directory.Build.props` / `.targets` live at the repository root**, not in
  `Berreman/`, so they also govern the vendored `MathNetNumerics/` projects. Never
  add a nested `Directory.Build.props` — MSBuild imports only the nearest one.
- **One ray law for every element.** The Main scene's beam tree (main beam,
  reflected side beams, the drawn segments, the light paths the experiment
  captures are derived from) is `OpticalConstructor.Domain/BeamLayout.fs`; every
  element is crossed by `RayModel.crossElement` (front face → Snell inside → exits
  parallel; reflections leave the front face) at its DRAWN orientation and depth.
  Never special-case an element kind in the drawing or the snapping. An OPEN ray
  (nothing on it, or no detector at its end) stops at the first drawn element
  box it meets. Nothing else may be drawn in the ray colour: the N1 normal marker
  has its own (`ElementRenderer.normalAxisColor`), and the Main view shows the
  orientation markers on the selected element only. A detector terminates its
  beam wherever it sits; elements rotate about their centre or, with the global
  `RotationPivot` option, about the middle of their incident face.
- **Locked R-only belongs to mirrors alone** (flat / curved mirror kinds and the
  `Mirror` sample geometry); lenses are T-only; everything else carries R and T.
- **Only the light source is placed freely.** It moves in table x / y / z (`Placement.height`,
  `moveLocks`, `Mobility.MovesFreely`), unbounded, and starts with every move and rotation locked;
  the beam root is its 3-D centre, so moving it carries the whole beam. Every other element is
  held to its RAY — one rule, the table plays no part: a ray starts at the element it leaves (the
  source, or the host of a reflected ray) and ends at its first detector, or 1 m along
  (`openRayEndX`) with none; a detector moves anywhere past the start, which extends the ray
  (`raySpan` / `alongBeamRange`). The Main Rotation bay uses `RotationControls.wrappingView`; the
  rotation test windows keep the single-row `view`, because their headless tests click fixed
  window coordinates.
- **The Main scene's Library is LIVE** (`Library.liveLibrary`: the seeded presets over the samples
  and user-sources stores, composed in `AppContext.create`), so a sample or source created in the
  Library window binds AND resolves on the table. User light sources live in `SourceProxy`
  (edited by `LightSourceEditorView` / `LightSourceEditorWindow`); the seeded lasers and UV / EUV
  lithography sources (`SeedSources`) are protected presets. A source is its wavelength.
- **Never touch `*.nb` files.** Mathematica notebooks are treated as pure binary
  (no EOL, no diff, no merge). Leave them alone.
- **Vendored MathNet.Numerics** is built from source under `MathNetNumerics/`;
  reference the projects, not the NuGet.
- Scratch/working folders (`Temp/`, `!Temp/`, `specs_copy/`, `data/`) are not the
  product — don't build product code there.
