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

# Tests run from each test project's own folder, over what the build produced
cd Berreman/BerremanTests && dotnet test --no-build -c Release                        # solver
cd Berreman/OpticalConstructor/OpticalConstructor.Tests && dotnet test --no-build -c Release
cd Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests && dotnet test --no-build -c Release --filter-trait "Category=ui-smoke"

# One class, or one test with all its theory rows (xUnit v3 filters, never VSTest's --filter)
dotnet test --no-build -c Release --filter-method "BerremanTests.SolverTests.*" --filter-method "BerremanTests.SolverTests"

# The ELEV analyzers (repo root, after the build; all projects without a base)
powershell -NoProfile -ExecutionPolicy Bypass -File Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004
```

Test stack: **xUnit v3 on Microsoft.Testing.Platform**, **FsCheck**
(property-based, run through `Check.One` inside a `[<Fact>]`),
**FluentAssertions**, and **Avalonia.Headless** for UI tests. Run the relevant
tests after every change; if your local build/tests are green, the matching
gate is green too. `BerremanTests` takes about 52 minutes whole (51 min 44 s
measured 2026-11-03; the inverse-fit classes alone run up to 997 s each, spec 0044
`.manual/020`), so run the classes you touched; the two constructor suites take
under a minute, `ui-tests` about three. No test, fixture or kit lives under
`Documents`; test kits and databases go under the test run's output directory.

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

**Tests run on Microsoft.Testing.Platform (MTP), and nothing VSTest is left**
(spec 0048 `.manual/001`, as STL `specs/0239/.manual/005`). The root
`global.json` sets `"test": {"runner": "Microsoft.Testing.Platform"}`. A test
project is `OutputType Exe` with `IsTestProject` and
`UseMicrosoftTestingPlatformRunner` true, and references `xunit.v3.mtp-v2` and
`Microsoft.Testing.Extensions.TrxReport` (xUnit v3 generates the entry point).
Never add back `Microsoft.NET.Test.Sdk`, `xunit.runner.visualstudio`,
`coverlet.collector`, an xUnit v2 package (`Xunit.SkippableFact`,
`FsCheck.Xunit`, `xunit.abstractions`) or `open Xunit.Abstractions`; skip at run
time with `Assert.Skip`. Filter with xUnit v3's own options (`--filter-class`,
`--filter-method`, `--filter-trait`, `--filter-not-trait`); a fact must return
`unit` (a generic fact is refused). The gates read the MTP summary
(`Test run summary: Passed!`, `succeeded: N`) and each run's TRX report
(`--report-trx`, under the project's `TestResults/`).

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
Elevation/                             # the ELEV gate driver and its known-findings list (not a project)
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
  meaningfully (`'Item`, `'Config`) — not bare `'a` / `'b`. Annotations are also
  required wherever the value restriction bites (empty collections, plain
  generic values) and around overloaded .NET call resolution.
- **Pure functions by default**; side effects live only at the edges.
- Prefer `async` computation expressions for IO.
- Simplicity and correctness over cleverness.
- Four-space indent. For floating-point comparisons in tests, reuse the
  project's tolerance helpers (see `BerremanTests/MatrixComparison.fs`) — do not
  hand-roll new epsilon logic.
- Naming vocabulary (`Param` / `Data` / `Info` / `State` / `Context` / `Proxy` /
  `Provider` / `Delegate` / `Generator` / `Factory` / …, plural collections,
  `tryFind…` / `…Opt` for optionality): follow `naming_conventions.md` (repo
  root, the same file as STL's) exactly — it is the authoritative naming
  reference.

## Comments: short, precise, useful (STL operator ruling, STL spec 0200 `.manual/015`)

A comment earns its line by saying something **the code cannot**. Say **why**,
never what — a restatement of the next statement is worse than no comment,
because it is one more thing that goes stale.

- **Cite the spec that drove the decision** (`spec 0044 §8.5`,
  `0048 .manual/001`). When behaviour changes, add the newer spec beside the old
  one rather than deleting it, so the *why it changed* survives too.
- **Document a decision once.** If the narrative already lives in
  `specs/<id>/.manual/`, point at it; do not restate it in the file.
- **One to three lines.** A block past ~6 needs a reason to exist; past ~12 it is
  an essay and belongs in the spec. Spend the lines on non-obvious hazards.
- **Delete, do not shorten**, a comment that repeats an adjacent identifier.
- **Doc comments** (`///`, PowerShell comment-based help) are API surface: they
  describe the *contract*, not the implementation's history.
- **No new comments in `.fsproj` / `.csproj`.** The only sanctioned one is a
  genuinely extraordinary MSBuild/toolchain fact the XML cannot express, in one
  line.

This governs what you write and what you touch. It is not a licence to sweep the
tree: the project files still carry comments from earlier specs.

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
  member mapping to the wire/disk integer (never a string, `ELEV017`) and a
  `tryCreate` that maps an unknown stored value to a *typed error*, never a throw.

- **No naked `bool`** in a domain record or signature. Use a two-case
  named-condition DU (`type LockoutState = LockedOut | NotLockedOut`) so a match
  site reads as prose and a third state is a non-breaking addition.

- **Explicit, concrete signatures on every public function.** Spell out the
  argument and return types; do not rely on inference for a public surface.
  Generic parameters appear only in `Result<'T, 'E>` and genuinely generic
  combinators.

- **Error DUs carry diagnostic payload.** Each `*Error` case carries an elevated
  id, a typed state, or a `reason : string` — never a secret. A bare error case is
  useless in a log. A string field on a union named `*Error` is sanctioned (only
  strings, only there); every case of such a union ends in `Err`, or `Exn` when it
  wraps an exception (`ELEV020`).
- **DU case names have two or more words** (`InteractionDeclined`, `AxisLocked`,
  not `Declined`): a one-word case says nothing at the match site and is the first
  to collide under `ELEV016`.
- **Members may speak their type's own primitives.** A member declared ON a type
  (intrinsic or extension) is exempt from the primitive-signature rules; the same
  signature as a FREE module function is a breach. A free function whose only
  parameter is one of our types is a fact about that type and belongs on it as a
  member (`ELEV013`). Logger functions (`log`, `logInfo`, …) keep their free text.
- **Names are unique, not qualified** (STL operator ruling, STL spec 0193 round
  056 and 0219 `.manual/017`). Every type name (`ELEV014`), every public
  module-level name (`ELEV015` — no second free `create` or `view`; name it
  `create<Something>`), every DU case name across all DUs (`ELEV016`) and every
  extension member on one receiver (`ELEV019`) is unique among ours (every
  `Softellect.*` assembly). When two collide, the one in the LOWER assembly keeps
  its name. A third-party namesake (FSharp.Core, the BCL, Avalonia) is a warning
  under the rule's `T` code, never enforced. `[<RequireQualifiedAccess>]` is not
  how a collision is resolved, and nothing authorizes a duplicate among our names:
  rename one (inside one assembly, the writer chooses which). An Elmish surface's
  names carry the surface (`RotationControlsState`, `rotationControlsView`), as
  STL's VNC viewer's do (`VncMainModel`, `mainView`) (spec 0048 `.manual/002` R2).
- **A closed set's wire/disk tag is an integer** (STL spec 0193 round 058,
  enforced as `ELEV017`): a multi-case union's `value` member returns an `int`
  (1-based, 0 left as the unset sentinel), never a string; its name is rendered by
  an honest render member (`.text`, `.describe`). It is read back by `tryOfDb` /
  `tryOfWire`, returning an option or `Result`. No `Unknown*` / `Other` catch-all
  case: an unrecognized stored value is the caller's typed error at the boundary.
- **Elevate at origin, never at the call site.** A value is born as its elevated
  type where it first exists (the `let` that parses it, the literal that defines
  it, the field it is bound into). Wrapping a bare local as you pass it
  (`f (Thickness t)`, `ELEV001`) and the wrap-then-unwrap round trip
  (`(Thickness x.value)`, `ELEV005`) are disallowed. `.value` appears only at the
  one true interop / serialization line (`ELEV004`). This holds for test fixtures
  too.
- **A cross-type reuse is a named member on the SOURCE type** (`one.toObjectTwo`),
  never a re-wrap at the call site (`ObjectTwo one.value`). Declaration order
  decides only the form: towards an earlier type it is an intrinsic member,
  towards a later one a type extension declared after it.
- **A bijection is collapsed, not bridged.** Wanting both `A.toB` and `B.toA`
  means one entity has two names: the LOWER type survives and the other is
  deleted (`ELEV006`).
- **Tuples:** at most three elements, and never two positions of the same type
  (`ELEV011`) — make it a record. A function that maps a two-case union to a
  `bool` is deleted in favour of matching on the union (`ELEV012`).
- **Large numerical data stays fast; everything small is elevated** (operator
  ruling, spec 0048 `.manual/002` R3; STL spec 0219 `.manual/010` and `012`).
  - A type whose fields or generic arguments ARE bulk numbers keeps its fast
    representation and is marked `[<NumericCarrier("<reason>")>]`. Examples: the
    MathNet vectors and matrices and our wrappers over them, FFT buffers, sweep
    and spectrum arrays, measured series, a fit's parameter vector.
  - A call into a third-party numeric library (MathNet, Alglib) is marked on
    its own declaration: `[<ElevationBoundary(nameof InteropBoundary, "<reason>")>]`.
  - A scalar with a meaning, a count, an index or a small tuple is elevated.
  - The test: a value stays a carrier's element only when wrapping it would cost
    a conversion inside a kernel loop or a copy of a large array.
  - A mark goes on the declaration, never on a module.
- **A process entry point** (`main : string[] -> int`) is marked
  `[<ElevationBoundary(nameof IoBoundary, "the process entry point - argv in, exit code out, per the .NET contract")>]`
  (STL spec 0229 `.manual/005` Q5).

The compiler is the enforcement mechanism: if a public signature type-checks, it
is guaranteed to carry only elevated types. The ELEV analyzers enforce the rest.

## The ELEV analyzers (spec 0048 `.manual/001`)

The rules above are checked by STL's **`Softellect.Analyzers`** (the ELEV rules),
consumed as a package: `Elevation/Run-ElevationGate.ps1` loads the version the
root `Directory.Build.targets` pins into `fsharp-analyzers` (installed at the
`FSharp.Analyzers.SDK` version that package depends on) and runs it over the F#
product projects of `Berreman.slnx`, the TestWindows heads and the App
included. The test projects and the vendored MathNetNumerics are out of scope.
What the rules apply to is the root `.elevation-analyzers.json`; edit that file,
never the analyzers. **Where any document, STL's included, disagrees with the
analyzers, the analyzers win** (operator, spec 0048 `.manual/002` R6).

| Code | Rule | In one line |
|---|---|---|
| ELEV001 | NoCallSiteRewrap | no wrapping a bare local as you pass it |
| ELEV002 | NoRawDuRender | render a DU through its own member, never `%A` / `%O` / `%s` |
| ELEV003 | NoPrimitivesOutsideBoundary | no bare primitive in a signature outside a marked boundary |
| ELEV004 | DotValueAtBoundaryOnly | `.value` only at the IO boundary (measured, not gated: R7) |
| ELEV005 | NoWrapUnwrapRoundTrip | no `(T x.value)` |
| ELEV006 | NoBidirectionalConversion | `A.toB` with `B.toA` is one entity: collapse it |
| ELEV007 | NoBoxIntoObjSink | no elevated value boxed into an `obj` sink |
| ELEV008 | NoPrimitiveInRecord | no bare primitive record field |
| ELEV009 | NoPrimitiveOnResultHappyPath | the `Ok` case is elevated |
| ELEV010 | NoPrimitiveBesideElevated | a signature naming our types elevates its primitives too |
| ELEV011 | NoWideTuple | tuples of at most three, no repeated element type |
| ELEV012 | NoTwoCaseDuToBool | no two-case union mapped to `bool` |
| ELEV013 | NoSingleParameterFunction | a function of one of our types belongs on it |
| ELEV014 / 015 / 016 / 019 | NoDuplicate… | unique type, public, DU-case and extension-member names |
| ELEV017 | NoStringTagOnClosedSet | a multi-case union's `value` tag is an `int`, never a string |
| ELEV018 | GuidNameMustBeGuid | a name that says Guid holds a Guid |
| ELEV020 | ErrorCaseMustEndInErrOrExn | `*Error` cases end in `Err` / `Exn` |
| ELEV021–024 | the wire guard | no plain bytes on a wire outside a carrier |

A `P` suffix (`ELEV003P`) is the same rule on a private signature; a `T` suffix is
a third-party namesake, never enforced. A genuine seam is marked on its own
declaration with `[<ElevationBoundary(nameof IoBoundary, "<reason>")>]` (or
`InteropBoundary`, `StorageBoundary`, `ContractBoundary`), and a type that holds
bulk numbers on purpose with `[<NumericCarrier("<reason>")>]`. Both are
`Softellect.Sys` types, matched by name.

The gates are `elevation` (changes since ASG's immutable arc-start commit, plus
uncommitted changes and dependents) and `elevation-full` (every project).
Standalone elevation without `-Base` analyzes every project; ASG supplies its
origin through `ASG_ARC_BASE_COMMIT` (spec 0050 `.manual/013`).
**ELEV004 is excluded in the gate command,
not in the rules**, as in STL. **The tree is ELEV-clean** (spec 0048 closed the
cleanup): `Elevation/elevation-known-findings.json` lists no finding, so every
finding fails the gate. **Never add an entry, never re-record the list, and never
edit `.elevation-analyzers.json` or the analyzers to silence a finding.**

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
- **Centralize IDs on one elevated id type** (spec 0048 `.manual/002` R1; STL's
  `Vnc/ViewerCore/VncViewerUiIds.fs`):
  - a single-case union over the string with `.value`;
  - every fixed id is a uniquely named `static member` of it;
  - a repeated control's id is a static member over an elevated parameter;
  - the control receives it through one member on the type.

  Never scatter id strings through the codebase, and never use a module of
  `[<Literal>]`s or `[<RequireQualifiedAccess>]`.

  Here the type is `ConstructorUiId` (`OpticalConstructor.Controls/UiIds.fs`). An
  id derived from a Domain value is an extension member of `ConstructorUiId` in
  the project that owns the value's type
  (`OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`).
- **Every user action is a semantic Elmish message.** The button and the test
  send the same message, so tests drive by meaning, never by pixels.
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
  constructor-unit-tests, ui-smoke, ui-tests, elevation, elevation-full). First
  gate failure short-circuits; `count_at_least` test gates must not regress the
  baseline count.
- **Specs are written under the ASG constitution**
  (`C:\GitHub\AI-Strategy-Generator\.constitution-md`): a spec opens with its
  wiring contract, names its minimum scope and its explicit negative scope,
  enumerates what it reuses before it invents, names its verification surface,
  and states the migration intent of every durable shape it changes. During
  implementation, a non-trivial deviation from the spec is recorded when it is
  made, with its reason; the working code is what is.
- Implementation is driven by an **arc-runner** (see `.user-md/`), not by a
  watching human. If a slice is ambiguous, pick the interpretation most
  consistent with the surrounding code and record the choice — do not block on a
  prompt that waits for user input.
- Route persistent worker output only into the per-arc `.artifacts/` folder,
  never `%TEMP%` / `%LOCALAPPDATA%`.
- **No legacy, no migration** (there is no release yet). Superseded code is
  deleted in the change that replaces it. A stored format whose version differs
  from the app's is a typed error: no schema up-converters, no EF Core migrations
  (`EnsureCreated` plus a schema stamp), no compatibility shims, no fallback
  readers, no "keep the old path as an option".
- **Reuse, never copy.** A reuse is a move or a call: the change that adds code
  at its new home deletes it at the old one. A duplicated function body, a
  re-transcribed constant or a second parser for one format is a defect.
- **Nothing is deferred** (operator, 2026-11-01). A step finishes what it is
  asked for: no stub, no TODO, no "later" or "follow-up". Work that cannot be
  finished as written stops with its question.
- **Build machines run without OneDrive** (operator ruling, 2026-11-01). ASG's
  `Disable-OneDrive.ps1` removes it for every account and keeps it out, so
  Documents, Desktop and `%APPDATA%` are on the local disk. No build step, gate,
  test or fixture may depend on a OneDrive folder or work around one. The product
  still meets OneDrive on users' machines; that is the product's concern
  (`specs/0046/.manual/020` §G), not the build's.

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
  `Mirror` sample geometry); lenses AND polarizers are T-only (locked — a splitting polarizer
  would be a new kind); a source emits; samples carry R and T.
- **An experiment runs on the light's real path** (`ExperimentRun` + `chartForRun`): for each
  captured branch the DETECTOR on it decides the quantity (intensity → S₀, ellipsometer → Ψ, Δ),
  and a detector captures the branch by which its light leaves the last object on its path;
  every element on the path acts in light order through its Mueller matrix from `ItemEvaluation`
  (a polarizer at its R1, every sample at its drawn incidence and spin for the branch the path
  leaves it by), and the source emits its own Stokes state times its intensity scale, by default
  unpolarized with the unit scale after the first polarizer. The sample's
  transmitted Mueller matrix is FLUX-normalized (`Propagation.sampleMuellerSpun`) — the engine's
  Mueller matrices are |E|²-based, its T is flux. Only a TUNABLE source (`SourceTuning`) can have
  its wavelength varied, within its limits, or set per experiment (`sourceWaveLengthOpt`).
- **The table is the active setup.** Its elements are the setup's slots (`Setups.fs`), the source
  first; a slot holds a kit item or nothing, and a dial-mounted item records its raw dial reading.
  An experiment references its setup by name and revision, and recording a changed table over a
  revision an experiment references mints the next revision (`Lifecycle.decideVersioning`).
- **Names come from the kit's naming template** (`NamingTemplates.fs`, spec 0050 S8.3): `{source}` is the polarization
  source (Prep 1), `{light}` the light source, `{obj<k>}` / `{phi<k>}` the objects and their angles (a dial reading or
  R1), then `{analyzer}`, `{detector}` and `{note}`; an empty second or later slot of a role, or an empty note, is left
  out with the text before it, and `{source:CPL1=CPL}` writes an item under another name. A new setup takes the
  template's name made free (`setupNameAmong`), one it cannot name keeps `Setup <n>`; the reference pipeline reads the
  lab's labels through `NamingTemplate.standard`, and the grid generator is `SetupGrids.fs`.
- **The constructor is checked against the Analytics example scripts** (`FsxAcceptanceTests`,
  trait `fsx-acceptance`, ~1.5 min): each drives the headless app to build the script's table,
  exports the run through `ExperimentExportProxy`, and compares it with `Variables.calculate`.
  `Variables.getWaveLengthValue` used to wrap metres as nm (every script wavelength sweep ran at
  ~10⁻⁷ nm) — fixed in spec 0046; keep it in the range's own unit.
- **Every window fits its screen and scrolls — nothing is ever clipped out of reach.** Each window's
  constructor calls `WindowFit.keepOnScreen` (shrinks it to its screen's working area on open);
  `MinWidthPanel` lays content out at the window's width but never below a minimum and scrolls sideways
  below it (Main's page, the Material editor). `ScrollExtentTests` is the law (800 × 600 and 1000 × 540:
  every control of every window can be scrolled fully on screen) — add a new window to it. Never put a
  WrapPanel (or a wrapping TextBlock) inside a horizontal StackPanel: it gets an unbounded width and never
  wraps — dock the label in a DockPanel instead. A plain wheel over the Main table scrolls the page until
  the table is fully in view, then zooms it (`wheelTarget`); with a modifier it is always the table's.
- **Main ribbon:** Add (opens first) · Rotate · Move · Render · Select · Experiments · Details. The
  Experiments bay's "Data files" section (Attach data file…) is the Inverse constructor's alone; a
  collected experiment's View opens its chart (there is no separate Open chart button).
- **Only the light source is placed freely.** It moves in table x / y / z (`Placement.height`,
  `moveLocks`, `Mobility.MovesFreely`), unbounded, and starts with every move and rotation locked;
  the beam root is its 3-D centre, so moving it carries the whole beam — an x move included: the
  elements' along-beam x is measured from the source, so its x moves (and its Reset) shift every
  element by the same amount (`carryRayWithRoot`), and the default positions (the 1 m open end,
  Reset points, new elements) are read against it (`alongFromSource`). Every other element is
  held to its RAY — one rule, the table plays no part: a ray starts at the element it leaves (the
  source, or the host of a reflected ray) and ends at its first detector, or 1 m along
  (`openRayEndX`) with none; a detector moves anywhere past the start, which extends the ray
  (`raySpan` / `alongBeamRange`). The Main Rotate bay uses `RotationControls.wrappingView`; the
  rotation test windows keep the single-row `view`, because their headless tests click fixed
  window coordinates.
- **The Main scene's Library is LIVE** (`Library.liveLibrary`: the preset store over the samples
  and user-sources stores). `AppContext.create` checks `Catalogue/Builtin/` and every registered
  catalogue folder on every start through `CatalogueImportContext.importOnStart`, holding changed
  rows for confirmation before import; a sample or source created in the Library window binds AND
  resolves on the table. User light sources live in `SourceProxy` (edited by `LightSourceEditorView` /
  `LightSourceEditorWindow`); the built-in lasers and UV / EUV lithography sources named by `PresetIds`
  are protected presets. A source is its wavelength.
- **Never touch `*.nb` files.** Mathematica notebooks are treated as pure binary
  (no EOL, no diff, no merge). Leave them alone.
- **Vendored MathNet.Numerics** is built from source under `MathNetNumerics/`;
  reference the projects, not the NuGet.
- Scratch/working folders (`Temp/`, `!Temp/`, `specs_copy/`) are not the
  product — don't build product code there.
- `data/` holds the committed reference experiment kits (spec 0050 `.manual/001` §1),
  one subfolder each. Only `data/chroma/` (the web app's knowledge base) and each kit's
  `.oc/` are git-ignored. Tests open those kits read-only and never write into `data/`.
