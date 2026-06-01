# 002 — impl-log: Domain foundation (units spine, beam-tree types, project aggregate)

## Progress

- [x] Read system prompt, project prompt, slice spec, gate roster, slice-001 outputs.
- [x] Studied engine symbols to reuse (Constants/Fields/Media/MaterialProperties/Solvers/BerremanMatrix).
- [x] Wrote impl-plan + this log.
- [x] Implemented `Units.fs` (UnitOfMeasure DU + four boundary conversions).
- [x] Implemented `BeamTree.fs` (topology, mirror/branch validation, routing, gradient discretize).
- [x] Implemented `Project.fs` aggregate (`OpticalConstructorProject` + nine `$defs` anchors).
- [x] Wrote the four test files (Units/Gradient/BeamTree/BeamRouting — 17 tests).
- [x] Registered `Project.fs` in Domain fsproj and the four test files in Tests fsproj.
- [x] `build` gate green (exit 0, no lowercase `error`).
- [x] `unit-tests` gate green (Passed 70, baseline held).
- [x] `constructor-unit-tests` gate green (Passed 18, up from baseline 1).
- [x] State-of-the-world written.

## Files modified

New (under `Berreman/OpticalConstructor/`):
- `OpticalConstructor.Domain/Project.fs` — `OpticalConstructorProject` aggregate + `schemaDefAnchors`.
- `OpticalConstructor.Tests/UnitsTests.fs` — AC-A5/A7/D1/D2 + cm⁻¹ + R-8 (7 tests).
- `OpticalConstructor.Tests/GradientDiscretizeTests.fs` — AC-B11 (3 tests).
- `OpticalConstructor.Tests/BeamTreeTests.fs` — AC-A4/B2/B3/B8 (4 tests).
- `OpticalConstructor.Tests/BeamRoutingTests.fs` — AC-A3/B4 (4 tests).

Filled (slice-001 placeholders → real content):
- `OpticalConstructor.Domain/Units.fs` — units spine.
- `OpticalConstructor.Domain/BeamTree.fs` — beam-tree topology + routing + gradient discretize.

Edited:
- `OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` — inserted `Project.fs` after `BeamTree.fs` (Units → BeamTree → Project → placeholders).
- `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — registered the four new test files after `ScaffoldTests.fs`.

NOT touched: any engine `.fs` (Berreman/OpticalProperties/Analytics), `BerremanTests/**`, `.gates/**`, the other Domain placeholder modules (`CurvedElements.fs`, `SourceSpec.fs`, …), Storage/Optimization/Ui/App projects.

## Decisions

- **`BeamNode.incident : IncidentLightInfo`** — reuse the engine local-beam-state type (`Fields.fs:338`) instead of inventing a beam-state record (§A.3 reuse-before-invention).
- **`children : Map<BeamBranch, BeamNode>`** — the immutable two-slot keying §B.2/R-10 calls for; `BeamBranch` is a plain comparable DU so only the key needs comparison (the engine payloads on the node need none).
- **`attach` is a static member `BeamNode.attach branch child parent : Result<_, BeamTreeError>`** — matches the spec's literal signature (§B.3); validation via `Result`, never exceptions.
- **`BeamTreeError = MirrorBranchMustBeReflected`** — single NET-NEW case traced to §B.3/R-11; kept minimal.
- **`BeamNode.system : OpticalSystem` AND `ConstructorElement.Sample of OpticalSystem`** — the spec mandates both (R-10 "BeamNode carries its OpticalSystem"; §A.4 "Sample (carrying an OpticalSystem)"). For a Sample node they coincide; `BeamNode.system` is the stack the solver consumes. Recorded under Gotchas in the SoW.
- **`discretize` guards `subLayerCount >= 1` with `invalidArg`** — the shared Part J §J.9 validation seam is not in this slice; an inline precondition keeps the pure function honest without pulling in later scope.
- **`evNmProduct = 1239.84` bound once** in `Units.fs`; all eV/cm⁻¹/length math routes through `Constants.fs` factors — no re-derived literal, no shadow `[<Measure>]`.

## Testing state

All four roster gates pass locally (Release/x64):

- `build` — `dotnet build Berreman.slnx -c Release -nologo -v:m` from `Berreman/`: exit 0, "Build succeeded.", 0 Error(s); no lowercase `error` token. Log `.artifacts/002-build.log`.
- `unit-tests` — `dotnet test --no-build -c Release` from `Berreman/BerremanTests/`: exit 0, Failed 0 / Passed 70 / Skipped 5 / Total 75. `BerremanTests` untouched; baseline `berreman_unit_tests = 70` held. Log `.artifacts/002-unit-tests.log`.
- `constructor-unit-tests` — `dotnet test …/OpticalConstructor.Tests.fsproj -c Release`: exit 0, Passed 18 / Total 18 (1 scaffold + 17 new). Up from slice-001 baseline of 1. Log `.artifacts/002-constructor-unit-tests.log`.

Remaining warnings are benign/pre-existing: NU1902 (log4net transitive from Softellect), MSB3277 WindowsBase conflict from WebView2's WPF assembly in the UI-referencing projects, SYSLIB0051 in the cloned Numerics C#. None is an `error`; the build gate stays green.

## Artifacts

- `.artifacts/002-build.log`, `.artifacts/002-unit-tests.log`, `.artifacts/002-constructor-unit-tests.log`.

## Gotchas

- **`EmField.propagate` is an optional type extension in `module BerremanMatrix`** (BerremanMatrix.fs:191) — callers MUST `open Berreman.BerremanMatrix`.
- **`Berreman.Fields.RT` shares `Reflected`/`Transmitted` names with `BeamBranch`** — tests qualify `BeamBranch.Reflected`/`.Transmitted`; in-module use resolves to the local `BeamBranch`.
- **No reliable structural equality on Math.NET-backed engine records** — routing tests assert on `emf.e.value.norm`, never whole-record `=`.
- **`--warnaserror+:25`** — every new DU match is exhaustive (verified by the green build).
