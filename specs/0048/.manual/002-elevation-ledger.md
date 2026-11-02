# 002 — The ELEV ledger: what the elevation analyzers find in Berreman, and the order to fix it

This is the ledger behind `Elevation/elevation-known-findings.json`. How the gates were set up is in `001` §3.

## 1. The measurement

- **Command:** `Elevation/Run-ElevationGate.ps1 -All -ExcludeRules ELEV004`, the `elevation-full` gate.
- **When:** 2026-11-01, on branch `0048` (`92c11f7` plus this round's changes, none to product code).
- **Analyzers:** `Softellect.Analyzers` 10.1.402.136 (STL's current slow ring), run by `fsharp-analyzers` 0.41.0.
- **Scope:** the 13 F# product projects of `Berreman.slnx`; 240 s at throttle 4.
- **Result:** **3,757 enforced findings**, recorded as 3,661 entries (one entry per rule, file and message, with a
  count). There are 222 more under ELEV004, which no gate enforces. No third-party (`T`) finding was reported.

The scoped `elevation` gate, run as the arc runs it, passes with all 3,757 recorded: 0 new findings, 268 s.

## 2. By rule

| Rule | Findings | What it reports | The usual fix |
|---|---:|---|---|
| ELEV003 | 1,005 | a bare primitive in a public signature | elevate the parameter or result; mark a genuine IO seam `[<ElevationBoundary>]` |
| ELEV003P | 458 | the same, on a private signature | the same, in one file |
| ELEV008 | 472 | a bare primitive record field | elevate the field's type |
| ELEV010 | 444 | primitives beside our own types in one signature | elevate the primitives (no boundary claim exempts this) |
| ELEV010P | 248 | the same, private | the same |
| ELEV015 | 312 | a public module-level name declared twice (`view` 26, `init` 15, `update` 15, `readout` 11, …) | rename to carry its domain (`rotationControlsView`) |
| ELEV013 | 236 | a free function of one of our types | make it a member of that type |
| ELEV013P | 144 | the same, private | the same |
| ELEV016 | 167 | a DU case name used by two unions | rename the higher one's case (two-word names) |
| ELEV011 | 102 | a tuple wider than three, or with a repeated element type | a record |
| ELEV014 | 81 | a type name declared twice (`Model` 14, `Msg` 14, `State` 12, `Handlers` 10, …) | rename the higher one |
| ELEV020 | 75 | a case of a `*Error` union not ending in `Err` / `Exn` | rename the case |
| ELEV009 | 11 | a primitive on a `Result` happy path | elevate the `Ok` payload |
| ELEV012 / 012P | 1 / 1 | a two-case union mapped to `bool` | delete the mapping; match on the union |
| **Total enforced** | **3,757** | | |
| ELEV004 (not gated) | 222 | `.value` outside the IO boundary | by hand, after the rest (§6 R7) |

Every duplicate-name finding (ELEV014/015/016) is a collision inside Berreman. None is against `Softellect.Sys`.

## 3. By project, in dependency order (lowest first)

`→` names what a project references. A step that touches a project also analyzes everything that references it.

| Project | → | Total | 003 | 003P | 008 | 010 | 010P | 013 | 013P | 011 | 014 | 015 | 016 | 020 | other | ELEV004 |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|---:|
| OpticalModel | — | 1 | 1 | | | | | | | | | | | | | |
| Berreman (Core) | MathNet | 106 | 31 | 4 | 14 | 42 | | 3 | 1 | 7 | | 4 | | | | 14 |
| OpticalProperties | Core | 8 | 1 | | | 7 | | | | | | | | | | |
| Analytics | OpticalProperties | 148 | 41 | 2 | 5 | 59 | 1 | 11 | 1 | 28 | | | | | | 8 |
| Controls | — | 791 | 250 | 89 | 153 | 46 | 17 | 15 | 2 | 11 | 29 | 173 | 6 | | | |
| Domain | Core, Analytics | 753 | 209 | 58 | 143 | 99 | 39 | 85 | 17 | 26 | 1 | 8 | 11 | 51 | 009 ×4, 012 ×1, 012P ×1 | 95 |
| Optimization | Domain | 128 | 41 | 14 | 36 | 22 | 6 | 2 | 4 | 1 | | | | 2 | | 1 |
| Storage | Domain | 149 | 32 | 34 | 3 | 25 | 25 | 2 | 1 | 6 | | 5 | 1 | 12 | 009 ×3 | 6 |
| Seeding | Domain | 2 | | | | | | 1 | | | | | | 1 | | |
| Ui | Domain, Storage, Optimization, Controls, Analytics | 1,252 | 334 | 180 | 111 | 139 | 108 | 107 | 85 | 22 | 25 | 75 | 55 | 9 | 009 ×2 | 93 |
| TestWindows | Domain, Controls, Ui | 403 | 63 | 74 | 7 | 5 | 47 | 10 | 31 | 1 | 26 | 45 | 94 | | | |
| TestWindows.App | TestWindows | 1 | 1 | | | | | | | | | | | | | |
| App | Ui | 15 | 1 | 3 | | | 5 | | 2 | | | 2 | | | 009 ×2 | 5 |
| **All** | | **3,757** | 1,005 | 458 | 472 | 444 | 248 | 236 | 144 | 102 | 81 | 312 | 167 | 75 | 13 | 222 |

**The heaviest files** (enforced findings):

| File | Findings |
|---|---:|
| `OpticalConstructor.Ui/TableAndElementRotationView.fs` | 298 |
| `OpticalConstructor.Controls/UiIds.fs` | 218 (ELEV015 148, ELEV003 70) |
| `OpticalConstructor.Ui/LibraryWindowView.fs` | 122 |
| `OpticalConstructor.Controls/ExperimentChart.fs` | 117 |
| `OpticalConstructor.Ui/SampleEditorView.fs` | 115 |
| `OpticalConstructor.Ui/MaterialEditorView.fs` | 108 |
| `OpticalConstructor.Domain/DispersionModels.fs` | 106 |
| `OpticalConstructor.TestWindows/ElementRotationView.fs` | 98 |
| `OpticalConstructor.Controls/ExperimentControls.fs` | 95 |
| `OpticalConstructor.TestWindows/SnapToReflectedView.fs` | 91 |
| `OpticalConstructor.Domain/MuellerReconstruction.fs` | 90 |
| `OpticalConstructor.TestWindows/TableRotationView.fs` | 74 |
| `OpticalConstructor.Controls/RendererControls.fs` | 64 |
| `Analytics/Variables.fs` | 63 |
| `OpticalConstructor.Domain/ElementId.fs` | 58 |

There are 138 files with findings. The per-file list is the known-findings file itself.

## 4. How a fix lands

1. Fix the finding in the code.
2. **In the same step, delete its entry** from `Elevation/elevation-known-findings.json`, or lower its count. A recorded
   finding that is no longer found is stale and fails the `elevation` gate on any run that analyzed its file.
3. Never add an entry, and never re-record the list. `specs/0048/.manual/tools/record_known_findings.py` recorded it
   once, when the gates were set up; it is provenance, not a tool for a step.
4. A rename of a type, a DU case or a public name across projects runs `elevation-full`. It is the one run that judges
   every entry.
5. The numbers do not change. `BerremanTests` and the constructor suites stay green and keep their counts. An
   elevation fix changes types, never the physics.

**Code that `020` deletes is not cleaned first.** Its entries go in the step that deletes it, because a deleted file's
entries are stale too. As measured:
- `Ui/SolverHandoffView.fs` 27;
- the code seeds in `Domain/ElementId.fs` (58 in the file, not all in the seeds) and `Domain/MaterialLibrary.fs` 34;
- the `.ocproj` stack: `Storage/ProjectFile.fs` 4 and `Storage/ProjectJson.fs` 4;
- the in-memory seams: `Domain/Scene.fs` 3, `Domain/ExperimentCollectionStore.fs` 1, `Domain/SampleStore.fs` 3,
  `Domain/MaterialStore.fs` 1 and `Domain/Lifecycle.fs` 1;
- `Storage/MuellerDataStore.fs` 8 (the `MuellerDataSet` side), `Storage/RecentFiles.fs` 6 (extended, not deleted) and
  `Seeding/Seeding.fs` 2.

## 5. Work packages, in the order to do them

Each package is bottom-up, so a package's renames never wait on a higher one. The section sizes follow the section
planner's rule of at most about forty steps.

| # | Package | Findings | Notes |
|---|---|---:|---|
| E1 | OpticalModel, Berreman (Core), OpticalProperties | 115 | The solver. Physical quantities get their own types (R3); the MathNet seam is the numeric boundary. Core is a shipped NuGet package, so its public API changes are breaking for its consumers (`BerremanRunner` is not in the solution). |
| E2 | Analytics | 148 | `Variables.fs`, `AnalysisFunctions.fs`, `Colorimetry.fs`. The `fsx-acceptance` tests drive these scripts, so they must keep passing. |
| E3 | Controls | 791 | UiIds (R1); the per-control `State` / `Handlers` / `view` names (R2); `ExperimentChart.fs`, `ExperimentControls.fs`, `RendererControls.fs`, `FacetedTreeControls.fs`. |
| E4 | Domain | 753 | ELEV020 renames (51) first, then `DispersionModels.fs`, `MuellerReconstruction.fs`, `MaterialComplexityEditor.fs` and the editors. It skips what 020 deletes (§4). |
| E5 | Optimization, Storage, Seeding | 279 | Storage's ELEV003P and ELEV010P sit mostly at the real IO seam, so this is where `[<ElevationBoundary>]` is used most. |
| E6 | Ui | 1,252 | `TableAndElementRotationView.fs` alone is 298. The Elmish `Model` / `Msg` / `init` / `update` / `view` names follow R2. |
| E7 | TestWindows, TestWindows.App | 404 | R5. |
| E8 | App | 15 | `AppConfig.fs` parses configuration, so its parse results get elevated (ELEV009). |

Estimated size: 3,757 findings, most of them mechanical. Grouped by file, that is roughly 150–250 steps, which is four
to six sections.

## 6. Rulings this ledger needs (operator)

Each item has a proposed default, which the spec takes unless the operator rules otherwise.

- **R1 — UiIds** (218 findings).
  - The automation ids live in one `[<RequireQualifiedAccess>]` module with a nested module per surface
    (`UiIds.Category.saveButton`). This is CLAUDE.md's "centralize IDs" rule as spec 0038 step 044 implemented it.
  - STL's ruling replaces qualified access with unique names, so `saveButton` in two sub-modules is ELEV015.
  - **Default:**
    - keep ONE module, drop `RequireQualifiedAccess` and the nesting;
    - give every id a unique name that carries its surface (`categoryRowSaveButton`); the string values stay
      byte-for-byte, so no test or automation script changes;
    - mark the module `[<ElevationBoundary>]`, because an automation id is the string the platform's automation API
      reads.
- **R2 — Elmish per-module names** (ELEV014 `Model` / `Msg` / `State` / `Handlers` ×50; ELEV015 `view` / `init` /
  `update` and friends, about 60).
  - **Default:** rename per surface (`RotationControlsState`, `rotationControlsView`), as STL's rule says. Nothing
    authorizes a duplicate.
- **R3 — floats in the solver's public API.** Core, Analytics and OpticalProperties return and take `float` and
  `int` (for example `degreeOfPolarization : Solution -> float`).
  - **Default:**
    - a quantity with a physical meaning gets its own type, as `Angle` and `WaveLength` already do;
    - a type that holds primitives on purpose is `[<NumericCarrier>]` (the MathNet vectors and matrices);
    - the MathNet adapter (`MathNetNumericsMath`) is the numeric boundary.
- **R4 — `Softellect.Sys` for the attributes.**
  - `[<ElevationBoundary>]` and `[<NumericCarrier>]` are `Softellect.Sys` types, matched by name. Version 10.1.402.120
    has both.
  - Core, Analytics, OpticalProperties, Domain, Controls and Ui do not reference `Softellect.Sys` today; App,
    Optimization and Storage do.
  - **Default:** add the package reference where an attribute is used. It is a new dependency of the shipped
    `Softellect.Berreman.Core` package.
  - The alternative is to declare no boundary in those projects and elevate everything there.
- **R5 — TestWindows / TestWindows.App** (404 findings). These are the diagnostic launcher and its test windows.
  - STL keeps test-helper and CLI heads out of the discipline until the libraries are green (STL 0193 round 048).
  - **Default:** keep them in scope and clean them last (E7).
  - The alternative is an `inScopePatterns` exclusion in `.elevation-analyzers.json`, which re-records nothing: their
    entries are deleted with the exclusion.
- **R6 — information only: STL's own docs disagree on DU tags.**
  - STL's `AGENTS.md` still says a DU's wire tag is its case name, `member this.value : string` (STL 0193 round 056).
  - ELEV017 enforces round 058: a closed set's `value` is an integer.
  - Berreman's `AGENTS.md` follows the analyzer.
- **R7 — ELEV004** (222 findings, not gated): as in STL, it stays outside the gate command. **Default:** not part of
  this ledger's packages; a by-hand round after E8.
