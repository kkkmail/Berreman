# 002 — The ELEV ledger's plan: the census, the operator's rulings and how a row closes

**Revision 2 (2026-11-01, the operator's answers):**

- The ledger is now `002-elevation-ledger.jsonl`, built from the recorded findings the way STL builds its elevation
  ledgers (§1, §4). This file was the ledger, and is now its plan.
- R1–R7 are answered (§2).
- Nothing is deferred any more. PART A closes every finding, including those in code PART B deletes later, and it ends
  with an empty known-findings list (§2, "No deferrals").

## 1. The ledger and this plan

- **The ledger is `specs/0048/.manual/002-elevation-ledger.jsonl`**, one JSON row per unit of work.
  - Its shape is that of STL's elevation ledgers: `C:\GitHub\Softellect\specs\0229\.manual\loop-queue.jsonl`, and
    0219's and 0220's before it.
  - It is built, not listed. `tools/build_elev_ledger.py` derives it from `Elevation/elevation-known-findings.json`,
    as STL's `specs/0229/.manual/tools/New-ElevLoopQueue.py` derives 0229's from a sweep. The recorded list is that
    sweep here.
- **This file is its plan:** the rulings (§2), the census (§3), the rows (§4), and how a row closes (§5, §6). Where a
  row's own `task` or `done_when` and this file differ, this file binds.
- `021` (spec 0046) puts the ledger first in the spec it prompts: PART A closes the rows in id order.

## 2. The rulings (operator, 2026-11-01)

The operator answered the first draft's seven questions. Each item states its ruling as it binds, with the place in
STL that does the same thing. Where anything is unclear, the STL way is followed: STL is much larger than this
repository and has resolved far more cases.

### R1 — UI automation ids, the way STL does them (`UiIds.fs`, 218 findings, row E006)

The operator: do it the way STL does; we do not use duplicate names in our code, and we do not care about name
matches with third-party code.

- **STL's shape** is `Vnc/ViewerCore/VncViewerUiIds.fs` (STL spec 0206 `.manual/018`), modelled on
  `BirdMessenger/BmbDesktopUi/BmbDesktopUiIds.fs` (STL spec 0197 step 2). The views and the tests share an id, which
  makes it a domain value, and it is elevated like every other one.
- **One single-case union over the string, with `.value`.** For example `ConstructorUiId of string`; the spec picks
  the name, which must be unique. `[<RequireQualifiedAccess>] module UiIds`, its nested modules and its `[<Literal>]`s
  all go (row E006).
- **The three other automation-id modules move onto the same type, each in its own row:**
  - `WorkbenchIds` and `CollectionIds` in `TableAndElementRotationView.fs` (row E036);
  - `LauncherIds` in `OpticalConstructor.App/Program.fs` (row E042).

  `MaterialIds`, `CategoryIds` and `SeedEntryIds` are domain ids, not automation ids, and R1 does not reach them.
- **Every fixed id is a `static member` of that type**, named for its surface (`rotationR1MinusButton`,
  `paletteRemoveSelectedButton`).
  - A member is not a module-level name, so ELEV015 does not reach it, and each name is still unique.
  - The string values stay byte for byte, so no automation script and no test expectation changes.
- **A repeated control's id is a static member over an elevated parameter**, never over a bare `string` or `int`.
  - STL writes `machineRow (machineId : TrustDeviceId)`, and declares `VncUiPosition` (a position in a list) beside
    the ids.
  - Controls references no Domain type. So an id derived from a Domain value is an extension member of the id type,
    declared in the project that owns the value's type (today's `LibraryWindowView.versionRow` / `entryNode`). ELEV019
    keeps those names unique too.
- **The id reaches Avalonia through one member on the id type**, in the Avalonia-facing project.
  - STL's is `VncUiId.automationAttribute`, an extension in `Vnc/Viewer/VncViewerControls.fs`.
  - Its `.value` there is the one interop line. That is ELEV004, which is measured and not gated.
- **The first draft's default is withdrawn:** one flat module of uniquely named literals, marked
  `[<ElevationBoundary>]`. STL marks no id module.
- **A test that cites an id in `InlineData`** takes it from the type instead (`MemberData` / `TheoryData`), because a
  member is not a literal.

### R2 — Elmish and per-control names (ELEV014, ELEV015, ELEV016)

The findings are the ELEV014 `Model` / `Msg` / `State` / `Handlers`, the ELEV015 `view` / `init` / `update` /
`readout` and the rest, and the ELEV016 case names.

- **Nothing authorizes a duplicate among our names**, and a match with a third-party name is never a reason to rename.
  This is STL spec 0219 `.manual/017`, points 1 and 2: a third-party match is filed under the rule's `T` code, and no
  gate enforces it.
- **When two of ours collide, the declaration in the LOWER assembly keeps its name** and the higher one is renamed.
  Inside one assembly the step chooses which, and says so in the row note.
- **STL's VNC viewer is the pattern for an Elmish surface.**
  - Its types carry the surface: `VncMainModel`, `VncMainMsg`, `VncFileManagerModel`.
  - So do its functions: `mainView`, `fileManagerView`, `initTerminal`, `initialMainModel`.
  - Here that gives `RotationControlsState`, `RotationControlsHandlers`, `rotationControlsView`, `TableRotationModel`,
    `initTableRotation`.
- **No `[<RequireQualifiedAccess>]`, no attribute, no exclusion.** No analyzer reads `AuthorizedDuplicateNameAttribute`
  any more (STL `Sys/Elevation.fs`).

### R3 — Numbers: large data stays fast, small values are elevated

The operator: "Anything that is a LARGE numerical data, especially if it is an interop with some third-party numerical
libraries MUST keep the data structure(s) optimized for speed. But anything that is small enough must be elevated."

- **STL's precedent is `Softellect.Math`.**
  - STL spec 0219 `.manual/010` §1 gives the reason: the doubles inside a sparse matrix ARE the numbers, and naming
    each one costs a conversion on every value a kernel moves.
  - `.manual/012` withdrew `010`'s kernel boundary and made being a carrier "a property of the TYPE, not an exception on
    the code that uses it".
- **Kept fast, and marked `[<NumericCarrier("<reason>")>]` on the type:** a type whose fields or generic arguments are
  bulk numbers.
  - Here that means:
    - the MathNet vectors and matrices, and the solver's wrappers over them;
    - the FFT buffers;
    - the sweep, spectrum and profile arrays;
    - measured data series;
    - the parameter vectors handed to Alglib.
  - A carrier's own fields and its generic arguments stop counting as primitives.
  - Its members are still judged like any other function, and every other primitive in a signature that names it is
    still reported.
- **A call into a third-party numeric library** (MathNet, Alglib) is marked on its own declaration:
  `[<ElevationBoundary(nameof InteropBoundary, "<reason>")>]`.
- **Everything else is elevated:**
  - a scalar with a physical meaning: `degreeOfPolarization : Solution -> float` returns a `DegreeOfPolarization`;
  - an index, a count, a tolerance;
  - a small tuple, which ELEV011 makes a record.

  The solver already does this with `Angle`, `WaveLength` and `RefractionIndex`.
- **The test** for a value: it stays a carrier's element when wrapping it would cost a conversion inside a kernel loop,
  or a copy of a large array. Otherwise it is elevated. Every carrier and every mark goes in the row note with its
  reason.
- **Never a module-level mark.** STL 0219 `.manual/012`: a declaration whose primitive cannot be elevated gets a mark
  "on THAT declaration - never at module level".
- **The numbers do not change.** An elevation changes types, never values (§5).

### R4 — `Softellect.Sys` at STL's current slow ring, 10.1.402.136

The operator: STL is on 136 now, not on 120.

- `[<ElevationBoundary>]` and `[<NumericCarrier>]` are `Softellect.Sys` types (`Sys/Elevation.fs`), matched by name.
  10.1.402.136 is the version in STL's `Sys/BuildInfo.ps1`, and it has been in the local feed since 2026-10-28.
- The root `Directory.Build.targets` still pins 10.1.402.120. Row E002 moves the pin to 136 before any mark is written.
- A project gets the package reference in the row that writes its first mark. That includes the shipped
  `Softellect.Berreman.Core` package, for which it is a new dependency.
- STL is never built or bumped from here.

### R5 — Every product project is cleaned, the heads included

That covers TestWindows and TestWindows.App (404 findings) and App (15). The operator: "all ELEV (except 004) must be
cleaned up".

- The first draft cited STL 0193 round 048, which kept CLI and UI heads out "until the libraries are green". That was
  STL's interim scope, and it is gone.
- The current reference is STL spec 0229 `.manual/005` §2:
  - **Q1:** every F# project is in scope, its heads, consoles, viewers and samples included. Test assemblies are
    exempt by the analyzers' own rule.
  - **Q5:** every `main : string[] -> int` is marked
    `[<ElevationBoundary(nameof IoBoundary, "the process entry point - argv in, exit code out, per the .NET contract")>]`.
    Here that is `OpticalConstructor.App/Program.fs` and `OpticalConstructor.TestWindows.App/Program.fs`.
- The order is dependency order: the TestWindows rows follow Ui, and App comes last (rows E037–E042).

### R6 — The analyzers win

- Where any document disagrees with the analyzers, the analyzers are followed. That includes STL's own `AGENTS.md`,
  which still gives a closed set a string wire tag (round 056) where ELEV017 enforces an integer.
- STL's documents will be updated in STL, but not now: an arc is running there.
- Berreman's `AGENTS.md` follows the analyzers.

### R7 — ELEV004 stays out of the gates

- As in STL, ELEV004 is excluded in the gate command (`-ExcludeRules ELEV004`), not in the rules.
- It is measured (222 findings) and is not part of this ledger.
- The ledger is the JSONL (§1), not an MD.

### No deferrals

STL 0229 `.manual/006`: "There are no deferrals. Work that cannot be finished is a blocked row."

- The first draft left the code PART B deletes uncleaned until the step that deletes it. That is withdrawn: every
  finding is closed in its row, and PART A ends with `Elevation/elevation-known-findings.json` empty.
- PART B then starts from an empty list, so the gate itself holds every line PART B writes.
- What PART B deletes later is a small share of PART A:
  - two whole files, `SolverHandoffView.fs` (27) and `ProjectFile.fs` (4);
  - declarations inside files that stay: the project aggregate in `ProjectJson.fs`, the code seed values, and the
    scene and collection seams.

## 3. The census

- **Command:** `Elevation/Run-ElevationGate.ps1 -All -ExcludeRules ELEV004`, the `elevation-full` gate.
- **When:** 2026-11-01, on branch `0048` (`92c11f7` plus spec 0048's first round, none of it product code).
- **Analyzers:** `Softellect.Analyzers` 10.1.402.136, STL's current slow ring, run by `fsharp-analyzers` 0.41.0.
- **Scope:** the 13 F# product projects of `Berreman.slnx`; 240 s at throttle 4.
- **Result:** **3,757 enforced findings in 138 files**, recorded as 3,661 entries (one entry per rule, file and message,
  with a count).
  - There are 222 more under ELEV004, which no gate enforces (R7).
  - No third-party (`T`) finding was reported.

The scoped `elevation` gate, run as the arc runs it, passes with all 3,757 recorded: 0 new findings, 268 s.

### By rule

| Rule | Findings | What it reports | The usual fix |
|---|---:|---|---|
| ELEV003 | 1,005 | a bare primitive in a public signature | elevate the parameter or result; mark a genuine seam's declaration (R3, R5) |
| ELEV003P | 458 | the same, on a private signature | the same, in one file |
| ELEV008 | 472 | a bare primitive record field | elevate the field's type, or the record is a carrier (R3) |
| ELEV010 | 444 | primitives beside our own types in one signature | elevate the primitives (no boundary claim exempts this) |
| ELEV010P | 248 | the same, private | the same |
| ELEV015 | 312 | a public module-level name declared twice (`view` 26, `init` 15, `update` 15, `readout` 11, …) | rename to carry its surface (R2) |
| ELEV013 | 236 | a free function of one of our types | make it a member of that type |
| ELEV013P | 144 | the same, private | the same |
| ELEV016 | 167 | a DU case name used by two unions | rename the higher one's case, in two or more words |
| ELEV011 | 102 | a tuple wider than three, or with a repeated element type | a record |
| ELEV014 | 81 | a type name declared twice (`Model` 14, `Msg` 14, `State` 12, `Handlers` 10, …) | rename the higher one (R2) |
| ELEV020 | 75 | a case of a `*Error` union not ending in `Err` / `Exn` | rename the case |
| ELEV009 | 11 | a primitive on a `Result` happy path | elevate the `Ok` payload |
| ELEV012 / 012P | 1 / 1 | a two-case union mapped to `bool` | delete the mapping; match on the union |
| **Total enforced** | **3,757** | | |
| ELEV004 (not gated) | 222 | `.value` outside the IO boundary | not in this ledger (R7) |

Every duplicate-name finding (ELEV014/015/016) is a collision inside Berreman. None is against `Softellect.Sys`.

### By project, in dependency order (lowest first)

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

The per-file list is the known-findings file itself. The heaviest files are rows of their own (§4).

## 4. The rows

**44 rows:** E001 the baseline, E002 the `Softellect.Sys` pin (R4), E003–E042 elevate (40), and E043–E044 closing.
They are cut as STL 0229 `.manual/005` §4 cuts its queue:

- every finding sits in its file, and every file in the project whose folder holds it;
- tiers drain in dependency order (§3), projects go in `ProjectReference` order inside a tier, and each project's files
  in F# compile order;
- a tier's files are cut into rows of at most **120 findings**, 0229's budget. A file over the budget is a row of its
  own, and a row never spans two tiers.

| Rows | Tier | Holds | Findings | Rulings |
|---|---|---|---:|---|
| E001 | | the baseline (§6) | | |
| E002 | | the `Softellect.Sys` pin (§6) | | R4 |
| E003 | Solver | Core (11 files), OpticalProperties, OpticalModel | 115 | R2, R3 |
| E004–E005 | Analytics | `Variables.fs` and the rest | 148 | R3 |
| E006 | Controls | `UiIds.fs` | 218 | R1, R2 |
| E007–E012 | Controls | the controls, `ExperimentControls.fs` (95) and `ExperimentChart.fs` (117) each on its own | 573 | R2 |
| E013–E021 | Domain | `DispersionModels.fs` (106), `MuellerReconstruction.fs` (90) and `MuellerInverse.fs` (41) each on its own | 753 | R2, R3 |
| E022–E024 | Services | Optimization, Storage, Seeding | 279 | R2, R3 |
| E025–E036 | Ui | groups of views, then the editors and windows one per row, `TableAndElementRotationView.fs` (298) last | 1,252 | R2; R1 in E036 |
| E037–E041 | TestWindows | the six test views, with `TestWindows.App/Program.fs` in the last row | 404 | R2, R5 |
| E042 | App | `AppConfig.fs`, `Program.fs` | 15 | R1, R2, R5 |
| E043 | | close PART A (§6) | | |
| E044 | | the lists, no new work (§6) | | |

**The fields.** Every elevate row carries:

| Field | Holds |
|---|---|
| `id`, `state`, `kind`, `tier`, `title` | `state` is `open`, `done` or `blocked`; `kind` is `baseline`, `pin`, `elevate` or `closing` |
| `files` | the row's files, in the order to clear them |
| `findings` | the row's enforced findings by rule, from the known-findings list |
| `projects` | the projects that own `files` |
| `compiled_by` | every project that compiles a row file |
| `packages` | those of them packed as NuGet packages (Core, OpticalModel, OpticalProperties, Analytics), whose public renames are listed (§5) |
| `suites`, `gates` | the test projects that reference `compiled_by`, and the gates the row runs: `build`, `elevation`, and `unit-tests` (BerremanTests), `constructor-unit-tests` (OpticalConstructor.Tests), `ui-smoke` / `ui-tests` (OpticalConstructor.Ui.Tests) where they reach |
| `rulings` | the rulings of §2 that bind the row |
| `done_when`, `note` | when the row is done (§5), and what its closing step writes |

The baseline, pin and closing rows carry `task` and `gates` instead. §6 binds over their `task` text.

## 5. Every row

**Read**, in this order: the row; the rulings in its `rulings`, in §2; `AGENTS.md`.

**Steps.** A step of the spec works inside ONE row: the whole row, or some of its files in the row's order. It never
spans two rows. Rows close in id order.

**Work.** Elevate per `AGENTS.md`:
- the elevated type is born where the value first exists;
- a conversion is a named member on the source type;
- a bijection collapses onto the lower type;
- names are unique (R2).

Callers move in the same step, across every project in `compiled_by` and every project that references them, test
projects included. A caller moves to the new name and never re-wraps. A mark goes only on a genuine seam's
declaration, with its kind and its reason (R3, R5).

**The known-findings list.**
- The step deletes the entries of what it fixed, or lowers their counts, in the same change.
- A change can also clear findings outside the row's files. The usual case is the higher side of a duplicate name,
  whose finding goes when the lower side is renamed. For example, moving `WorkbenchIds` onto the id type (row E036)
  clears the App's two ELEV015 entries (row E042). Those entries go in the same step, because a stale entry fails the
  gate, and the note names them.
- It never adds an entry and never re-records the list.

**Gates.**
- `build` and `elevation` run on every step, with zero warnings from our code.
- The test gates in `gates` run selected by class on what the step touched. `unit-tests` runs by class only; the
  whole BerremanTests run is the spec's final step (`021`).
- A rename across projects runs `elevation-full`.
- Counts never regress, and no number any test asserts changes.

**The row's note**, in this order (STL 0229 `.manual/005` §5):
1. the counts before and after;
2. every `[<ElevationBoundary>]` and `[<NumericCarrier>]` added, with its reason;
3. every stored-format change: a field or case whose JSON, settings or database shape changed, and what an old file now
   meets. Under `020` §R that is a typed error, never a migration. Each one is also a line of
   `stored-format-changes.md`;
4. every declaration deleted;
5. every public rename in a package of `packages`. Each one is also a row of `renames.tsv` (old, new, package, row),
   as in STL 0229 Q4.

**Flip the row** in the step that clears its last file, in the same change as that step's code: `state` becomes
`done`, `closedIn` names the step, and the note is written. The two are halves of one state. If they can disagree, the
next session resumes from a lie (STL 0229 `.manual/006`).

**Blocked.** A row can turn out to be wrong about the code, or a ruling can turn out not to apply as written. Then the
row's `state` becomes `blocked`, its note names the question, and the work stops at that row. A ruling is never
adjusted to fit the code, and nothing is deferred.

**Never regenerate the ledger.** `build_elev_ledger.py` ran once, when the ledger was made, as
`record_known_findings.py` did for the list.

## 6. The baseline, pin and closing rows

- **E001, the baseline.**
  - Before any source edit, run `elevation-full` over the tree the spec starts from. It must report 0 new and 0 stale.
  - Any difference is a blocked row that names it.
- **E002, the pin (R4).**
  - Move `Softellect.Sys` in the root `Directory.Build.targets` from 10.1.402.120 to 10.1.402.136, then restore.
  - Repair any API drift in App, Optimization, Storage and BerremanTests, the projects that reference it today.
  - Gates:
    - `build`;
    - `constructor-unit-tests`, `ui-smoke` and `ui-tests` whole;
    - `unit-tests` by the classes that reach `Softellect.Sys`.
- **E043, PART A closes.**
  - `Elevation/elevation-known-findings.json` keeps its shape with no finding in it, so from here on every finding fails
    the gate.
  - `elevation-full` reports 0, and every test gate is green at its count. `unit-tests` stays the spec's final step.
- **E044, the lists, no new work.**
  - Check the three lists against the row notes and the branch diff:
    - every mark, derived from git as STL 0219 `.manual/026` derived its own;
    - `renames.tsv`;
    - `stored-format-changes.md`.
  - No build and no suite.
