# 001 — OneDrive off the build machines, the move to MTP, and the ELEV gates: log

The operator's four asks (2026-11-01), in order:
1. OneDrive must be completely disabled on every build machine. That needs an ASG script, and `0046/.manual/020` and
   `021` must say so.
2. Set up the ELEV gates (ELEV004 excluded) in this repository, without touching STL, where an arc is running. Run
   them here, write a ledger in 0048 for whatever must be fixed, and put that ledger into the first part of `021`.
3. Check that `CLAUDE.md` / `AGENTS.md` carry the ELEV rules and the other coding guidelines, against STL's
   guidelines and the ASG constitution.
4. Move every gate from VSTest to Microsoft.Testing.Platform (MTP), as STL did (STL `specs/0239/.manual/005`).

Nothing is committed. No version ring was bumped. STL was only read.

## 1. OneDrive

- **The script:** `C:\GitHub\AI-Strategy-Generator\Disable-OneDrive.ps1`, a new file beside the other machine
  provisioners. It runs elevated, under pwsh 7. It:
  1. refuses to start while any account's OneDrive folder holds a cloud-only (Files On-Demand) file, because such a
     file cannot be opened once OneDrive is gone; `-Force` overrides;
  2. sets the policies `DisableFileSyncNGSC`, `DisableFileSync`, `DisableLibrariesDefaultSaveToOneDrive`,
     `KFMBlockOptIn` and `PreventNetworkTrafficPreUserSignIn`;
  3. stops every OneDrive process, and stops and disables its updater service;
  4. uninstalls the per-machine install and this account's per-user install, and deletes the per-user program folder
     of every other account;
  5. removes the `OneDrive` / `OneDriveSetup` Run entries from every profile hive, the Default profile included;
  6. unregisters the scheduled tasks;
  7. unpins the Explorer navigation-pane entry;
  8. reports known folders still redirected into OneDrive; `-RestoreKnownFolders` points them back.

  It ends by verifying. Exit 0 means nothing is left; exit 1 lists what is. Synced files are never deleted. `-DryRun`
  changes nothing.
- **Not run here.** This session is not elevated (`thunder\claude-agent`), and the script refuses to run otherwise.
  It parses clean. **OneDrive was running on this machine during the session** (pid 44608). Run it elevated, with
  `-DryRun` first.
- **Not wired into `Install-All.ps1`.** Adding it as a step is a one-line change to that orchestrator's step table, if
  the operator wants it there.
- **Specs:**
  - `0046/.manual/020` §G gains the build-machine rule.
  - `0046/.manual/021` gains it in its operator notes and in the prompt's binding rules.
  - `AGENTS.md` gains it under "Working in this repo".

## 2. VSTest → MTP

**Starting point.**
- The three test projects were already xUnit v3 (`xunit.v3` 4.0.0, which is `xunit.v3.mtp-v2`), but they ran on
  VSTest: `Microsoft.NET.Test.Sdk`, `xunit.runner.visualstudio`, `coverlet.collector`, and
  `IsTestingPlatformApplication=false` in the root `Directory.Build.props`.
- `BerremanTests` still carried three xUnit v2 packages:
  - `Xunit.SkippableFact`;
  - `FsCheck.Xunit`, which nothing used;
  - their `xunit.abstractions`.

**What the v2 remnants were hiding.** Under xUnit v3, a `[<SkippableFact>]` is not a fact, so
`SolverTests.basicSolverTest1–3` were never discovered. `FieldFunctionTests` took v2's `ITestOutputHelper`; it has
no facts.

**Changes (as STL 0239 `.manual/005` §2):**
- **Root:**
  - a new `global.json` with `"test": {"runner": "Microsoft.Testing.Platform"}`, and no SDK pin, since Berreman never
    had one;
  - `Directory.Build.props` loses `IsTestingPlatformApplication=false`;
  - `Directory.Build.targets` loses the `coverlet.collector`, `FsCheck.Xunit`, `Microsoft.NET.Test.Sdk`,
    `xunit.runner.visualstudio`, `xunit.v3` and `Xunit.SkippableFact` rows, and gains
    `Microsoft.Testing.Extensions.TrxReport` 2.5.2 and `xunit.v3.mtp-v2` 4.0.2 (STL's versions).
- **Each test project:**
  - `OutputType Exe` and `UseMicrosoftTestingPlatformRunner true`;
  - references `Microsoft.Testing.Extensions.TrxReport` and `xunit.v3.mtp-v2`;
  - the VSTest references are deleted.
- **Code:**
  - `SolverTests.fs`: the three `[<SkippableFact>]` become `[<Fact>]`, and `Skip.If(true, …)` becomes v3's
    `Assert.Skip`;
  - `FieldFunctionTests.fs`: `open Xunit.Abstractions` becomes `open Xunit`;
  - left alone: `AdvancedSolverTests.fs` and `EigenVectorTests.fs` still say `open Xunit.Abstractions`, but no project
    compiles them.
- The three test projects' `bin` / `obj` were deleted before the rebuild, so no VSTest adapter survives in an output.
  The solution builds with no new warning (the only two are MathNet's own `SYSLIB0051`).

**The gates:**
- **Every test gate**, `unit-tests`, `constructor-unit-tests`, `ui-smoke` and `ui-tests`, now runs
  `dotnet test --no-build -c Release -p:Platform=x64 --no-ansi --results-directory TestResults --report-trx
  --report-trx-filename <gate>.trx {selector}` from its project's folder, with `requires_gates: [build]`.
- **`pass_when`:**
  - `stdout_match '(?m)^Test run summary: Passed!'`;
  - STL's `failure_capture`;
  - the count floor reads `succeeded:`, with its baseline keys unchanged.
- **`result_capture`:** TRX. The skip policy is `allow` for `unit-tests`, whose `SolverTests` has eight skipped facts,
  and `fail` elsewhere.
- **Selector:** the class selector `--filter-method "{value}.*" --filter-method "{value}"`.
- **Filters:**
  - `ui-smoke` filters with `--filter-trait "Category=ui-smoke"`;
  - `ui-tests` with `--filter-not-trait "Category=ui-smoke"`.
- **Timings:**
  - `ui-tests`' deadline rises from 180 s to 360 s (measured 177 s; STL's rule is at least twice the measurement);
  - `constructor-unit-tests` and `ui-smoke` declare `whole_suite: light` with their measured times.
- **Every descriptor gains `phase`** (`build` for `build`, `verify` for the rest). ASG's current reader refuses a
  descriptor without one, and none of Berreman's had it.

**An ASG reader defect.**
- **The defect:** ASG's own YAML reader (`multistep/_common/yaml_lite`) silently drops every key after a top-level
  block sequence that is not indented (`tags:` followed by `- test` at column 0).
- **The effect:** `constructor-unit-tests`, `ui-smoke` and `ui-tests` were written that way, so ASG read them with no
  `applies_to`, `qualifier`, `requires_gates` or `selector`.
- **The fix here:** the three are re-laid in the indented style STL uses.
- **For ASG:** this belongs in its error log. It was not changed here.

**Verified:**
- ASG's descriptor reader parses and validates all 7 descriptors. It bakes each command, with and without a selector,
  and reads `succeeded: 727` and a `failed <name> (12ms)` line through each gate's own patterns.
- **Each suite, run as its gate runs it:**

  | Suite | VSTest before | MTP after |
  |---|---|---|
  | constructor-unit-tests | 727 passed | 727 succeeded, 13 s |
  | ui-smoke | 208 passed | 208 succeeded, 27 s |
  | ui-tests | 559 passed | 559 succeeded, 177 s |
  | BerremanTests discovery (`--list-tests`) | 203 | 206 |

  The three extra BerremanTests are exactly `basicSolverTest1–3`.
- **`BerremanTests` was not run whole**, under the operator's standing "STOP berreman tests". Only the
  `SolverTests` class ran, through the gate's own selector: 40 tests, 32 passed, 8 skipped.
- **Still open: the `unit-tests` gate's time.** Its `runtime_estimate_seconds` is still 180, but a whole run is about
  30 minutes (`0044/.manual/020`). Before its summary, MTP prints only failures and skips (STL 0239 `.manual/005`
  §1). So a WHOLE run would hit the attempt deadline, and can be silent for longer than the 600 s silence window. Filtered by class, as the selector now
  allows, it fits. See `021` §2.

## 3. The ELEV gates

**There were none.**
- No `.elevation-analyzers.json`.
- No gate.
- No reference to the ELEV rules anywhere in this repository.

**Why STL's driver is not called directly.**
- `Analyzers/scripts/Run-ElevationGate.ps1` hard-codes STL's root, solutions and exclusions.
- It publishes the analyzers into STL's own `Analyzers/bin/publish`. Calling it from here would write into STL, where
  an arc is running, and would couple Berreman's gates to STL's working tree.

**What reuses STL.**
- **The analyzers:** the published `Softellect.Analyzers` package. Its version is pinned once in the root
  `Directory.Build.targets` (10.1.402.136, STL's current slow ring, from the `../Packages` feed `nuget.config`
  already names).
- **The output contract:** the same `FAILED:` / `ELEV-GATE:` lines, so the gate descriptors use STL's `pass_when`
  unchanged.
- **The known-findings format:** STL's.

**The proper long-term home.** A repository-neutral driver should ship with the analyzers, in the package or in ASG.
That is an STL change, not made here; the follow-up is in §6.

**New files.**
- `.elevation-analyzers.json`. In scope: `^Softellect\.Berreman\.`. Ours, for the duplicate-name rules:
  `^Softellect\.`. STL's file exclusions are cleared. Everything else keeps the analyzers' built-in vocabulary.
- `Elevation/Run-ElevationGate.ps1`, Berreman's driver.
  - **Scope:** the F# projects of `Berreman.slnx`, minus MathNetNumerics and the three test projects, which the
    analyzers exempt.
  - **Change mapping:** each changed `.fs` maps to the projects that compile it, then expands to their dependents. A
    clean tree, git failing, or a change to what the rules read means every project. `-All`, `-Project` and `-Base`
    work as in STL.
  - **The analyzer and the tool:** the nupkg is extracted under `Elevation/bin/`, which git ignores. `fsharp-analyzers`
    is installed there at the `FSharp.Analyzers.SDK` version the package's nuspec names (0.41.0).
  - **Robustness:** a project that delivers no report gets one serial retry. Reports land in `Elevation/bin/reports`,
    never `%TEMP%`.
- `Elevation/elevation-known-findings.json`, in STL's format with one entry per line. It holds the 3,661 entries
  (3,757 findings) of the run below, each naming the ledger `002`.
  - A recorded finding does not fail.
  - A new one fails.
  - An entry found fewer times than recorded fails as stale whenever a project compiling its file was analyzed (every
    entry under `-All`). STL judges staleness on full runs only; judging it here on every run forces a fix to delete
    its entry in the same step.
  - `-NoKnownFindings` fails on everything.
- `specs/0048/.manual/tools/record_known_findings.py` recorded that list once. It is provenance, not a step's tool.
- **The gates, in `.gates/Elevation/`:** `elevation` (scoped, `scope: working_tree`) and `elevation-full` (`-All`,
  `stage: final`). Both run with `-ExcludeRules ELEV004`, both require `build`, and both have a 600 s deadline.
- `.gates/Elevation/.gates-md`, plus a corrected root `.gates/.gates-md`. It used to list two root gates that do not
  exist, and one group.

**The runs.**
- **Full** (`-All -ExcludeRules ELEV004`, before the list existed): FAILED with 3,757 findings in 13 projects, 240 s.
  The ledger is `002`.
- **The `elevation` gate's own command**, run as the arc runs it: `OK - 0 new finding(s) … known findings: 3757`,
  268 s. The analyzer config had changed, so it analyzed all 13 projects.
- **Driver checks on `OpticalModel`:**
  - with the list: OK, 1 known;
  - with `-NoKnownFindings`: FAILED, 1 new;
  - with that entry's count raised to 2: FAILED, 1 stale.

  The list was then re-recorded unchanged.

## 4. `CLAUDE.md` / `AGENTS.md`

- **The structure:** the two files were byte-identical copies. As in STL, `AGENTS.md` is now the one authoritative
  file, and `CLAUDE.md` is a pointer that imports it (`@AGENTS.md`).
- **Added to `AGENTS.md`, from STL's `AGENTS.md`:**
  - `naming_conventions.md` as the naming reference (it is the same file as STL's);
  - value-restriction annotations;
  - the comments ruling, including no new comments in project files (it governs what is written, not a sweep);
  - two-word DU case names;
  - `*Error` cases ending in `Err` / `Exn`;
  - members may speak their primitives, and a single-type free function belongs on its type;
  - unique names instead of `RequireQualifiedAccess`, with UiIds named as a ledger item;
  - integer wire tags for closed sets;
  - elevate at origin, never at the call site;
  - cross-type reuse as a member on the source type;
  - bijections collapsed;
  - the tuple rules.
- **New sections:**
  - "The ELEV analyzers": the rule table, `P` / `T`, the boundary attributes, the gates, and the known-findings
    discipline;
  - the MTP rules, replacing the VSTest paragraph;
  - the ASG constitution's obligations on a spec;
  - the gate list;
  - the OneDrive rule.
- **The test commands** are the MTP forms. `BerremanTests`' time is corrected from "~13 minutes" to about 30
  (`0044/.manual/020`).
- **A conflict inside STL** (`002` R6): STL's `AGENTS.md` still describes string wire tags (round 056), while its
  analyzer ELEV017 enforces integers (round 058). Berreman follows the analyzer.

## 5. Files

- **Berreman:**
  - `global.json` (new);
  - `Directory.Build.props`, `Directory.Build.targets`;
  - the three test `.fsproj`, `SolverTests.fs`, `FieldFunctionTests.fs`;
  - `.elevation-analyzers.json` (new) and `Elevation/` (new);
  - `.gates/` (7 descriptors, 2 group docs);
  - `AGENTS.md`, `CLAUDE.md`;
  - `specs/0046/.manual/020`, `021`;
  - `specs/0048/.manual/` (new).
- **ASG:** `Disable-OneDrive.ps1` (new).
- **STL:** nothing.

## 6. For the operator

- Run `Disable-OneDrive.ps1` elevated on each build machine, `-DryRun` first. `-RestoreKnownFolders` applies where a
  known folder still points into OneDrive.
- The `002` §6 rulings, R1–R7. Each has a default the spec takes if not overruled.
- The `unit-tests` deadline and silence window for a whole `BerremanTests` run (§2).
- **STL follow-up (not done):**
  - ship a repository-neutral elevation driver with `Softellect.Analyzers`, or in ASG, so this repository's
    `Elevation/Run-ElevationGate.ps1` can be deleted;
  - bring STL's `AGENTS.md` wire-tag paragraph in line with ELEV017.
- **ASG follow-up (not done):** `yaml_lite` drops keys after an unindented top-level sequence (§2).
