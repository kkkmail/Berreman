# Step 0391 — impl log

## Progress

- 2026-11-13: read the task, the system and project prompts, the slice, step 390's records and the code
  (`StudyEngine.fs`, `FitRuns.fs`, `StudyForward.fs`, `PrefitChecks.fs`, `Studies.fs`, `Kit.fs`, `Setups.fs`). Wrote
  the impl plan.
- `StudyEngine.fs`:
  - `UnknownFitStudyErr` (`the kit has no such study: <reason>`);
  - `open OpticalConstructor.Domain.PrefitChecks`, for the data change flags and `MissingDataErr`;
  - appended last: `StudyFitRequest`, `[<ReferenceEquality>] StudyFitParam` and `StudyFitParam.runFit`.
  - The Optimization project builds: 0 errors, 0 warnings incremental.

- `StudyEngineTests.fs`:
  - the two new facts and the widened describe fact;
  - helpers `fitInstant`, `fitParamOver`, `heldRequest`, `fitRunOf`, `snapshotOf`, `fittedInRun`, `linearFitRunOf`
    and `assertLoneFitRun`.
- First class run: the series fixture failed the "no iteration told" guard. Automatic dispatch takes it nonlinear,
  which is step 386's designed choice. It is removed from the `runFit` fact; see Decision 4.
- Added the changed-since-attach check to the CP cases (Decision 5).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs` (authorized)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs` (authorized)

## Decisions

1. **The kit-derived parts are settled before any engine runs.**
   - The brief's order is: study, stage, pre-fit, data, engine, clock, then the record.
   - `dataRead` and `parameters` are pure reads of the pre-fit and the kit, so they are computed after `dataWith` and
     before the engine. A refusal there never wastes a fit.
   - Only the precedence of two refusals the brief does not name changes.
2. **`MissingDataFileFlag` is refused, never recorded.**
   - The brief names the hash for `DataUnchangedFlag`, `ChangedSinceAttachFlag` and `ChangedSinceFitFlag` only.
   - A missing file fails the pre-fit check, which `dataWith` already refuses. The flag can only survive when the
     status and the read disagree.
   - It answers `StudyForwardFitErr (PrefitFailedErr (run, MissingDataErr _))`, an existing case, so no hash nobody read
     is recorded. No other `StudyFitError` case is added.
3. **`Experiment.itemsUsedBy`'s `SetupError` maps to `StudyForwardFitErr (ForwardSetupErr _)`, an existing case.** The
   fitted runs are the kit's own and their setups resolved in the pre-fit, so this is the type's totality, not a new
   path.
4. **The series fixture is not driven through `runFit`.**
   - Step 386's `engineWith` makes a study nonlinear when a fitted path holds two unknown matrices ("QZ only ever
     behind a free LR"; the series runs hold QZ then LR).
   - Step 390's series fact reaches the linear stage 2 only by calling `reconstructLinear` directly.
   - Measured: through `runFit` the series dispatches to `fitNonlinear`, and the watch is told iterations 1, 2, 3, …
   - The brief's "every new step 390 case" is the partial variants (15, 14, 1 free), the fixed ideal CP, the constant
     and compound optics, the fixed surrounding objects, the equality links and the rank-deficient case. The acceptance
     names the ideal-CP m00 and equality-linked fixtures and the partial m33. All of those are driven through `runFit`
     under both stages. The series is a separate step 390 fact, and for it a nonlinear dispatch is the expected one,
     not the "unexpected nonlinear solve" the brief guards against.
5. **The data read is also checked at a changed hash.** In the CP cases, a proxy reports the Science run's file changed
   since attach. The run records it at the current hash and the unchanged Dark run at its own. This adds assertions
   inside the authorized fact; no test id is added.
6. **The rank-deficient case** is driven through `runFit` under both stages. It answers the solve's own
   `ObjectSolveFitErr` naming QZ, after one real SVD solve, and the clock is never read, so no run is stamped.
7. **Guards against an unexpected nonlinear solve.**
   - Every linear request gets a watch that records iteration reports, and the fact asserts none.
   - The deviate provider fails the fact on any draw.
   - The fact asserts `LinearStudyEngine`, at least one recorded SVD system, and one clock read.
8. **Step 385's study is rebuilt in `StudyEngineTests`.** Its fixture is private to `FitRunTests`, which this step may
   not edit. Rebuilt from the same drafts: CPL1 with `c2.retardance` unknown from 1.5 within [1, 2], QZ with `m00`
   unknown from 1 within [0, 2], the Phone, LP1, setups A and Q, runs A1, A2, E1 and K1.

## Testing state

These are local runs on the final code. Each finished within its effective deadline.

- `build`: `dotnet build Berreman.slnx -c Release` exit 0, 0 errors.
  - The only warnings are 12 NU1902 lines (exempt), and the log holds no `error`.
  - Timings: 45 s cold, 24–27 s incremental (deadline 600 s).
- `constructor-unit-tests`, whole suite, the gate's command: 1689/1689, 113–114 s (deadline 180 s).
  - The count is 1687 → 1689 (+2).
  - The TRX shows each of these `Passed` under its exact name:
    - `a fit run records its name, stamp, stage, engine, fitted runs, the data it read and the vector it used and
      produced` (12.3 s);
    - `a fit naming no study of the kit, or refused by its stage, records nothing`;
    - the widened `every study-fit error renders its condition and its payload`.
- `unit-tests`, selected `BerremanTests.MuellerReconstructionTests` + `BerremanTests.MuellerInverseTests`: 45/45,
  710 s (deadline 7200 s). No BerremanTests file is edited.
- `elevation`, standalone with no `-Base`, so all 13 projects:
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 226s` (deadline 600 s), with 0 known findings.
- The twenty `invariant-s38-*` greps, decoded from the snapshot and run with `git grep`: all find nothing (`clean: 20`).
- Mutation checks, each rebuilt and run against the new fact, then restored byte for byte from
  `.artifacts/0391-scripts/StudyEngine.fs.orig`. Every one fails the fact:
  - forced nonlinear dispatch: the "no iteration told" guard;
  - the attachment's own hash for a changed file: the `dataRead` check;
  - no material components: the `parameters` check;
  - entry starts taken from the fitted values: the n_o start check.
- LF only: 0 CR bytes in every written file, and `git diff --numstat` equals `--ignore-cr-at-eol`.

Commit-ready: every requirement of the slice is addressed in this round.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0391-build-local.log`, `0391-constructor-unit-tests-local.log`, `0391-unit-tests-local.log`,
  `0391-elevation-local.log`, `0391-invariants-local.log`, `0391-mutations-local.log` — the final local runs.
- `0391-studyengine-tests-local.log`, `0391-trx/studyengine.trx` and `0391-trx/constructor-unit-tests.trx` — the class
  and suite runs with per-test durations.
- `0391-scripts/` — the mutation script and the pre-mutation copy of `StudyEngine.fs`.

## Gotchas

- **The series fixture is nonlinear under automatic dispatch** (Decision 4). Step 390's series fact bypasses
  `engineWith` by calling `reconstructLinear` directly. Any later step that drives it through `runFit` meets
  `fitNonlinear` over 32 unknowns.
- **`StudyFitRequest`'s labels `study` / `stage` / `calibrationUse` are also `StagedStudy`'s and `FitRun`'s.**
  - The request is declared last, so an unannotated record expression with those labels now infers
    `StudyFitRequest` wherever `StudyEngine` is open.
  - `runFit` annotates its `FitRun`, and the test annotates its requests.
- **The fit-run fact runs the polarimeter's nonlinear fit twice**, once directly and once through `runFit`, because the
  brief asks for equality with `fitNonlinear`'s values. It is the class's second-longest fact (12 s). The suite stays
  at 114 s of its 180 s.
- **A run's `ChangedSinceFitFlag` cannot be seen in the run.**
  - It records the attachment's own hash, the same as `DataUnchangedFlag`.
  - So the latest-by-stamp selection of `fitRuns` changes only the pre-fit flag, which a fit run does not store.
  - Only the changed-since-attach hash is observable, and it is checked.
