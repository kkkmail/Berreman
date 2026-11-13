# Step 0378 — state of the world

## Where we are

This is spec 0050 section S11.2, the forward model of an experiment and the study residual.

- **Steps 372–376** built the forward model and the row residuals.
- **Step 377** added the study's pre-fit: every enabled run a fit reads, checked and flagged in study order.
- **This step** gathers what the residual reads from that pre-fit, once:
  - each fitted run's kept rows;
  - each session's dark level.
- **Step 379** builds the study residual over this `StudyData`.

## What's working

- Add `StudyForward.StudyRun`, `SessionDark` and `StudyData` (with `darkOf`): a study's fitted rows and dark levels
- Add `Study.dataWith`:
  - it places each fitted run's checked points on its scan, in study order;
  - each session listing a fitted run gets the mean signal of its own Dark runs' rows, else of every Dark run's.
- Refuse a failed pre-fit check (`PrefitFailedErr`) and a Dark run that records no signal (`DarkReadingErr`)
- Move the reading-kind text onto `RecordedReading.kindText`, so the residual and the dark refusal share it
- Add three facts and widen the forward-error render fact (constructor suite 1642 → 1645)

## Tests

```yaml
gates:
  constructor_unit_tests: 1645
```

| Gate | Result | Time (deadline) |
|---|---|---|
| build (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`) | exit 0, `0 Error(s)`, no lowercase `error` in the log. The warnings are the exempt NU1902 advisories, plus the vendored MathNet `SYSLIB0051` whenever MathNet recompiles. None comes from this step | 77 s full, 17 s incremental after the final edit (600 s) |
| constructor-unit-tests (the gate's own command, whole suite) | `Test run summary: Passed!`, 1645 succeeded (1642 at checkpoint 377, +3), 0 failed, 0 skipped. In the TRX, `a study's data holds each fitted run's kept rows in study order`, `a session's dark level is the mean signal of its own dark runs, else of every dark run, and none without a dark run`, `a failed pre-fit check or a dark run that records no signal is refused` and the widened `every forward error renders its condition and its payload` are `Passed` | 73 s (180 s) |
| elevation (from arc base `f395597c`, ELEV004 excluded) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0, on the final source | 228 s (600 s) |
| the twenty s37 invariants | each `section_invariants --find-nothing` prints `INVARIANT HOLDS` and exits 0 | seconds each (300 s) |

Nothing is deferred. Every duration is within its deadline.

## Architecture

`Study.dataWith context prefits : Result<StudyData, StudyForwardError>` works in four steps:

1. **Checks.** Every prefit's `check` is collected through the shared ordered `ItemEvaluation.sequenced`. The first
   `Error`, in prefit (study) order, answers `PrefitFailedErr (name, error)` before any data is placed.
2. **Runs.** Each `ResidualRunUse` prefit gives `points.measuredRowsFor experiment.scan` (step 371) as a `StudyRun`,
   in order. The check's points are already kept by `keptBy` and validated, so an excluded point is never a row.
3. **Dark signals.** Every `DarkLevelRunUse` prefit's rows are placed on its scan. Each row must be a `RecordedSignal`;
   otherwise `DarkReadingErr` names the run and what it records (`kindText`, or "a Mueller observation"). A Dark run
   keeping no point also refuses.
4. **Darks.** These come from the sessions of `this.sessionsIn context.kit.experiments` (step 358) that list a fitted
   run.
   - Each such session reads `session.darkRunsIn` over the prefits' experiments (step 357): its own enabled Dark runs,
     else every Dark run the fit reads.
   - Its level is the pooled mean of those runs' row signals, as a `DarkMean`.
   - A session with no such run has no entry, so `darkOf` answers `None`. Step 379 then lets its intensity runs take
     the detector's `darkOffset` (step 374).

`StudyForwardError` gains two cases:

- `PrefitFailedErr of experiment : ExperimentName * error : PrefitError`, rendered
  `the experiment is not ready for a fit: <name>: <error.describe>`;
- `DarkReadingErr of reason : string`, rendered `a dark run does not record a signal: <reason>`.

No cache or option is added.

## Deferred

Nothing.

## Gotchas

- **"Listing a run" means listing a fitted run.** A session made only of Dark runs takes no level. Step 379 looks a
  level up by the session listing each fitted run. The dark fact pins this with K2 alone in `Session 3`.
- **The mean is pooled over rows, not over runs.** K1 (0.2, 0.4) with K2 (0.9) gives 0.5, not 0.6. The dark fact
  asserts this.
- **A disabled Dark run counts for nothing.** It is absent from the prefits, so it is neither a session's own nor one
  of "every Dark run". `darkRunsIn` still sees the disabled run in the session's list, but finds no Dark prefit of it.
- **A Dark run with every point excluded refuses** (`"<run> keeps no point"`). Otherwise a session reading only that
  run would average nothing, or silently fall back to `darkOffset`. This is asserted inside the authorized refusal
  fact; no test identity was added.
- **Refusal order:**
  1. a failed check, before anything else;
  2. then a fitted run's `UnpairedPointsErr`;
  3. then a Dark run's `DarkReadingErr`.

  Every Dark run is read, whether or not a session needs its level.
- **Record labels.** `StudyRun` now holds `experiment` and `rows`, which shadow the labels of `ExperimentPrefit` and
  `MappedAttachment` for any un-annotated lookup in a file that opens `StudyForward`. The build shows no such site.
  New code should annotate, as the existing code already does.
- **Test fixtures.** Step 377's helpers were generalised without changing their facts:
  - `prefitDataOver` takes a points map, and `prefitPoints` keeps step 377's;
  - `contextOverData` builds the bench context over any data seam;
  - `lp2Turn` is the Malus scan's axis.

## Changelog

- 2026-11-13 — Step 0378: added `StudyRun`, `SessionDark`, `StudyData` and `Study.dataWith`, the study's data:
  - each fitted run's kept rows, in study order;
  - each session's dark level, the pooled mean signal of its own Dark runs, else of every Dark run;
  - a failed check refuses with `PrefitFailedErr`, and a Dark run of no signal with `DarkReadingErr`.

  Three new facts and the widened error-render fact (constructor suite 1642 → 1645).
