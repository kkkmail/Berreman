# Step 0489 — state of the world

## Where we are

Spec 0050, section S14.2: the Measurement Planner.

- Step 0488 wired the plan job into the constructor window.
- This step adds Add to kit: a plan candidate becomes a setup and a `PlannedRun` experiment of the kit. Plan →
  measure → attach → fit is then one object model.
- Step 0490 builds the Plan view over this model, and step 0492 composes the real host.

## What's working

- Added `AddPlannedExperiment of PlanCandidate`: it records the candidate's plate in the table's Object 1 slot as a
  setup, and a `PlannedRun` sweeping R2 then R1 on its branch
- It refuses a read-only kit, a table with no sample slot, a candidate the kit already plans, and a branch no detector
  captures
- Moved `GenerateSetupGrid`'s body into the private `withGridRecorded`, which Add to kit reuses; the grid's behaviour is
  unchanged
- Added three `PlanWindowModelTests` facts: added, not added again, and refused

## Tests

These are local runs over the final tree, each with the gate's own command. The supervisor's gate engine is the
authority.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; only the exempt `NU1902` | 42 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 | 242 s | 600 s |
| constructor-unit-tests | `Passed!`, 1905 of 1905 = 1902 + the three census facts | 137 s | 180 s |
| ui-smoke | `Passed!`, 268 of 268 | 223 s | 600 s |
| ui-tests (selected `KitWindowTests`, `StudyViewTests`) | `Passed!`, 24 of 24 | 34 s | 360 s |
| 21 × `invariant-s46-*` | each grep finds nothing, also with `--untracked` | — | 300 s |

- **Red.** All three facts were red against the missing message, by compile.
- **Mutations.** Four mutations were each caught by the intended fact and reverted: dropping the role, not putting the
  draft back, skipping the coverage check, and skipping the capture check.
- `SetupModelTests`, the grid's facts, pass unchanged.
- No run exceeded its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1905
  ui_smoke_tests: 268
  ui_tests: 24
```

## Architecture

- **One recording for the grid and the plan.** `withGridRecorded grid collection role model` does the work that was
  `GenerateSetupGrid`'s body:
  - it writes the grid's setups the kit lacks;
  - it commits `collection`'s draft over every setup, each appended experiment taking `role` before it is recorded;
  - it answers `Ok (written, setups, appended)`, or `Error` with the model carrying step 276's text.
- **The callers.**
  - `GenerateSetupGrid` passes the window's collection and `ScienceRun`.
  - `AddPlannedExperiment` passes a collection whose draft is rebuilt from `ExperimentDraft.empty`, then `PlannedRun`.
- **The planned draft** is built only by the draft's own functions:
  - `chooseElement [VaryR2]`, `setRange incidenceRange`, `addScanAxis`;
  - `chooseElement [VaryR1]`, `setRange azimuthRange`, `chooseMeasurement capture`.
  - Its scan is exactly the one block `PlanCandidate.coverageIn` reads.
- **The window's draft is put back on every path**, so Add to kit never disturbs the Experiments bay.
- **No undo of its own.** An added experiment is removed through the Experiments bay's Remove (.spec-md §10.5).

## Deferred

Nothing. Every requirement and the three census facts landed this step. The Plan view, its button and the composed
host are steps 0490–0492's own briefs.

## Gotchas

- **An impossible combination.** `SetupGrid.create` unticks a combination the table cannot make, and `generate` then
  answers `Ok []`. The arm therefore asks the combination's `slotsOver` first and says `Not generated — <why>`, rather
  than "planning" nothing. This is the one check the brief does not list.
- **The capture check is load-bearing.** `effectiveCapture` coerces a requested branch to what the detectors allow.
  Without the check, a reflected candidate over a transmitted-only table is recorded as a transmitted experiment.
- **The branch word.** `transmitted` / `reflected` is a local match on the candidate's observable.
  `PlanCandidate.caption` holds the same words as its own local, in `MeasurementPlanner.fs`, which is outside this
  step's edits.
- **Shared record labels.** `slot`, `r1` and `itemOpt` are shared by several records. Annotate
  `SetupGrids.SlotChoice` and `SlotOptions` literals.

## Changelog

- 2026-11-18 — Step 0489: Add to kit (`AddPlannedExperiment`) records a plan candidate as a setup and a `PlannedRun`
  through the grid generator's recording (`withGridRecorded`, shared with `GenerateSetupGrid`); three
  `PlanWindowModelTests` facts.
