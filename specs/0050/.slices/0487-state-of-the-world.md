# Step 0487 — state of the world

## Where we are

Spec 0050, section S14.2: the Measurement Planner. Step 0486 added the plan run itself, `PlannerInputs.planWith`,
with per-candidate progress and Cancel over the caller's `rowsOf`. This step hosts that run off the UI thread on
`JobRunner`, as step 393 hosts a study fit. It adds `OpticalConstructor.Ui.PlanJob`: the host, the job state, the
`PlanMsg` messages and their `updateWith`, and `PlanEnvironment.create`. The environment plans the named study's
material through the kit's `.oc/planner/` cache, and a read-only kit caches nothing. Step 0488 wires it into the
Main window's shell (`KitWindowShell.planRuns`, `StudyPlan of PlanMsg`), and 0490 builds the Plan view over it.

## What's working

- Added `PlanJob.fs`: `PlanJobHost.posting` / `onUiThread` / `inert` run one plan on `JobRunner` with per-candidate
  progress against the total and Cancel
- `PlanMsg.updateWith` runs one plan at a time, commits its report, keeps the last report on Cancel and holds any other
  error
- `PlanEnvironment.create` plans the named study's material, each candidate's rows read from or written to the kit's
  `.oc/planner/` cache
- `PlannerError` gains `PlanRaisedErr` and `UnknownPlannerStudyErr`
- Five `PlanJobTests` facts cover progress and commit, Cancel, a raise, one-at-a-time, and the cache

## Tests

Local runs over the final tree, each with the gate's own command; the supervisor's gate engine is the authority.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; only the exempt `NU1902`s (and the vendored MathNet's pre-existing `SYSLIB0051`s on its recompile) | 86 s first, 26 s incremental | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 (arc origin from `.arc-base-json`) | 237 s | 600 s |
| constructor-unit-tests | `Passed!`, 1899 of 1899 = 1894 + the five census facts | 139 s | 180 s |
| ui-smoke | `Passed!`, 268 of 268 | 224 s | 600 s |
| ui-tests (selected `KitWindowTests`, `StudyViewTests`) | `Passed!`, 24 of 24 | 34 s | 360 s |
| 21 × `invariant-s46-*` | each grep finds nothing, also with `--untracked` over the two new files | — | 300 s |

- `PlanJobTests` alone: 5 of 5, 7 s. The five TRX names match the census ids.
- An earlier whole-suite run, with elevation running beside it, failed only the environment fact. It had planned
  through the host and hit the ten-second wait; the fact now plans synchronously (see Gotchas), and every later run is
  green.
- No run exceeded its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1899
  ui_smoke_tests: 268
  ui_tests: 24
```

## Architecture

- **The study fit's shape, for a plan.**
  - `PlanJobHost.posting` gives each job its own `CancellationTokenSource` and `Stopwatch`, and runs
    `RanToCompletion [ plan observer ]` through `JobRunner.startBackground`.
  - The observer posts each `PlanProgress` as a `JobProgress` with a known total, so `progressText` reads
    `<k> / <n> (<t> s)`.
  - A raise ends as `PlanRaisedErr e.Message`. An outcome without exactly one result ends as `PlanRaisedErr` naming
    the count.
  - `onUiThread` is `posting` through `Dispatcher.UIThread.Post`. `inert` runs nothing.
- **The count crosses by a named member.** `PlanCandidateCount.toJobStepCount`, a private extension in `PlanJob.fs`,
  replaces the brief's `JobStepCount p.evaluated.value`, which ELEV001 reports as a call-site re-wrap.
- **`updateWith`:**
  - `RunPlan` while running changes nothing; otherwise it starts the plan, holds the handle and clears the error.
  - `CancelPlan` cancels only a running job.
  - `PlanEnded (Ok _)` commits the report.
  - `PlanCancelledErr` is `RunCancelled`, the report kept.
  - Any other error returns the job to `Idle`, clears its progress, keeps the report and holds the error, as the
    study fit's `refusedWith` does.
  - No queue, retry, second job or option.
- **The environment writes only the cache.** `PlanEnvironment.create host kit forward` finds the study in
  `forward.kit.studies` (none: `UnknownPlannerStudyErr` naming it). Then `plannerInputsWith` and `planWith` run with
  `rowsOf` = `cacheKeyWith forward.kit.items` bound to `kit.cachedPlannerRows key (candidateRowsWith
  forward.solverParameters)`. No kit file is written.

## Deferred

Nothing. Every requirement and all five census facts landed this step. The shell wiring, view and UiIds are steps
0488–0492's own briefs.

## Gotchas

- **Outside `effects.edits`** (the same shape step 0486 recorded and the operator committed):
  - `PlannerInputsTests.fs` and `PlannerReportTests.fs`: their exhaustive `PlannerError` or-patterns gain the two new
    cases, or the incomplete match warns (FS0025).
  - `PlanRunTests.fs`: `private` is dropped from `planStudy` and `contextOver`, so the environment fact runs over
    "`PlanRunTests`' study" by reuse, not a copy. No assertion changes.
- **Plan a real kit synchronously in a fact.** A real plan through the host, under the whole parallel suite, can
  exceed the stubs' ten-second wait. The environment fact calls `PlanEnvironment.planWith` directly, with a recording
  observer; facts 1–4 cover the host path.
- **The read-only check reuses the cache.** The same folder opened read-only hits the read-write run's cache, so its
  "writes nothing" is checked on a hit. The read-only miss path is `PlannerCacheTests`' existing fact. A second, fresh
  read-only kit was tried, but it took the fact to 91 s under the suite, so it was dropped.
- **The constructor suite is close to its deadline:** 139 s of 180 s. `PlanRunTests` (about 120 s in one class) and
  the environment fact (62 s under load, about 6 s alone) carry the real solves.
- **Shared field names:** `PlanRequest` and `PlanReport` both have `study`, `material` and `criterion`. Annotate a
  record literal where both are in scope.

## Changelog

- 2026-11-18 — Step 0487: `RunPlan` on `JobRunner` (`PlanJob.fs`: host, state, request, messages, environment,
  `updateWith`); `PlannerError` gains `PlanRaisedErr` and `UnknownPlannerStudyErr`; five `PlanJobTests` facts.
