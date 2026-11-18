# Step 0490 — state of the world

## Where we are

Spec 0050, section S14.2: the Measurement Planner.

- Steps 0486–0489 built the plan run, its job, its window model and Add to kit.
- This step adds the Plan view: the Inverse constructor's flow strip gains Plan between Unknowns and Fit, and its
  surface shows Run plan and the committed recoverability report.
- Step 0491 adds the ranked candidates and their Add to kit to this view; step 0492 composes the real host.

## What's working

- Added `PlanFlowStep` between Unknowns and Fit in the Inverse constructor's flow strip; the Main strip has none
- Added `PlanView.planView`: the chosen study's crystal materials, the two criteria, and Run plan with per-candidate
  progress and Cancel on one row
- The Plan view shows the committed report: findings, rank, confounded combinations, bounds grid and instrument states
- Added the Plan step's fourteen `ConstructorUiId`s and `planMaterialChoice`
- Added the `TestAppScope.planKitIn` fixture, `PlanViewTests`, the Plan step's reachability fact, and the changed
  flow-strip facts

## Tests

These are local runs over the final tree, each with the gate's own command. The supervisor's gate engine is the
authority.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; only the exempt `NU1902` | 92 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 | 243 s | 600 s |
| constructor-unit-tests | `Passed!`, 1905 of 1905 (two changed expectations, count unchanged) | 137 s | 180 s |
| ui-smoke | `Passed!`, 269 of 269 = 268 + `pressing Run plan…` | 230 s | 600 s |
| ui-tests (selected `KitWindowTests`, `PlanViewTests`, `ScrollExtentTests`) | `Passed!`, 40 of 40 | 62 s | 360 s |
| 21 × `invariant-s46-*` | each grep finds nothing, also with `--untracked` | — | 300 s |

- **The ui-tests count.** The selection holds the three new census facts. Without them it would be 37, so the rise
  is 3, as the acceptance asks.
- **Red, shown by mutation.** One build carried four mutations, and each was caught by its intended fact:
  - Main's strip keeps Plan;
  - the findings list drops the structural findings;
  - Cancel and the bar leave the run's row;
  - Run plan is withheld from a read-only kit.

  All four were reverted byte for byte.
- **The first elevation run failed** with 6 × `ELEV013P`, and was fixed before the final runs; see Gotchas.
- No run exceeded its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1905
  ui_smoke_tests: 269
  ui_tests: 40
```

## Architecture

- **The Plan step is a step, not a window.** It is a `ConstructorFlowStep` of the Inverse constructor alone, like the
  study's other steps:
  - `stepsOf ForwardConstructor` filters it out;
  - `ShowFlowStep` keeps the open bay for it;
  - `SwitchConstructorMode ForwardConstructor` from it lands on Experiments, through the existing rule.
  - It adds no `WindowFit.keepOnScreen` call, and it joins `ScrollExtentTests` instead.
- **`PlanView.fs` is a pure projection of steps 0487–0489's model.** It reads `planMaterials`, `planMaterialOpt`,
  `planSurface`, `planJob` and `planReportOpt`, and sends `SetPlanMaterial`, `SetPlanCriterion` and `StudyPlan`
  (`RunPlan` / `CancelPlan`). It adds no message or model field.
- **The run row** is Run plan (disabled while running) · Cancel · determinate bar · progress text. It is keyed by its
  request, because `actionButton` keeps its first handler per key.
- **The report's renderings are private extension members** of `ControlCaption`, `CombinationText`, `PlanReport` and
  `PlannerInformation`, as the elevation rules require of one-parameter helpers.

## Deferred

Nothing. Every requirement of the brief and the four census facts landed this step. The ranked candidates and their
Add to kit are step 0491's own brief, and the composed host is step 0492's.

## Gotchas

- **`ELEV013P` is enforced on private helpers.** A free private function whose only parameter is one of our types
  fails `elevation`, though the build is green. Write it as a private extension member of that type (`PlanJob.fs`,
  `PlanView.fs`).
- **The UI test project cannot see `OpticalConstructor.Tests`' fixtures.** So `planKitOver` builds its own probe
  quartz and QZ-Z, with the brief's numbers and GUIDs of its own (`49000000-…`). It saves them through the context's
  stores.
- **`σ / value` is computed as σ / |value|.** g33 is negative. The header keeps the brief's text.
- **Two choices the brief leaves open:**
  - **The rank line at full rank with no finite condition** reads `Rank r of n from what the kit holds.`.
  - **Before the first candidate**, the progress text reads `Starting…`, as the kit send does.
- **Run plan stays offered in a read-only kit**, because a plan writes no kit file.
- **`SYSLIB0051` warnings from the vendored MathNet** appear whenever it recompiles. They are pre-existing, outside
  this repo's F# code, and absent from incremental builds.

## Changelog

- 2026-11-18 — Step 0490: the Plan view. `PlanFlowStep` sits between Unknowns and Fit in the Inverse constructor. New
  `PlanView.planView` shows the material and criterion choices, Run plan with per-candidate progress and Cancel, and
  the committed report's findings, rank, confounded combinations, bounds and instrument states. Added the Plan step's
  `ConstructorUiId`s, the `planKitIn` fixture, `PlanViewTests`, and the reachability and flow-strip facts.
