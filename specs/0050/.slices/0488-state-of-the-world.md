# Step 0488 — state of the world

## Where we are

Spec 0050, section S14.2: the Measurement Planner.

- Step 0486 added the plan run (`PlannerInputs.planWith`), and step 0487 hosted it on `JobRunner` (`PlanJob.fs`).
- This step wires that job into the constructor window, as step 0412 wired the study fit. The shell carries the host
  (`KitWindowShell.planRuns`), the model carries the job's state and the Plan step's choices, and `StudyPlan` wraps
  `PlanMsg`.
- The real host is composed in step 0492, and step 0490 builds the Plan view over this model.

## What's working

- Added `KitWindowShell.planRuns : PlanJobHost`; `KitWindowShell.inert` holds `PlanJobHost.inert`
- Added `PlanSurfaceState` and the model's `planJob` / `planSurface`. `planMaterials` lists the chosen study's crystal
  materials; `planMaterialOpt` and `planReportOpt` pick the material and the report the Plan step shows
- `StudyPlan`, `SetPlanMaterial` and `SetPlanCriterion` messages: one plan at a time through the shell's host, under a
  read-write or read-only kit, writing no kit file
- Added three `PlanWindowModelTests` facts: run, cancel and read-only; material choice and criterion; report on
  completion

## Tests

Local runs over the final tree, each with the gate's own command; the supervisor's gate engine is the authority.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; only the exempt `NU1902`s (and the vendored MathNet's pre-existing `SYSLIB0051`s on a full recompile) | 86 s first, 25–42 s incremental | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 (arc origin from `.arc-base-json`) | 242 s | 600 s |
| constructor-unit-tests | `Passed!`, 1902 of 1902 = 1899 + the three census facts | 136 s | 180 s |
| ui-smoke | `Passed!`, 268 of 268 | 224 s | 600 s |
| ui-tests (selected `KitWindowTests`, `StudyViewTests`) | `Passed!`, 24 of 24 | 34 s | 360 s |
| 21 × `invariant-s46-*` | each grep finds nothing, also with `--untracked` | — | 300 s |

- **The new class alone:** 3 of 3, 2.5 s. The TRX names match the census ids.
- **Red.** Each fact was red against the missing symbols, by compile. Two mutations were then caught by the intended
  facts and reverted: guarding `RunPlan` for a read-only kit, and ignoring the study in `planReportOpt`.
- No run exceeded its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1902
  ui_smoke_tests: 268
  ui_tests: 24
```

## Architecture

- **The fit's shape, for a plan.** The host lives in the shell (inert until step 0492), the state in the model
  (`planJob : PlanJobState`), and one message wraps `PlanMsg`. `StudyPlan msg` is `msg.updateWith (PlanEnvironment.create
  model.kitShell.planRuns model.kit model.studyForwardContext) model.planJob`.
- **No read-only guard.** `StudyFit`'s `RunFit` is guarded because a fit writes its fit run. `StudyPlan` is not: a plan
  writes no kit file, and a read-only kit caches nothing (step 485).
- **The Plan step's choices are window state, never stored.** `PlanSurfaceState { materialOpt; criterion }` starts at
  `{ None; DeterminantPlanCriterion }`.
  - `planMaterials` lists the chosen study's opened materials whose physics is a crystal, in study order.
  - `planMaterialOpt` is the surface's material while that list holds it, else its first entry.
  - `SetPlanMaterial` is taken only for a version the list holds; otherwise the model is returned unchanged.
- **`planReportOpt`** is the committed report only while its study is the chosen study and its material is
  `planMaterialOpt`. Choosing another study hides the report, even when that study opens the same crystal.

## Deferred

Nothing. Every requirement and the three census facts landed this step. The Plan view, its UiIds and the composed
host are steps 0490–0492's own briefs.

## Gotchas

- **The fixture kit is in-memory, as the brief says.** `KitOpening.openedOver` is private and `openKitAt` opens only a
  disk folder. The fixture builds `OpenedKit` from the in-memory kit's public reads and loads it through `loadKit` over
  `initInverse`, the stores and imports `StudyWindowModelTests` uses.
- **Catalogue first.** The kit records QZ-Z, the probe quartz and the tabulated glass only after `putSealedEntries` has
  placed them in the catalogue store. The window's `tryItemModelChoice` then resolves QZ-Z from the same store.
- **The completion fact solves nothing.**
  - The window kit's `.oc/planner/` cache is seeded with literal rows, under the keys the window's own forward context
    gives. The plan through the posting host reads them and ends in milliseconds.
  - Why: the constructor suite runs 136 s of its 180 s deadline, and at step 0487 a real plan through the host missed the
    ten-second wait under load.
- **Checking a recorded job without solving.** Fact 1 runs each recorded job under an already-cancelled token. The job
  plans the window's QZ study and stops at "0 of 2 candidates were evaluated", so it proves what was recorded without a
  solve.
- **Shared field labels:** `PlanRequest`, `PlanReport` and `PlanSurfaceState` share `criterion` (the first two also
  `study` and `material`). Annotate a record literal's type.

## Changelog

- 2026-11-18 — Step 0488: the window plans a study's crystal material through `KitWindowShell.planRuns` (`StudyPlan`,
  `SetPlanMaterial`, `SetPlanCriterion`; `PlanSurfaceState`; `planMaterials` / `planMaterialOpt` / `planReportOpt`);
  three `PlanWindowModelTests` facts.
