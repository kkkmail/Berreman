# Step 0483 — state of the world

## Where we are

Spec 0050, section S14.2: the Planner's numerical layer, the plan, the cache and the Plan view.

- Steps 0470–0472 gave the planner its inputs and structural report.
- Steps 0477–0479 gave a candidate's information.
- Step 0482 gave the kit's candidate experiments.

This step 0483 lands obligation S142-INSTRUMENT: the instrument-matrix condition number for choosing the polarizer and
compensator states (001 §M). For each polarizer of the kit, paired with each compensator, the planner:

- chooses the four compensator angles of the 15° grid whose generator matrix is best conditioned;
- reports the condition number of the 16 × 16 instrument matrix over them.

A polarizer with no compensator is told it needs a retarder, and a compound optic is never paired.

## What's working

- Add `PlannerError.InstrumentEvaluationErr`: an optic missing from the items, unresolved by the library, or refused
  by the item evaluator, named by its label with the cause.
- Add `InstrumentChoice` (polarizer, compensator, four angles, the instrument's `ConditionNumber`) with the planner's
  constant `compensatorAngleGrid` (0°, 15°, …, 165°).
- Add `InstrumentFinding` (`ConditionedInstrumentFinding` | `CompensatorMissingFinding`) and its plan sentence.
- Add `PlannerInputs.instrumentFindingsWith`, built from existing pieces:
  - the item evaluator at the first source's line;
  - `MuellerReconstruction.designRow`;
  - `ResidualJacobian.conditionNumber`.
  It chooses the least-conditioned four of the 495 grid subsets, and a tie keeps the earliest.
- Add the three `PlannerInputsTests` facts; constructor-unit-tests 1877 → 1880.

## Tests

- `build`: green, 84 s against a 600 s deadline. `.artifacts/0483-build-01.log`.
  - Its warnings are the exempt NU190x advisories and the vendored MathNet `SYSLIB0051` warnings. Those appear whenever
    MathNet recompiles, as in 0478–0482's first builds, and this step does not touch MathNet.
- `constructor-unit-tests`, the gate's own command over the whole suite (`all_discovered`): Passed!, succeeded 1880,
  failed 0, skipped 0, in 116 s against a 180 s deadline. `.artifacts/0483-constructor-unit-tests.log`.
  - The count rises by exactly 3 over checkpoint 482's 1877.
  - The three new facts pass under the census ids:
    - `a polarizer and a retarder give the four compensator angles of least generator condition, and the instrument's
      condition follows`;
    - `a polarizer alone is told it needs a compensator, and a compound optic is not paired`;
    - `an optic the library does not resolve refuses the instrument findings, naming it`.
- `elevation`: the gate's command with no base, so all 13 projects, ELEV004 excluded:
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0, 241 s against a 600 s deadline.
  `.artifacts/0483-elevation.log`.
- The 21 `invariant-s46-*` greps: each finds nothing (`.artifacts/0483-invariant-commands.txt`).
- No gate exceeded its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1880
```

## Architecture

- **Placement.** The new types follow step 482's `PlanCandidate` block in `MeasurementPlanner.fs`.
  `instrumentFindingsWith` sits in a `type PlannerInputs with` block in the same module, so it compiles as an intrinsic
  member: `MeasurementPlanner.PlannerInputs.instrumentFindingsWith`.
- **One evaluator.** Optics are read only through `ItemModelChoice.boundWith` and `BoundOpticalElement.muellerAt` at
  `ElementOrientation.transmittingAt θ`. Analyzer slots seen from behind (R0002) are a run-path concern and are not
  involved here: the analyzer row is the slice's row 0 of `M_P(0) · M_C(θ)`.
- **One condition seam.** Both the 4 × 4 generator matrix and the 16 × 16 instrument matrix are conditioned by
  `ResidualJacobian.conditionNumber`. The instrument matrix's rows are the linear engine's own `designRow`s, so its
  condition is the noise amplification of the reconstruction the engine performs.
- **Pure and derived.** The grid is a constant of the planner, not a setting. The findings are derived, never stored,
  so they carry no wire tag. No cache, store or option was added.

## Deferred

Nothing. Every requirement of the slice landed in this step.

## Gotchas

- `PlannerReportTests.fs` lies outside `effects.edits`, but its exhaustive `PlannerError` or-pattern had to gain
  `InstrumentEvaluationErr _`, because FS0025 is an error in the test project. It is a one-line edit with no assertion
  or identity changed: the precedent of steps 470, 471 and 477.
- Only paired optics are evaluated. A polarizer with no compensator is reported without reading its matrix, so a
  broken polarizer surfaces only once a compensator is in the kit.
- The tie rule relies on identical computation. Mirror-image angle sets are mathematically tied, and the earliest-set
  check in the test holds because the planner and the test build the same rows in the same order.

## Changelog

- 2026-11-18 — Step 0483: instrument findings (`InstrumentChoice`, `InstrumentFinding`,
  `PlannerInputs.instrumentFindingsWith`, `PlannerError.InstrumentEvaluationErr`) and their three `PlannerInputsTests`
  facts.
