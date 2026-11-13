# Step 0379 — state of the world

## Where we are

This is spec 0050 section S11.2, the forward model of an experiment and the study residual.

- **Steps 372–376** built the forward model and the row residuals.
- **Step 377** added the study's pre-fit.
- **Step 378** gathered the study's data: each fitted run's rows and each session's dark level.
- **This step** closes the residual. `Study.residualWith` is one weighted vector over every enabled fitted run, at a
  parameter vector laid out by the study's generated axes.
- **S11.3's engines** will minimise this vector. **S12.2** asserts on AT-2 that, for polarimeter observations at unit
  weight, it is the harness's `throughputResidualFor`.

## What's working

- Add `Study.residualWith context layout data vector`: one `ResidualVector` over `data.runs` in study order
- Each run is bound at its assignment, with its session's dark level, and every entry is multiplied by
  `residualWeightOf` (experiment × detector)
- Lay out every row's element entries run after run, then every Mueller row's throughput entry: `throughputResidualFor`'s
  layout
- Stop the walk at the first error, with no cache, scaling or penalty
- Add three `StudyForwardTests` facts: the mixed study, the weights and disabled run, and the polarimeter study against
  `MuellerFitting.throughputResidualFor` (constructor suite 1645 → 1648)

## Tests

```yaml
gates:
  constructor_unit_tests: 1648
```

| Gate | Result | Time (deadline) |
|---|---|---|
| build (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`) | exit 0, `0 Error(s)`, no lowercase `error`. The warnings are only the exempt NU1902 advisories. The first full build also showed vendored MathNet's `SYSLIB0051`, from a file this step does not touch | 58 s (600 s) |
| constructor-unit-tests (the gate's own command, whole suite) | `Test run summary: Passed!`, 1648 succeeded (1645 at checkpoint 378, +3), 0 failed, 0 skipped. In the TRX, the three new facts are `Passed` | 78 s (180 s) |
| elevation (from arc base `f395597c`, ELEV004 excluded) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0, on the final source. The arc's `Berreman.slnx` change widens the run to all 13 projects | 223 s (600 s) |
| the twenty s37 invariants | each prints `INVARIANT HOLDS` and exits 0 | seconds each (300 s) |

These are development runs. The supervisor's gate engine is the authority. Nothing is deferred, and every duration is
within its deadline.

A development-only mutation (weight dropped, throughputs interleaved) failed the weight fact and the polarimeter fact.
The source was restored before the final runs above.

## Architecture

`Study.residualWith (context : StudyForwardContext) (layout : StudyParameterLayout) (data : StudyData)
(vector : StudyParameterVector) : Result<ResidualVector, StudyForwardError>` is the last declaration of
`StudyForward.fs`.

For each `StudyRun` of `data.runs`, in order:

1. **Assignment.** `layout.assignmentFor this context.kit.items name vector` (step 373).
2. **Dark level.** `data.darkOf` of the session in `layout.sessions` that lists the run (step 378). It is `None` when
   that session reads no Dark run, and then the detector's own `darkOffset` applies (step 374).
3. **Binding.** `run.experiment.boundWith context this assignment darkOpt` (step 375).
4. **Weight.** `this.residualWeightOf name bound.detectorItem` (step 365).
5. **Rows.** Each row's `bound.predictedReadingOf row` is taken `residualAgainst` its measured reading (step 376). A
   settings row carries that reading; an observation row's is `RecordedMueller observation.measured`. Both
   `elements` and `throughputOpt` are multiplied by the weight.

The vector is every row's elements, then every row's throughput, in the same order.

The walk is a fold through `Result.bind`, so no run or row after the first error is evaluated. The shared
`ItemEvaluation.sequenced` would have evaluated them all first, so it is not used here. A disabled run, a run that is
not fitted, and a Dark run add nothing, because `data.runs` holds none of them (steps 377, 378).

## Deferred

Nothing.

## Gotchas

- **A pre-fit reads the detector slot's library entry, not the item's model.** An ellipsometer item held as a
  `LibraryModelChoice` leaves the slot's `valueId` unset, which `detectorKindOf` reads as Intensity, so a Ψ/Δ file
  fails its check. The test's ellipsometer bench binds its detector slot to `PresetIds.detectorEllipsometer`. A
  kit-built ellipsometer setup needs the same binding for its data to pass.
- **The mixed fact cannot tell "throughputs last" from "interleaved".** Its one Mueller row is the last row, as the
  slice prescribes. The polarimeter fact, with three observations, pins the order.
- **Fixture refactors in `StudyForwardTests`:**
  - `attachingAt` now calls `attachingThrough`;
  - `contextOverData` now calls `holdingRuns`;
  - `studyDataOver` now calls `unchangedStatusesOf` and `studyDataIn`.

  No existing fact changed.
- **The test file now opens `OpticalConstructor.Optimization`**, for `MuellerFitting`. No name collided.

## Changelog

- 2026-11-13 — Step 0379: added `Study.residualWith`, the study residual.
  - It covers every enabled fitted run in study order, each entry times its run's weight, the Mueller throughputs
    last, and the first error stops the walk.
  - For polarimeter observations at unit weight it equals `throughputResidualFor`.
  - Three new facts (constructor suite 1645 → 1648).
