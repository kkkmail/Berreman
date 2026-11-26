# Step 0486 — impl plan

## Approach

The Planner's run over a kit, in a new `PlannerRuns.fs` (`OpticalConstructor.Optimization`), compiled directly after
`PlannerInformation.fs`:

1. `MeasurementPlanner.fs`: `PlannerError` gains `PlanCancelledErr` ("the plan was cancelled: <reason>") and
   `PlannerForwardErr` ("the forward model refused a candidate: <reason>").
2. `PlannerRuns`, in the order the step gives:
   - (a) `PlanCandidateCount`, `PlanProgress`, `[<ReferenceEquality>] PlanObserver` (the shape of
     `OptimizerIterationObserver`);
   - (b) `PlanCandidate.configurations`: incidence outer, azimuth inner, through `MuellerFitting.configurationOf`;
   - (c) on `PlannerInputs`:
     - `scalingWith`: the targets' crystal axes centred at the prior crystal, each scaled to its unknown's
       `bounds.searchUnit`; a target not an unknown is `UnknownPlannerTargetErr`;
     - `forwardWith`: the study engine's `createBerremanForward` over `opticalProperties`;
     - `candidateRowsWith`: resolves the polarimeter's `noise.benchNoise` first (a refusal is
       `UnresolvedPlannerItemErr` naming the detector, the reason naming the axis), then the scaling, then
       `candidateRowsOver`; a `MuellerFitError` is `PlannerForwardErr`;
     - `priorRows`: one unit-noise row per target with a prior, `unit_k / σ_k` at its own coordinate.

     `scalingWith` and `priorRows` read one private walk of the targets' axes and unknown states.
   - (d) `PlanStanding`, `RankedPlanStep` and `PlanReport`.
   - (e) `PlannerInputs.planWith`, in this order:
     1. the structural report and the instrument findings;
     2. the scaling;
     3. each candidate's coverage;
     4. each candidate's rows, the cancellation read before each and the observer told after each;
     5. the baseline (priors, then the in-kit rows) with its bounds;
     6. `rankWith` of the open candidates, each step with its bounds.

     The first error stops the run.
3. `PlanRunTests.fs`, appended last, holds the five census facts over `PlannerInputsTests`' kit and a study freeing
   g11 and g33 within ±50 %. Its `StudyForwardContext` reads the Built-in library and material store, and the
   file-backed experiment data store over an in-memory kit, so no proxy is copied. Its `rowsOf` is either literal
   rows that count their calls, or `candidateRowsWith SolverParameters.defaultValue`. The fifth fact wraps that in
   `KitContext.cachedPlannerRows`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/PlannerRuns.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlanRunTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
- Consequential edits, as at steps 483 and 485:
  - the exhaustive `PlannerError` or-patterns in `PlannerInputsTests.fs` and `PlannerReportTests.fs`;
  - `private` dropped from the `PlannerInputsTests` kit bindings the new facts reuse (step 482's bench, run and
    sample axis, the crystal entry, QZ-X).

## Risks

- `MuellerFitError` has no render member. A private extension member in `PlannerRuns` renders it for
  `PlannerForwardErr`: `%A` is ELEV002, and a free function of one of our types is ELEV013.
- The rank-2 fact over the real forward depends on g33 being seen off normal incidence within the 1e-6 rank floor.
  Measured by the test.
- New names must be unique (ELEV014/016/019). I grepped the tree: none of them is taken.
