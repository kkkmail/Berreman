# Step 0469 — state of the world

## Where we are

Spec 0050 S14.1, the Measurement Planner's layer 1. Steps 465–466 gave the symmetry tables, step 467 the look model
(`PlannerLook.sightOf`) and step 468 the answer over a set of looks (`CrystalParameters.targetSightOf`) and the Snell
cap. This step closes layer 1 with its output: the structural recoverability report, which answers outright "this kit
can never recover X" and "you need an off-axis cut", each finding with its remedy (001 §M).

## What's working

- Added `CrystalParameters.structuralReport`: reachable targets, one finding per unrecoverable target, and an off-axis-cut finding
- Remedies by reason over the crystal's distinct principal plates: add a missing plate, tilt (Snell-capped), or full Mueller
- Findings and remedies render as sentences; every angle is in degrees, one decimal, invariant culture
- An unknown target refuses with `UnknownPlannerTargetErr`, naming it and the crystal's parameters
- Added four `PlannerReportTests` facts (constructor suite 1851 → 1855)

## Tests

Development runs (the arc-runner's gate engine is the authority; these are the worker's own checks):

- build: succeeded, 0 errors, 63 s (second attempt; the first stopped on two missing `open`s in
  `MeasurementPlanner.fs`); the only warning is the exempt `NU1902`.
- elevation: `OK - 0 new finding(s) in 13 project(s)`, 236 s (deadline 600 s).
- constructor-unit-tests: 1855/1855 passed, 109 s (deadline 180 s); +4 over checkpoint 1851, exactly the four new
  facts. The class alone, filtered: 12/12.
- The twenty-one s45 invariants: no added line contains any of their patterns, and no other tracked file changed.

No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1855
```

## Architecture

- `structuralReport` reads only earlier layer-1 members: `components` (the targets), `targetSightOf` (reachable or
  the reason), `sightOf` (whether a candidate plate sees a component, through a normal-incidence, transmitted,
  full-Mueller look at its `SampleCut.normalAxisOpt`), `snellCap` and `symmetry`. Nothing is cached or re-derived.
- Unknown targets go through `ItemEvaluation.sequenced`, the shared ordered Result collector, so the first unknown
  name answers.
- The candidate plates are `[ZCut; XCut]` for a uniaxial crystal (a y-cut sees it as an x-cut does) and all three
  principal cuts otherwise. Only plates the looks lack are offered for along-propagation or no-retardance targets.
  An off-diagonal component seen only off normal incidence gets the tilt alone.
- The off-axis-cut finding fires when the looks' distinct normals are exactly `[ Some a ]`. Its targets are those
  whose component lies on `a` alone, and its cuts are the candidates with a different normal.
- One private `Angle.plannerDegreesText` renders every angle in a sentence, so the one-decimal invariant format is
  written once. It is private, so it adds no public surface beyond the step's contracts.
- `PlannerRemedy`, `PlannerFinding` and the report are derived, never stored, so they carry no wire tag.

## Deferred

Nothing. Every requirement of the brief landed in this step.

## Gotchas

- `MeasurementPlanner.fs` now opens `MaterialComplexityEditor` (for `Anisotropy`'s cases) and `ItemModels` (for
  `ParameterName`), in the same order as `PlannerSymmetry.fs`.
- The uniaxial probe's Snell cap renders `40.4°` (40.36°), the triclinic probe's `41.1°` (41.13°, smallest index
  1.52).
- The unknown-target reason is `<name> (it has <names, ", ">)`; the fact checks it names the first unknown target, not
  a later one.
- The tests reach a whole parameter through a private `crystalComponentNamed`, which step 467's `componentNamed` now
  calls; the eight earlier facts are unchanged.

## Changelog

- 2026-11-17 — Step 0469: added `SampleCut.plateText`, `PlannerRemedy`, `TargetFinding`, `OffAxisCutNeed`,
  `PlannerFinding`, `RecoverabilityReport`, `PlannerError` and `CrystalParameters.structuralReport` to
  `MeasurementPlanner.fs`, and four `PlannerReportTests` facts; constructor suite 1855.
