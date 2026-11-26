# Step 0470 — state of the world

## Where we are

This is spec 0050 S14.1, the Measurement Planner. Steps 465–469 built layer 1 (symmetry, looks and the structural
recoverability report) over a crystal and a set of looks. This step adds the planner's first input from a kit: the lab
profile. Following 001 §M, nothing is typed in. The kit's items are read through their models into sources, optics and
detectors, each detector with its noise model, and the reach of each detector is derived from them. Step 471 adds the
plates, the prior and the targets.

## What's working

- Added `LabProfile.ofItems`, which reads each kit item's model, through the caller's `choiceOf`, into sources, optics or detectors
- Detectors carry their own noise values, checked against the model's noise axes; an edited value is read, not the default
- An item the library does not resolve, or a refused noise value, refuses the whole profile with `UnresolvedPlannerItemErr`
- `reachOf` gives an intensity detector a full Mueller reach only beside two polarizing optics; `reaches` lists each reach once
- Added three `PlannerInputsTests` facts (constructor suite 1855 → 1858)

## Tests

Development runs. The arc-runner's gate engine is the authority; these are the worker's own checks:

- build: succeeded, 0 errors. The first attempt stopped on FS0025 in `PlannerReportTests.fs` (see Gotchas). The final
  build took 19 s, and its only warning is the exempt `NU1902`.
- elevation: `OK - 0 new finding(s) in 13 project(s)` in 226 s (deadline 600 s), run over the final source.
- constructor-unit-tests: 1858/1858 passed in 109 s (deadline 180 s). That is +3 over checkpoint 1855, exactly the
  three new facts. The class alone, filtered, passed 3/3.
- The twenty-one s45 invariants: each decoded command finds nothing. The untracked new test file was also scanned
  directly against the same patterns and matched nothing.

No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1858
```

## Architecture

- `ofItems` reads models only through `choiceOf : Item -> ItemModelChoice option`, the same function the study's
  forward model and the window use, so model resolution is written in one place only.
- Each item becomes a one-item profile, or the empty one. The list goes through `ItemEvaluation.sequenced`, the
  shared ordered Result collector, and is then concatenated. This keeps item order, the first refusal is the answer,
  and nothing is ever answered in part. No intermediate type was needed.
- A compound is a `CompoundOptic` when any leaf is a `RealPolarizerModel`. The leaves are collected by recursion
  through nested compounds, and the test uses `List.tryPick`, so no `bool` appears. Otherwise the compound is a
  `CompensatorOptic`. Preset polarizers map by behavior: `ComputedIdeal` to polarizer, `ConstantMueller` to compound.
- A detector's `noise` is `ParameterValues.ofParameters item.parameters` checked against the model's noise axes.
  Step 486 resolves `noise.benchNoise` in Optimization; the Domain code neither opens nor references Optimization.
- `OpticRole` and `DetectorRecording` are derived and never stored, so they carry no wire tag.

## Deferred

Nothing. Every requirement of the brief landed in this step.

## Gotchas

- **One edit outside the listed effects.** Adding the mandated `PlannerError.UnresolvedPlannerItemErr` made step
  469's exhaustive match in `PlannerReportTests.fs` a compile error (FS0025). That match is in the fact `a target the
  crystal does not have refuses the report, naming it`. The fix is one explicit failing arm for the new case. No
  assertion was removed and no test identity changed.
- A bare binding form chosen as `LibraryModelChoice` (no preset or sample bound) is not covered by the brief's
  mapping. It refuses as `UnresolvedPlannerItemErr`, the same way `ItemModelChoice.boundWith` refuses that choice.
- For an intensity detector, `noise` also carries `gain` and `darkOffset`, because it holds all of the item's values,
  as the brief specifies. Step 486 reads the noise axes by name.
- The third fact also asserts the brief's noise refusals: an out-of-range `elementUncertainty` and a missing
  `throughputUncertainty` each name the item and the axis.
- `MeasurementPlanner.fs` now opens `Kit` and `ItemEvaluation`. The library's polarizer-behavior cases are written
  qualified (`Library.ComputedIdeal`) rather than by opening `Library`.

## Changelog

- 2026-11-17 — Step 0470: added `PlannerError.UnresolvedPlannerItemErr`, `PlannerSource`, `OpticRole`,
  `PlannerOptic`, `DetectorRecording`, `PlannerDetector` and `LabProfile` (`ofItems`, `reachOf`, `reaches`) to
  `MeasurementPlanner.fs`, and the `PlannerInputsTests` class with three facts; constructor suite 1858.
