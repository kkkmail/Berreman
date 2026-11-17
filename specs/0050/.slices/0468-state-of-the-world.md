# Step 0468 — state of the world

## Where we are

Spec 0050 S14.1, the Measurement Planner's layer 1. Steps 465–466 gave the symmetry tables (`PlannerSymmetry.fs`)
and step 467 the look model (`MeasurementPlanner.fs`: `PlannerLook.sightOf`, what one look sees of one tensor
component). This step adds the answer over a set of looks: whether a target is reachable at all, the reason when
it is not, and the Snell cap that bounds how far any tilt carries the wave off a plate's normal.

## What's working

- Added `UnseenReason` (five reasons, each with its report sentence) and `TargetSight` to `MeasurementPlanner`
- `CrystalParameters.targetSightOf` answers reachable when any look sees a target, else the closest look's reason
- `CrystalParameters.snellCap` is `internalAngle` of the smallest principal index at `IncidenceAngle.maxValue`
- Added three `PlannerReportTests` facts (constructor suite 1848 → 1851)

## Tests

Development runs (the arc-runner's gate engine is the authority; these are the worker's own checks):

- build: succeeded, 0 errors, 25 s (second attempt; the first stopped on a missing `open` in the test file);
  warnings only the exempt `NU1902` and the vendored MathNet `SYSLIB0051` that predates the arc.
- elevation: `OK - 0 new finding(s) in 13 project(s)`, 243 s (deadline 600 s).
- constructor-unit-tests: 1851/1851 passed, 112 s (deadline 180 s); +3 over checkpoint 1848, exactly the three
  new facts. The class alone, filtered: 8/8.
- The twenty-one s45 invariants: no added line contains any of their patterns, and no other tracked file changed.

No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1851
```

## Architecture

- `targetSightOf` reads only step 467's `sightOf`: it takes every look's sight and walks a closest-first table
  (seen → reachable; then reach misses → `FullMuellerNeededReason`, no retardance → `NoRetardanceReason`, normal
  incidence → `NormalIncidenceOnlyReason`); a target every look sees along its propagation falls through to
  `AlongEveryPropagationReason`. An empty look list is `NoLookReason` before any sight is taken.
- `UnseenReason` and `TargetSight` are derived, never stored, so neither carries an int wire tag.
- `snellCap` takes the minimum of the record's own `RefractionIndex` fields (n_o and n_e; index1 to index3) and
  reuses `MuellerInverse.internalAngle` at `IncidenceAngle.maxValue` (89°); nothing is re-derived.

## Deferred

Nothing. Every requirement of the brief landed in this step.

## Gotchas

- `component` is F#-reserved: the brief's `(component : TensorComponent)` parameter is `tensorComponent`.
- `RefractionIndex` is a plain single-case union over `double`, so `List.min` orders it structurally; no `.value`
  is read.
- The closest-first table is a local `(LookSight * TargetSight) list` read by `List.tryPick`: two distinct
  element types (ELEV011) and no two-case union projected to a `bool` (ELEV012).
- The tests reach components by name through a private `componentNamed`, which `sightsOf` now calls too; step
  467's five facts are unchanged.

## Changelog

- 2026-11-17 — Step 0468: added `UnseenReason`, `TargetSight`, `CrystalParameters.targetSightOf` and
  `CrystalParameters.snellCap` to `MeasurementPlanner.fs`, and three `PlannerReportTests` facts; constructor
  suite 1851.
