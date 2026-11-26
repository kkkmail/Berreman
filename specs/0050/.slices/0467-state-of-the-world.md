# Step 0467 — state of the world

## Where we are

Spec 0050 S14.1, the Measurement Planner's layer 1. Steps 465 and 466 gave the symmetry tables
(`PlannerSymmetry.fs`: point groups, `CrystalAxis`, `TensorComponent`, `CrystalParameters.components`, the axis a
cut puts on the plate normal). This step adds the look model on top of them, `MeasurementPlanner.fs`: what one
measurement of a plate is, and what it sees of each tensor component the study engine fits. Step 468 builds on it
(the Snell cap through `internalAngle`).

## What's working

- Added `MeasurementPlanner` (Domain): `DetectorReach`, `LookTilt`, `AxisAspect`, `LookSight` and `PlannerLook`
- `PlannerLook.sightOf` judges each component by the transverse, linear-retardance, full-Mueller and off-normal rules
- `MeasurementConfiguration.plannerLookWith` reads a spec 0044 design's configuration as a look, nothing transcribed
- Added `PlannerReportTests` with the five facts of the brief (constructor suite 1843 → 1848)

## Tests

Development runs (the arc-runner's gate engine is the authority; these are the worker's own checks):

- build: succeeded, 0 errors, 81 s; warnings only the exempt `NU1902` and the vendored MathNet `SYSLIB0051`
  that predates the arc.
- elevation: `OK - 0 new finding(s) in 13 project(s)`, 237 s (deadline 600 s).
- constructor-unit-tests: 1848/1848 passed, 109 s (deadline 180 s); +5 over checkpoint 1843, exactly the five
  new `PlannerReportTests` facts. The class alone, filtered: 5/5.
- The twenty-one s45 invariants: the new files contain none of their patterns; no tracked file they scan changed
  beyond the two `<Compile>` lines.

No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1848
```

## Architecture

- The rules speak the engine's bi-anisotropic ρ (the `MuellerInverse.fs` header), not the crystallographic g:
  g33 is unseen along its own axis, and `gyrationProjection` is not called.
- `aspectOf` is the one place "transverse" is decided: only the normal axis of a normal-incidence look is
  longitudinal. Every rule in `sightOf` reads it, so a plate with no principal axis on its normal
  (`normalAxisOpt = None`) has every axis transverse, and the Planner never calls a component unseen through it.
- Linear retardance at normal incidence is read from the crystal's own `components`: two or more distinct
  `PrincipalIndexComponent`s on the transverse axes. A uniaxial z-cut has one (n_o on both), so it is a pure
  rotator and n_o is `NoRetardanceInLook`.
- `LookSight` is declared before `PlannerLook` (the brief lists it sixth) because `sightOf` returns it.

## Deferred

Nothing. Every requirement of the brief landed in this step.

## Gotchas

- `component` is F#-reserved: the `sightOf` parameter is `tensorComponent`.
- The facts reuse the test project's literal `CrystalMaterialFixtures`, as step 465 did, and look each parameter
  up by its `ParameterName` in `crystal.components`.
- No two-case union is mapped to a `bool` (ELEV012): transverse axes come from `List.choose` over `AxisAspect`,
  and "two or more" is the list pattern `_ :: _ :: _`.
- Normal incidence is `incidence.value = IncidenceAngle.normal.value`. `IncidenceAngle.create` maps 0° to exactly
  0.0, so a design written with `IncidenceAngle.create (Angle.degree 0.0)` is a normal look.

## Changelog

- 2026-11-17 — Step 0467: added `MeasurementPlanner.fs` (looks, detector reach, per-component sight,
  `plannerLookWith`) and `PlannerReportTests` (5 facts); constructor suite 1848.
