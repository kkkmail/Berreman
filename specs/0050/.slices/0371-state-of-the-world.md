# Step 0371 — state of the world

## Where we are

Spec 0050 section S11.2 (the forward model of an experiment and the study residual). Steps 369–370 put an experiment's
pre-fit checks in `PrefitChecks`; this step opens `OpticalConstructor.Domain.StudyForward`, the last Domain file, with
what a detector records and each measured point paired with what fixes its prediction (001 §K, step 304). Later S11.2
steps add the predictions, the residual and their error cases to this module.

## What's working

- Add `StudyForward.RecordedReading`: a detector's signal, Ψ/Δ, or absolute Mueller matrix
- Add `StudyForward.MeasuredRow`: a point at its scan settings (and own wavelength), or an observation with its configuration
- Add `MappedPoints.measuredRowsFor`, placing every kind of mapped point on `scan.quantities` in order
- Refuse a point the scan cannot place with `StudyForwardError.UnpairedPointsErr`, naming the point and the counts
- Add `StudyForwardTests` with three facts (constructor suite 1620 → 1623)

## Tests

```yaml
gates:
  constructor_unit_tests: 1623
```

- build: exit 0, no warning from our code, 81 s (deadline 600 s).
- constructor-unit-tests: Passed!, 1623 succeeded (1620 at checkpoint 370, +3), 0 failed, 0 skipped, 76 s (deadline 180 s);
  the three new facts are `Passed` in the TRX.
- elevation from arc base `f395597c`: OK, 0 new findings in 13 projects, 231 s (deadline 600 s).
- The twenty s37 invariants: each `section_invariants --find-nothing` exits 0, and their patterns find nothing in the
  two new files.
- Nothing deferred. All durations are within their deadlines.

## Architecture

- `measuredRowsFor` maps each point to a `Result` and folds them with the shared `ItemEvaluation.sequenced` (order
  kept, first refusal wins), so it adds no second collector.
- It places points by `validatedFor`'s rules but does not re-validate ranges or units. Callers run it on points the
  pre-fit check has already validated.
- An abscissa becomes a scan value through `MeasuredAbscissa.toSweptValue`, a named member on the source type (ELEV001),
  declared in `StudyForward.fs`.

## Deferred

Nothing.

## Gotchas

- An ellipsometric point is never refused. It sets the scan's first `VaryR2` quantity when it has an angle of incidence,
  and otherwise has no setting. Only an intensity series point or a capture row under two or more quantities is refused.
- Angles are read through `Angle.degrees` without rounding (60° reads 59.999999999999993), as `validatedFor` reads them.
- A column row is refused when its cell count differs from the quantity count. Under a dark line, only a row of no cells
  is placed.

## Changelog

- 2026-11-13 — Step 0371: `StudyForward.fs` with `StudyForwardError`, `RecordedReading`, `MeasuredRow` and
  `MappedPoints.measuredRowsFor`, plus `StudyForwardTests` (constructor suite 1620 → 1623).
