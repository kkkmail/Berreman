# Step 0206 — state of the world (IMPLEMENT: the detector noise axes in `ItemModels`)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), closing on its S7 prerequisite
(001 §H, §I, §S6): the shared item contracts land before S7 so that copying an experiment can map its items.

- Step 205 moved `ParameterName` and `DialSign` into `ItemModels.fs`.
- This step adds the vocabulary a model publishes its parameters in — a value, a kind with a stable int wire tag,
  and an axis — and the detector's noise axes as data.
- The bench noise implementation does not move: its four types stay in `BerremanTests/InverseFitHarness.fs` until
  step 339 moves them to `Optimization/BenchNoise.fs` (001 §Q / §S10).

## What's working

- Add `ParameterValue` (one parameter's number in its kind's unit) to `ItemModels`.
- Add `ParameterKind` (angle, phase, fraction, scale, Mueller element, thickness, reading) with int wire tags 1–7,
  `tryOfWire` (0 and unknown tags → `None`) and a separate `.text` rendering.
- Add `ItemParameterAxis { name; kind; defaultValue }` and `ItemParameterAxis.detectorNoiseAxes`: rotation
  (angle), element and throughput (fractions) uncertainties, preserving the supplied values.
- Add fact `the detector noise axes keep their declared quantities` (constructor unit tests 1265 → 1266).

## Tests

These are development runs, recorded in `0206-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors.
- **constructor-unit-tests:** 1266 passed, 0 failed, 0 skipped (1265 → 1266, +1). The new test identity is `Passed`
  in the TRX.
- **elevation** (from the arc origin): `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects (ELEV004 excluded).
- **The fourteen `invariant-s22-*` greps:** each finds nothing.

```yaml
gates:
  constructor_unit_tests: 1266
```

## Architecture

- **`ItemModels` holds the whole parameter vocabulary** a model publishes (spec 0050 001 §I): `ParameterName`,
  `DialSign`, `ParameterValue`, `ParameterKind`, `ItemParameterAxis`. It still depends on nothing, so the library
  entries and the kit (compiled after it) can store axis descriptors.
- **A detector's noise model is three ordinary axes**, not a noise-budget record: `detectorNoiseAxes` projects three
  supplied values onto named, kinded axes. A detector model will carry that list; an instantiated item carries the
  corresponding ordinary parameter states (S7).
- **`ParameterKind`'s tag is the disk contract** (1-based in declaration order, 0 the unset sentinel); `.text` is the
  rendering and is never written as the tag (ELEV017).

## Deferred

Nothing in this step's scope.

- The detector item model that carries the axis list, and the item's parameter states, are S7 steps by the spec's
  plan; the bench noise types move at step 339, by the spec's plan.

## Gotchas

- **Domain must not name the bench-noise types**, not even in a comment: `AngleUncertainty`, `ElementUncertainty`,
  `ThroughputUncertainty` and `NoiseParam` are declared only in `BerremanTests/InverseFitHarness.fs` until step 339.
- **Wire tags are frozen by order.** A new `ParameterKind` case goes last with tag 8; reordering the cases changes
  stored files.
- **The red solution build recompiled the vendored `MathNetNumerics/Numerics`** and printed its two `SYSLIB0051`
  warnings (`Exceptions.cs`); the next solution build printed none.

## Changelog

- 2026-11-09 — step 0206: `ParameterValue`, `ParameterKind` (int wire tags, `tryOfWire`, `.text`) and
  `ItemParameterAxis` with `detectorNoiseAxes` added to `ItemModels.fs`; one new fact.
