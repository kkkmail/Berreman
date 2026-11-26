# Step 0004 — ADD_RED — state of the world

## Where we are

This is step 4 of spec 0050, section S1.1 ("Every dispersion axis is data: tabulated and
transcendental cases"). Step 2 made a tabulated table an axis case. Step 3 moved `AbscissaKind`
into the core, so a transcendental law can carry its abscissa as data. This step declares RED_0002.
An analytic model lowers to data, so equal Tauc-Lorentz and Gaussian-oscillator models must lower
to equal axes (001 §B, .spec-md §3.1). Today both models lower to `EpsAxisEvaluated this.evaluate`,
a fresh closure on every call, and those closures compare by reference. Step 5 (CLOSE_RED) turns
both models into the core law cases `TaucLorentzAxis` and `GaussianOscillatorAxis`, which compare
structurally.

## What's working

- Added the failing-first fact `equal Tauc-Lorentz and Gaussian-oscillator models lower to equal axis data` (RED_0002)
- The fact checks equal axes and hashes for one model lowered twice and for two separately built models, equal `toEpsValue`, and an unequal axis when `bandGap` / `energy` shifts
- Hoisted the AC-B5 Tauc-Lorentz and Gaussian coefficient sets into shared builders, reused rather than copied
- Observed the red: only the new fact fails, and on its first axis equality; the 29 earlier DispersionModelsTests facts pass

## Tests

These runs are local observations made to record the red. The arc-runner's gate engine runs and
records the gates.

- `build`: exit 0 with 0 errors. The only warnings are the two vendored MathNet SYSLIB0051
  warnings that were already on the start tree.
- `constructor-unit-tests` (DispersionModelsTests): 30 facts, 1 failed, 29 succeeded. The failure
  is the declared red,
  `equal Tauc-Lorentz and Gaussian-oscillator models lower to equal axis data`, with the message
  `Tauc-Lorentz: one model lowered twice gave unequal axes (EpsAxisEvaluated <fun:get_evaluate@573> vs EpsAxisEvaluated <fun:get_evaluate@573>)`.
- The whole constructor suite: 733 facts, 732 succeeded, 1 failed (the declared red).
- `elevation`: `ELEV-GATE: OK`. No in-scope project was touched, because test projects are
  outside the ELEV scope.
- The three s01 invariants each find nothing.

```yaml
gates:
  constructor_unit_tests: 732
```

## Architecture

- There is no production change, so this step makes no architectural decision. The fact's
  contract for step 5 is this: `EpsAxisDispersion` equality and hashing, and `EpsWithDispValue`
  equality, are structural for the Tauc-Lorentz and Gaussian-oscillator lowerings, and are
  sensitive to `bandGap` and `energy`.
- The fact compares only through `=` and `hash`. It names no case that does not exist yet, so it
  holds whichever data case step 5 lowers to.

## Deferred

- Nothing in this step's scope is deferred. The green belongs to step 5 (CLOSE_RED, closes
  RED_0002), as the spec orders it.

## Gotchas

- The AC-B5 transcendental fact now builds its models through the shared
  `taucLorentzCoefficients ()` / `gaussianOscillatorCoefficients ()`. Its match, its assertions and
  its comment are unchanged. Step 5's line citations (`:300-337`, `:302-305`, `:378-379`) have
  drifted; the fact names locate them.
- The builders are functions, so each call builds a separate coefficient record. "Two separately
  built equal models" therefore really are separate allocations.

## Changelog

- 2026-11-05 — step 0004: declared RED_0002. Equal Tauc-Lorentz and Gaussian-oscillator models
  must lower to equal axis data, and today they fail on closure reference equality. The AC-B5
  coefficient sets are now shared builders.
