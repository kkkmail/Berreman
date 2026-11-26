# Step 0006 — ADD_RED — state of the world

## Where we are

This is step 6 of spec 0050, section S1.1 ("Every dispersion axis is data: tabulated and
transcendental cases"). Step 5 turned Tauc-Lorentz and Gaussian-oscillator into the core law cases
`TaucLorentzAxis` and `GaussianOscillatorAxis`. This step declares RED_0003 for the two
transcendental models that remain: equal Forouhi-Bloomer and Brendel-Bormann models must lower to
equal axis data (001 §B, .spec-md §3.1). Today both still lower to
`EpsAxisEvaluated this.evaluate`, a fresh closure on every call, and those closures compare by
reference. Step 7 (CLOSE_RED) adds `ForouhiBloomerAxis` and `BrendelBormannAxis`, so that no
model lowers to the evaluated case.

## What's working

- Added the failing-first fact `equal Forouhi-Bloomer and Brendel-Bormann models lower to equal axis data` (RED_0003)
- The fact reuses step 4's `assertLowersToEqualData`: equal axes and hashes, equal `toEpsValue`, and an unequal axis when `bandGap` / the first `broadening` shifts
- Hoisted the AC-B6 a-Si and gold coefficient sets into shared builders; `forouhiBloomerASi` / `brendelBormannGold` keep their names
- Observed the red: only the new fact fails, and on its first axis equality; the 30 earlier DispersionModelsTests facts pass

## Tests

These runs are local observations made to record the red. The arc-runner's gate engine runs and
records the gates.

- `build`: exit 0 with 0 errors. The only warnings are the two vendored MathNet SYSLIB0051
  warnings that were already on the start tree.
- `constructor-unit-tests` (DispersionModelsTests): 31 facts, 1 failed, 30 succeeded. The failure
  is the declared red,
  `equal Forouhi-Bloomer and Brendel-Bormann models lower to equal axis data`, with the message
  `Forouhi-Bloomer: one model lowered twice gave unequal axes (EpsAxisEvaluated <fun:get_evaluate@610> vs EpsAxisEvaluated <fun:get_evaluate@610>)`.
- The whole constructor suite: 734 facts, 733 succeeded (step 5's baseline), and 1 failed (the
  declared red).
- `elevation`: `ELEV-GATE: OK`. No in-scope project was touched, because test projects are
  outside the ELEV scope.
- The three s01 invariants each find nothing.

```yaml
gates:
  constructor_unit_tests: 733
```

## Architecture

- There is no production change, so this step makes no architectural decision.
- The fact sets this contract for step 7, for the Forouhi-Bloomer and Brendel-Bormann lowerings:
  - `EpsAxisDispersion` equality and hashing, and `EpsWithDispValue` equality, are structural.
  - They are sensitive to `bandGap` and to the first `broadening`.
- The fact compares only through `=` and `hash`. It names no case that does not exist yet, so it
  holds whichever data case step 7 lowers to.

## Deferred

- Nothing in this step's scope is deferred. The green belongs to step 7 (CLOSE_RED, closes
  RED_0003), as the spec orders it.

## Gotchas

- `forouhiBloomerASi` and `brendelBormannGold` are now built from
  `forouhiBloomerASiCoefficients ()` and `brendelBormannGoldCoefficients ()`, with the literals
  unchanged. Step 7's line citations (`DispersionModelsTests.fs:403-430`, `:534-552`) have
  drifted, to `:432-465` and `:570-588`. The names locate them.
- Each builder call makes a separate coefficient record. "A separately built equal copy" is
  therefore a separate allocation, and cannot pass by sharing a reference.

## Changelog

- 2026-11-05 — step 0006: declared RED_0003. Equal Forouhi-Bloomer and Brendel-Bormann models
  must lower to equal axis data, and today they fail on closure reference equality. The AC-B6
  coefficient sets are now shared builders.
