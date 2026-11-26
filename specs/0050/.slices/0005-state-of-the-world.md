# Step 0005 — CLOSE_RED — state of the world

## Where we are

This is step 5 of spec 0050, section S1.1 ("Every dispersion axis is data: tabulated and
transcendental cases"). Step 2 made a tabulated table an axis case. Step 3 moved `AbscissaKind`
into the core. Step 4 declared RED_0002: equal Tauc-Lorentz and Gaussian-oscillator models must
lower to equal axes. This step closes RED_0002.

Both models now lower to core law DATA (`TaucLorentzAxis` / `GaussianOscillatorAxis`), which
compares and hashes structurally. Their kernels moved from `DispersionModels.baseIndex` into the
core, so one implementation exists (.spec-md §3.1). Forouhi-Bloomer and Brendel-Bormann still
lower to the evaluated closure; their laws are step 7.

## What's working

- Moved the Tauc-Lorentz and Gaussian-oscillator kernels into the core as `TaucLorentzLaw` / `GaussianOscillatorLaw`, one implementation
- `EpsAxisDispersion` gains `TaucLorentzAxis` / `GaussianOscillatorAxis`, compared and hashed structurally, so RED_0002 is green
- Added `AbscissaKind.abscissaAt`, an elevated `AbscissaValue` bit-identical to `Units.fromMeters`
- `toEpsAxis` lowers both models to their law; `facetKey`, `isEditable` and `parameters` take the new cases
- Added three exact-literal law facts in `EpsWithDispValueTests` (G17 values from the start tree's `evaluate`)

## Tests

These runs are local observations, made to confirm the change. The arc-runner's gate engine runs
and records the gates.

- `build`: 0 errors and 0 warnings on the final rebuild. The first full build showed only the two
  vendored MathNet SYSLIB0051 warnings that were already on the start tree.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 11 project(s)`. The first run caught ELEV010
  on the slice's prescribed `abscissaAt : float`, which the elevated `AbscissaValue` fixes.
- `unit-tests` (EpsWithDispValueTests, DispersionFormulaTests): 24/24 succeeded. The three new law
  facts equal their literals exactly.
- `constructor-unit-tests` (whole suite): 733/733 succeeded. That includes the step-4 red, the
  two changed AC-B5 expectations, and the unchanged AC-B6 and LibraryFacetsTests facts.
- `ui-smoke`: 208/208 succeeded.
- `ui-tests` (MaterialEditorWindowTests, NkDispersionChartTests): 41/41 succeeded.
- The three s01 invariants (`elev-list-empty`, `no-vstest`, `no-kit-kit-json`) each find nothing.

```yaml
gates:
  berreman_unit_tests:     24
  constructor_unit_tests: 733
  ui_smoke_tests:         208
  ui_tests:                41
```

`berreman_unit_tests` and `ui_tests` are the counts under this step's class filters.

## Architecture

- **A transcendental law is core data.**
  - `TaucLorentzLaw` and `GaussianOscillatorLaw` live in `Berreman/Berreman/Dispersion.fs`.
    Each carries its coefficients and its `AbscissaKind`, and is marked `NumericCarrier`.
  - Each `complexIndex` is the former `baseIndex` arm, moved verbatim.
  - The Domain coefficient records lower through intrinsic `toTaucLorentzLaw` /
    `toGaussianOscillatorLaw`. `baseIndex` answers the law's index, so `evaluate` and the
    lowered axis run the same kernel.
- **The lowering stays isothermal.**
  - `thermoOptic` is not carried into a law.
  - `evaluateAt` still adds `dndT·(T − T₀)` on top of `baseIndex`.
- **The abscissa is elevated.**
  - `AbscissaKind.abscissaAt (w : WaveLength) : AbscissaValue` performs `Units.fromMeters`'
    arithmetic over the scale and numerator `ofUnit` derives.
  - `AbscissaValue` is a single-case DU over `float`.
- **Equality.** `EpsAxisDispersion`'s custom equality compares the law cases structurally (hash
  tags 3 and 4). Only `EpsAxisEvaluated` still compares by closure reference.
- **Library facet and editability.** `facetKey` files both law cases under the existing
  "Evaluated (transcendental)" key. `isEditable` treats them as data without editable term
  coefficients (view-only), as before.

## Deferred

- Nothing in this step's scope is deferred. Forouhi-Bloomer and Brendel-Bormann still lower to
  `EpsAxisEvaluated` because the spec gives their laws to step 7, not this step.

## Gotchas

- The slice prescribes `abscissaAt : float`, and ELEV010 rejects it (a bare `float` beside the
  elevated `WaveLength`). AGENTS.md rules that the analyzers win, so the member returns
  `AbscissaValue`. The arithmetic is unchanged and stays bit-identical: the exact-literal facts
  pass.
- `AbscissaKind.ofUnit` moved to just after `ThermoOptic` in `DispersionModels.fs`, so the
  coefficient records' intrinsic law members can see it. It is no longer in the "Lowering"
  section.
- The law records share field labels (`epsInf`, `amplitude`, `broadening`, …) with the Domain
  coefficient records. Every construction site is annotated or constructor-directed. Keep it that
  way, or record-label inference may pick the wrong type.
- The fact id `AC-B5 transcendental models lower to the evaluated case and match evaluate` still
  says "evaluated case", because the id is fixed by the step's effects. It now matches the law
  cases.

## Changelog

- 2026-11-05 — step 0005: closed RED_0002. Tauc-Lorentz and Gaussian-oscillator models lower to
  the core laws `TaucLorentzAxis` / `GaussianOscillatorAxis`, and their kernels moved into the
  core. The abscissa is elevated as `AbscissaValue` (ELEV010). Added three exact-literal law facts.
