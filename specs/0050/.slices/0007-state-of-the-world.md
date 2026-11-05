# Step 0007 — CLOSE_RED — state of the world

## Where we are

This is step 7 of spec 0050, section S1.1 ("Every dispersion axis is data: tabulated and
transcendental cases"). Step 5 made Tauc-Lorentz and the Gaussian oscillator core law data. Step 6
declared RED_0003: equal Forouhi-Bloomer and Brendel-Bormann models must lower to equal axis data.
This step closes RED_0003.

Both models now lower to core law DATA (`ForouhiBloomerAxis` / `BrendelBormannAxis`). Their
kernels, and Weideman's Faddeeva series, moved from `DispersionModels.baseIndex` into the core, so
one implementation exists (.spec-md §3.1). Every `DispersionModel` now lowers to data.
`EpsAxisEvaluated` remains only for the coded Silicon and Langasite presets: step 8 turns those into
published laws, and step 9 deletes the case.

## What's working

- Moved the Forouhi-Bloomer and Brendel-Bormann kernels (and the private Faddeeva series) into the core as `ForouhiBloomerLaw` / `BrendelBormannLaw`
- `EpsAxisDispersion` gains `ForouhiBloomerAxis` / `BrendelBormannAxis`, compared and hashed structurally, so RED_0003 is green
- `toEpsAxis` lowers every model to data; the `EpsAxisEvaluated this.evaluate` lowering is gone
- `facetKey`, `isEditable` and `parameters` take the new cases; the entries stay view-only under the transcendental facet key
- Added two exact-literal law facts in `EpsWithDispValueTests` (G17 values from the start tree's `evaluate`)

## Tests

These runs are local observations, made to confirm the change. The arc-runner's gate engine runs
and records the gates.

- `build`: 0 errors. The first full build showed only the two vendored MathNet SYSLIB0051 warnings
  that were already on the start tree. The final rebuild: 0 warnings, 0 errors.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 11 project(s)`.
- `unit-tests` (EpsWithDispValueTests, DispersionFormulaTests): 26/26 succeeded. The two new law
  facts equal their literals exactly.
- `constructor-unit-tests` (whole suite): 734/734 succeeded. That includes the step-6 red, the
  changed AC-B6 and step-015 expectations, and the unchanged AC-B6 reference facts.
- `ui-smoke`: 208/208 succeeded.
- `ui-tests` (MaterialEditorWindowTests): 29/29 succeeded.
- The three s01 invariants (`elev-list-empty`, `no-vstest`, `no-kit-kit-json`) each find nothing.

```yaml
gates:
  berreman_unit_tests:     26
  constructor_unit_tests: 734
  ui_smoke_tests:         208
  ui_tests:                29
```

`berreman_unit_tests` and `ui_tests` are the counts under this step's class filters.

## Architecture

- **The four transcendental laws are core data.**
  - `ForouhiBloomerLaw` and `BrendelBormannLaw` live in `Berreman/Berreman/Dispersion.fs`,
    beside step 5's two laws. Each carries its coefficients and its `AbscissaKind`, and is marked
    `NumericCarrier`.
  - Each `complexIndex` is the former `baseIndex` arm, moved verbatim.
  - The Domain coefficient records lower through intrinsic `toForouhiBloomerLaw` /
    `toBrendelBormannLaw`. `baseIndex` answers the law's index, so `evaluate` and the lowered axis
    run the same kernel.
- **The Faddeeva function lives once, privately, in the core**, beside `BrendelBormannLaw`, its
  only caller. The Domain copy is deleted.
- **`Dispersion.fs` opens `MathNetNumericsMath`** (first, as `Fields.fs` does), so the moved
  bodies keep `Complex.create`.
- **The lowering stays isothermal.** `thermoOptic` is not carried into a law; `evaluateAt` still
  adds `dndT·(T − T₀)` on top of `baseIndex`.
- **Equality.** `EpsAxisDispersion`'s custom equality compares the new cases structurally (hash
  tags 5 and 6). Only `EpsAxisEvaluated` still compares by closure reference.
- **Library facet and editability.** `facetKey` files both law cases under the existing
  "Evaluated (transcendental)" key. `isEditable` treats them as data without editable term
  coefficients (view-only), as before; the step-015 fact now pins this.

## Deferred

- Nothing in this step's scope is deferred. The case `EpsAxisEvaluated` stays because the spec
  gives the Silicon and Langasite published laws to step 8 and the case's deletion to step 9.

## Gotchas

- The slice's line citations into `DispersionModelsTests.fs` (`:403-430`, `:534-552`) had drifted
  after step 6 hoisted the coefficient builders. The facts were found by name.
- Two fact ids still say "evaluated", because the ids are fixed by the step's effects: `AC-B6
  ForouhiBloomer and BrendelBormann lower to the evaluated case and match evaluate` and
  `ForouhiBloomer and BrendelBormann are pickable and lower to the evaluated segment`. They now
  match the law cases.
- The new law records share field labels (`a`, `b`, `c`, `bandGap`, `strength`, `resonance`,
  `broadening`, …) with the Domain coefficient records and with `TaucLorentzLaw`. Every
  construction site is annotated or constructor-directed. Keep it that way, or record-label
  inference may pick the wrong type.

## Changelog

- 2026-11-05 — step 0007: closed RED_0003. Forouhi-Bloomer and Brendel-Bormann models lower to the
  core laws `ForouhiBloomerAxis` / `BrendelBormannAxis`. Their kernels and the Faddeeva series moved
  into the core, and no model lowers to `EpsAxisEvaluated` any more. Added two exact-literal law
  facts.
