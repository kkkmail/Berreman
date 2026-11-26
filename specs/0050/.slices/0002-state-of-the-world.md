# Step 0002 — CLOSE_RED — state of the world

## Where we are

Step 2 of spec 0050, section S1.1 ("Every dispersion axis is data: tabulated and transcendental
cases"). It closes RED_0001 from step 1: a tabulated refractiveindex.info page or n,k CSV now
imports as axis data, one `TabulatedNK` table inside the `EpsWithDispValue` tree (001 §B,
.spec-md §3.1), instead of a closure with `complexity = None`. The interpolation kernel moved
from `MaterialImport` into the core, so only one implementation exists. The transcendental models
still lower to `EpsAxisEvaluated`. Later S1.1 steps turn them into data, and step 9 deletes that
case and the custom equality.

## What's working

- Added `TabulatedNK` to the core axis type, with `TabulatedNkTable`, `TabulatedInterpolation` and `TabulatedTableError`
- Moved the linear, clamped tabulated interpolation from `MaterialImport` into `TabulatedNkTable.complexIndex`; no number moved
- Tabulated imports now carry one view-only isotropic segment over their sample range, with `properties = complexity.toProperties`
- Every match on the axis type handles the new case: no editable parameters, the "Tabulated n/k" facet key, opens view-only
- Added 7 facts: 5 core table facts, one facet-key fact and one import-table fact; step 1's two reds are now green

## Tests

Observed locally as development checks. The arc-runner's gate engine runs and records the gates.

- `build`: clean, 0 errors. The only warnings are the two vendored MathNet SYSLIB0051 warnings that were already on the start tree.
- `constructor-unit-tests` (whole suite): 732 / 732, step 1's two reds included.
- `unit-tests` (`EpsWithDispValueTests` and `DispersionFormulaTests`): 21 / 21, the five new facts included.
- `ui-smoke`: 208 / 208.
- `ui-tests` (`MaterialEditorWindowTests` and `NkDispersionChartTests`): 41 / 41.
- `elevation`: `ELEV-GATE: OK`, 0 new findings in 11 projects.
- The three s01 invariants each find nothing.

```yaml
gates:
  constructor_unit_tests: 732
  berreman_unit_tests: 21
  ui_smoke_tests: 208
  ui_tests: 41
```

## Architecture

- `TabulatedNkTable` is a `[<NumericCarrier>]` record with three columns:
  - `wavelengths : float<meter> array`, ascending;
  - `n` and `k` as `float array`;
  - `interpolation : TabulatedInterpolation`, a one-case union whose wire tag is the integer 1,
    read back by `tryOfWire`.
- `tryCreate` refuses an empty table, unequal columns and a wavelength column that decreases
  anywhere. It never sorts; sorting is the importer's job.
- `sampleRange` gives the first and last samples as `WaveLength.nm` values.
- `EpsAxisDispersion` gains `TabulatedNK` between `ComplexEps` and `EpsAxisEvaluated`. Under the
  custom equality it compares structurally (`t1 = t2`) and hashes with `hash (2, t)`.
- `NkTableRow`'s λ column is now `float<meter>`, so the importer hands the unit-typed column
  straight to the table.
- The `EpsAxisDispersion` doc comment now sits on that type rather than on `RealNkFormulas`.

## Deferred

- Nothing in this step's scope. The transcendental data cases, the removal of `EpsAxisEvaluated`
  and of the custom equality, and the JSON converter for `TabulatedInterpolation` belong to later
  steps of the arc (steps 3–9 and step 28), as the spec orders them.

## Gotchas

- A tabulated import now has a defined band, its sample range, so the out-of-band diagnostic
  flags a request outside it. The slice intends this: outside its range the table clamps.
- `sampleRange` builds its endpoints in nm. Compare a range by `.value` in metres, never by the
  `WaveLength` case: a µm fixture is equal in metres but is a different case.
- `tryCreate` checks the column lengths before emptiness. Three empty columns give
  `EmptyTableErr`; an empty wavelength column beside a non-empty n column is a count mismatch.
- `MaterialEditorView.formulaOfModel` falls back on any non-`RealNK` model, so a `TabulatedNK`
  axis passes through it safely.

## Changelog

- 2026-11-05 — step 0002: `TabulatedNK` axis case added to the core and built by the tabulated
  importer. The closure kernel was deleted and RED_0001 closed. Seven facts added.
