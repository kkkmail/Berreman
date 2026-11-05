# Step 0009 — IMPLEMENT — state of the world

## Where we are

This is step 9 of spec 0050, section S1.1 ("Every dispersion axis is data: tabulated and
transcendental cases"). Steps 2, 5, 7 and 8 moved every producer of `EpsAxisEvaluated` to a data
case: tabulated imports, the four transcendental models, and the coded Silicon / Langasite
presets. This step deletes the case and `EpsAxisDispersion`'s custom equality, so every per-axis
dispersion is data and the union takes F#'s structural equality (001 §B; .spec-md §0.1, first
row; obligation `S11-NO-CLOSURE`). No file under `Berreman/` names `EpsAxisEvaluated`, which S1.2's
invariant `s02-no-eps-axis-evaluated` requires.

## What's working

- Deleted `EpsAxisEvaluated` and the custom `Equals` / `GetHashCode`; `EpsAxisDispersion` now has F#'s structural equality
- Rewrote the union's doc comment to list its data cases: term data, the tabulated table, the four oscillator laws and the published laws
- Deleted the dead `parameters` / `facetKey` / `isEditable` arms; `isEditable` now reads "editable exactly when every axis is `RealNK` or `ComplexEps`"
- Out-of-band fixtures build a constant `RealNK` (n = 1.5, k = 0) data axis through the `ConstantNK` lowering
- Added the `EpsWithDispValueTests` fact: equal data of all eight cases is equal and hashes equal, and one changed value breaks equality

## Tests

These runs are local observations, made to confirm the change. The arc-runner's gate engine runs
and records the gates.

- `build`: 0 errors. The only warnings are the two vendored MathNet SYSLIB0051 ones that were
  already on the start tree.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 11 project(s)`.
- `unit-tests` (EpsWithDispValueTests, DispersionFormulaTests): 30/30 succeeded, 29 before plus
  the new fact.
- `constructor-unit-tests` (whole suite): 734/734 succeeded.
- `ui-tests` (OutOfBandBadgeTests, MaterialEditorWindowTests, MaterialsWindowTests): 66/66
  succeeded.
- `ui-smoke`: 208/208 succeeded.
- The three s01 invariants (`elev-list-empty`, `no-vstest`, `no-kit-kit-json`) and the `s02`
  `EpsAxisEvaluated` grep each find nothing.

```yaml
gates:
  berreman_unit_tests:     30
  constructor_unit_tests: 734
  ui_smoke_tests:         208
  ui_tests:                66
```

`berreman_unit_tests` and `ui_tests` are the counts under this step's class filters.

## Architecture

- **`EpsAxisDispersion` is pure data.**
  - It has eight cases: `RealNK`, `ComplexEps`, `TabulatedNK`, `TaucLorentzAxis`,
    `GaussianOscillatorAxis`, `ForouhiBloomerAxis`, `BrendelBormannAxis` and `PublishedLawAxis`.
  - It carries no attribute. Equality and hashing are F#'s derived structural ones, which
    compare arrays element-wise.
  - Comparison is inferred as unsupported (`Complex` is not `IComparable`), so nothing changes
    for callers.
- **Editability is a positive predicate.** `MaterialComplexity.isEditable`: a constant eps is
  editable, and a dispersive eps is editable exactly when every axis is `RealNK` or `ComplexEps`.
  The match lists every case, so a new axis case forces a decision.
- **The facet key is unchanged.** The four transcendental laws and the published laws share
  "Evaluated (transcendental)". That is the facet's name, not the deleted case.

## Deferred

- Nothing in this step's scope is deferred.

## Gotchas

- **Stale comments outside this step's effects.** They still call the coded presets' eps an
  "evaluated closure", or say the transcendental models lower to "the evaluated case". None names
  `EpsAxisEvaluated`. The impl-log lists them:
  - `MaterialEditorView.fs:362`, `MaterialsWindowView.fs:257` and `OutOfBandDiagnostic.fs:21`
  - the test comments in `MaterialEditorWindowTests`, `LibraryFacetsTests` and
    `MaterialComplexityTests`
  - two `DispersionModelsTests` fact names
- **The new fact is protective, not red.** The deleted `Equals` override already compared the
  data cases structurally. The fact pins that behaviour on F#'s derived equality.
- **The fixtures' k formula holds one zero term.** It is `ConstantNK`'s lowering, which differs
  from `zeroFormula`'s empty term list but evaluates to the same k = 0. The out-of-band
  diagnostic reads only the segment intervals.

## Changelog

- 2026-11-05 — step 0009: deleted `EpsAxisEvaluated` and `EpsAxisDispersion`'s custom equality,
  so every axis is data under F#'s structural equality. Removed the dead Domain arms and the docs
  that named the case. The out-of-band fixtures now use a data axis. Added the
  structural-equality fact.
