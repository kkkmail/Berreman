# Step 0011 — IMPLEMENT — state of the world

## Where we are

This is step 11 of spec 0050, in section S1.2 ("A material's physics is its complexity"), under
obligation `S12-DATA-FIXTURES`. After step 10, every engine preset can be stated as a
`MaterialComplexity`. This step makes every place that builds a `MaterialEntry` from engine
properties state its physics as data first, while `properties` is still a field (.spec-md §3.2).
Those places are the script-acceptance fixtures, the minted-id fact and the library-file import.
Step 13 then derives `properties` from the complexity, and these comparisons have already shown
that no number moves.

## What's working

- The Fsx acceptance fixtures now store every material as `MaterialComplexity` data: a Sellmeier lowering, the Langasite and Silicon published laws, constant glasses and vacuum, and absorbing EUV layers
- The script side of every comparison still builds its own properties, and `euvComplex` is deleted
- A metadata-only library file now re-imports each entry as vacuum term data (`vacuumPlaceholderComplexity`), editable like any constant material
- The minted-id fact now resolves a constant complexity and compares eps by value at 400, 600 and 800 nm
- Added the fact `a metadata-only library file re-imports each entry as vacuum data`

## Tests

These runs are local observations, made to confirm the change. The arc-runner's gate engine runs
and records the gates.

- `build`: 0 errors. The only warnings were the two vendored MathNet SYSLIB0051 ones that were
  already on the start tree.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 5 project(s)`. Storage changed, and its four
  dependents were analysed with it. The test files are outside the analyzers' scope.
- `constructor-unit-tests` (whole suite): 737/737 succeeded. That is 736 plus the new
  ExportImportTests fact; the reworked minted-id fact passes too.
- `ui-tests` (FsxAcceptanceTests): 14/14 succeeded. Every script comparison is within its 1e-6
  tolerance.
- `ui-smoke`: 208/208 succeeded.
- The four s02 invariants (`elev-list-empty`, `no-vstest`, `no-kit-kit-json` and
  `no-eps-axis-evaluated`) each find nothing.

```yaml
gates:
  constructor_unit_tests: 737
  ui_smoke_tests:         208
  ui_tests:                14
```

`ui_tests` is the count under this step's FsxAcceptanceTests filter.

## Architecture

- **`vacuumPlaceholderComplexity` is public in `OpticalConstructor.Storage.MaterialLibrary`.**
  The new ExportImportTests fact compares with that one declaration, so the vacuum value is not
  transcribed a second time. It is an elevated domain value, so ELEV allows it as a public name.
- **The table side reads `complexity.toProperties`; the script side keeps the script's own
  construction.** Equal curves within 1e-6 therefore show that the data states the script's
  physics.
- **Each script material's segment band is the script's own sweep.** Glass_Dispersive is
  300–700 nm. Langasite and Silicon share 250–600 nm (`langasiteOnSiliconSweep`).

## Deferred

- Nothing in this step's scope is deferred.

## Gotchas

- **`euvComplex` is deleted, and `euvLayerIndex` replaces it.** The slice deletes the
  properties-building helper. The EUV facts still need n = 1 − δ + iβ in two places each, so
  a private `euvLayerIndex (delta) (beta) : ComplexRefractionIndex` builds only the index. The
  script layer's `Eps.fromComplexRefractionIndex >> OpticalProperties.fromEpsion` now lives in
  `euvStack`, as the slice asks.
- **The minted-id fact compares with `complexity.eps.toEpsWithDisp.getEps`.** That is the
  complexity's own eps, not `toProperties`, so the fact still holds once step 13 makes
  `properties` a derived member.
- **The new export/import fact writes under `AppContext.BaseDirectory`, as the slice asks.** The
  older facts in the same file still use `Path.GetTempPath()`. They are outside this step's
  effects and were left alone.
- **The `MaterialEntryDto` doc in `Report.fs` still says the file carries display metadata only.**
  That is still true; only the "view-only until §D.9" comment was stale, and it is deleted.

## Changelog

- 2026-11-05 — step 0011:
  - The Fsx acceptance fixtures, the minted-id fact and the library-file import state their
    physics as `MaterialComplexity` data.
  - Imported library entries carry `vacuumPlaceholderComplexity`.
  - One fact added.
