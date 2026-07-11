# Step 037 — impl-log (IMPLEMENT: inverse constructor state + collection builder)

## Progress

- [x] Domain: `ExperimentDataLoad.fs` (detector-kind resolve + load/validate) + fsproj register
- [x] Ui: `ConstructorMode` + `ExperimentFileStatus` DUs, Model fields, init threading, `initInverse`
- [x] Ui: chart-visibility gate (`experimentChartVisible`) on the draft chart surface
- [x] Ui: collection-builder messages + update arms + Ui-layer bay section + picker seam
- [x] App: `AppContext` two proxies + `initMainWith` record-update threading in `MainConstructorWindow`
- [x] Ui.Tests: inverse init, hint restores chart, attach status (×4), collection round-trip, ui-smoke
- [x] Local build + all gate suites green (advisory; arc-runner is the gate authority)

## Files modified

- **Domain (new):** `OpticalConstructor.Domain/ExperimentDataLoad.fs` — pure
  `detectorKindOf` + `loadAndValidate` (load through `ExperimentDataProxy` in the detector-kind
  shape, then `MeasuredData.validateAgainstExperiment`). Registered last in the Domain fsproj.
- **Ui:** `OpticalConstructor.Ui/TableAndElementRotationView.fs` —
  - `ConstructorMode` + `ExperimentFileStatus` DUs (before `Model`); 7 new `Model` fields
    (`constructorMode`, `experimentData`, `experimentCollections`, `collectionName`,
    `savedCollections`, `collectionStatus`, `experimentDataStatus`).
  - `initWith` gains the 2 proxy params; `defaultExperimentProxies` + `mainPalette` +
    `mainSeedElements` helpers; `initMainWith`/`initInverse` (5 proxy args, default experiment
    proxies internally); `initInverse` seeds src + UNBOUND sample + det, inverse mode.
  - `experimentChartVisible` gate; `experimentResult`/`chartForExperiment`/`experimentPsiDelta`
    return empty when gated off.
  - `Msg`: `CollectionSetName`/`CollectionSave`/`CollectionLoad`/`AttachDataFileTo`; update arms +
    `measuredSeriesPointCount`/`classifyDataError` helpers.
  - `CollectionIds` module, `pickDataFile` picker seam, `attachDataFileButton`, `dataFileRow`,
    `collectionBuilderView`, `experimentsBayContent`; experiments bay now renders
    `experimentsBayContent` (Controls `ExperimentControls.view` untouched, stacked with the
    Ui-layer builder). Added `open …ExperimentCollectionStore` (resolves `CollectionName.tryCreate`).
- **Ui:** `OpticalConstructor.Ui/AppContext.fs` — `experimentData` + `experimentCollections` fields,
  built in `create` (`ExperimentDataStore.createFileBacked` + `ExperimentCollectionProxy.createInMemory`).
- **App:** `OpticalConstructor.App/Program.fs` — `MainConstructorWindow` threads the two app-scope
  proxies onto the surface by record update over `initMainWith`.
- **Ui.Tests (new):** `InverseConstructorTests.fs` (11 facts, 1 `ui-smoke`); `AppContextTests.surfaceOf`
  threads the two proxies by record update; Ui.Tests fsproj registers the new file.

## Testing state

`commit_ready: true`. Local diagnostic runs (advisory — the arc-runner gate engine is the sole
authority; Invariant 6):

- **build** (whole solution, Release): green — 0 errors; no new warnings from touched projects (only
  the step-001-catalogued pre-existing NU1701 / SYSLIB0051 / FS0044 / FS3873 / FS1125 remain).
- **unit-tests** (BerremanTests): 119 passed / 5 pre-existing skips — unaffected (solver untouched).
- **constructor-unit-tests** (OpticalConstructor.Tests): 659 passed / 0 failed — unchanged (the step
  does not touch that project; the new Domain helper is exercised through the Ui.Tests over mocks).
- **ui-smoke**: 171 passed (170 → +1, the inverse-builder render proof).
- **ui-tests**: 446 passed (436 → +10, the inverse/attach/collection facts).

Logs under `specs/0038/.artifacts/037-*.log`.

## Artifacts

- `037-build-01.log` — first full build (caught 3 collision errors, fixed).
- `037-inverse-tests-01.log`, `037-constructor-tests-01.log`, `037-ui-smoke-01.log`,
  `037-ui-tests-01.log` — gate-suite runs.
