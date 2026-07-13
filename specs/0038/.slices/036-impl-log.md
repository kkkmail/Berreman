# Step 036 — impl-log (IMPLEMENT_CONTRACT STORE_XDUO_0006 ExperimentDataProxy)

## Progress

- [x] Read system prompt, project prompt, slice spec, step-035 SoW, spec-md §0.3.
- [x] Surveyed the Domain seam (`ExperimentDataProxy.fs`, `MeasuredData.fs`) and
      the Storage IO-boundary precedent (`ProjectFile.openProject`).
- [x] Wrote `ExperimentDataStore.fs` (Storage) with `createFileBacked`.
- [x] Registered it in `OpticalConstructor.Storage.fsproj`.
- [x] Wrote `ExperimentDataStoreTests.fs` (Tests) and registered it.
- [x] Built the solution (Release) and ran the constructor unit tests.
- [x] Wrote the state-of-the-world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/ExperimentDataStore.fs` — NEW: the real
  file-backed adapter (`createFileBacked`) behind the step-035 `ExperimentDataProxy`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj` — register
  `ExperimentDataStore.fs` (before the `Storage.fs` anchor).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentDataStoreTests.fs` — NEW: round-trip,
  missing-file, and delegation tests for the real store.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — register the
  new test file after `ExperimentDataProxyTests.fs`.

## Testing state

Local run (advisory only — per Invariant 6 the arc-runner gate engine is the sole
gate authority and re-runs every gate after this session exits):

- **build** (`dotnet build Berreman.slnx -c Release`): GREEN — 0 errors. 10
  warnings, all pre-existing / third-party (NU1701 Wolfram.NETLink, FS0044
  ChartWindow, SYSLIB0051 MathNet, FS3873 Dispersion, FS1125 SeriesDataTests).
  Neither new file emits a warning. Log: `.artifacts/036-build.log`.
- **constructor-unit-tests**
  (`dotnet test OpticalConstructor.Tests -c Release`): GREEN — 659 passed / 0
  failed / 0 skipped (655 step-035 baseline + 4 new `ExperimentDataStoreTests`
  facts). Log: `.artifacts/036-constructor-tests.log`.
- **unit-tests** (BerremanTests), **ui-smoke**, **ui-tests**: unaffected by this
  Storage + Tests-only change; baselines carried forward (119 / 170 / 436). The
  whole solution — Ui.Tests and BerremanTests included — compiled clean in the
  build. The arc-runner re-runs all gates.

`commit_ready: true` — every requirement of the slice (real `createFileBacked`
adapter, IO exception mapped to a typed error at the boundary, no parsing logic
of its own, round-trip + missing-file + delegation tests) landed this round.

## Artifacts

- `C:\GitHub\Berreman\specs\0038\.artifacts\036-build.log`
- `C:\GitHub\Berreman\specs\0038\.artifacts\036-constructor-tests.log`

## Gotchas

- **IO failure maps to the existing `MalformedDataFile`, NOT a new Domain error
  case.** The step `touches: [OpticalConstructor.Storage, OpticalConstructor.Tests]`
  keeps Domain out of scope; the step-035 mock already returned `MalformedDataFile`
  for a missing file, so the real store matches it. The four-case
  `ExperimentDataError` channel is unchanged.
