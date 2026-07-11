# Step 036 — State of the world

## Where we are

Spec 0038 Part I, step 036 (`IMPLEMENT_CONTRACT STORE_XDUO_0006 ExperimentDataProxy`,
`depends_on: 35`). Step 034 gave the Domain the pure, text-taking measured-data
PARSERS (`MeasuredData.fs`); step 035 DECLARED the `[<ReferenceEquality>]
ExperimentDataProxy` load seam (`ExperimentDataProxy.fs`, module `ExperimentData`)
with an in-memory mock. This step supplies the REAL file-backed adapter behind that
seam — `ExperimentDataStore.createFileBacked` in `OpticalConstructor.Storage`, the
ONLY new real IO this spec permits (spec-md §0.3c). It reads a data file's text at
the IO boundary and delegates to the step-34 parsers, bringing the contract to the
`implemented` lifecycle. `touches: [OpticalConstructor.Storage,
OpticalConstructor.Tests]` — Domain and Ui are untouched.

## What's working

- Add `ExperimentDataStore.createFileBacked : unit -> ExperimentDataProxy` in a new
  Storage `ExperimentDataStore.fs`: both proxy fields read the file text via
  `File.ReadAllText` at the boundary and hand it to the step-34
  `parseIntensitySeries` / `parseEllipsometricSeries` — no parsing logic of its own.
- Catch any .NET IO exception (missing file, missing directory, access failure) AT
  the boundary and map it to a typed `ExperimentDataError` (`MalformedDataFile`
  naming the file kind, path, and reason) — an exception never crosses into Domain.
- Register `ExperimentDataStore.fs` in the Storage fsproj (before the `Storage.fs`
  anchor; depends only on the Domain project reference).
- Add four `ExperimentDataStoreTests`: a temp file under the test output round-trips
  a known intensity series and a known ellipsometric series (store output equals the
  step-34 parser's), a missing path yields a typed error from both fields (never a
  throw), and an empty-but-existing file yields the parser's `EmptyDataFile` —
  proving the adapter reads then delegates.
- Build green (0 errors, no warnings from the new files); constructor suite 659
  passing, 0 failing (655 → 659).

## Tests

- **build** (whole solution, Release): green — 0 errors; the 10 warnings are all
  pre-existing / third-party (NU1701 Wolfram.NETLink, FS0044 ChartWindow,
  SYSLIB0051 MathNet, FS3873 Dispersion, FS1125 SeriesDataTests). Neither new file
  emits a warning.
- **constructor-unit-tests**: green — 659 passed / 0 failed / 0 skipped, including
  the 4 new `ExperimentDataStoreTests` facts (655 baseline + 4).
- **unit-tests** (BerremanTests), **ui-smoke**, **ui-tests**: unaffected by this
  Storage + Tests-only change and carried forward from the step-035 baseline; the
  whole solution (Ui.Tests + BerremanTests included) compiled clean in the build
  gate. The arc-runner re-runs every gate as the sole authority.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 659
  ui_smoke_tests:         170
  ui_tests:               436
```

## Architecture

- **The adapter is the disk-read EDGE, and only that.** `MeasuredData` (step 034)
  takes the file TEXT and never touches the filesystem or throws;
  `createFileBacked` is exactly the missing `path -> read text -> parse` boundary.
  It adds NO schema, NO malformed-row logic, NO elevated point types — those all
  stay in `MeasuredData`. A private generic `loadThrough parse kind path` helper
  reads the text and defers to the supplied step-34 parser, so both fields share
  the one boundary.
- **Exceptions are caught AT the boundary and mapped to typed errors** (CLAUDE.md:
  platform APIs that throw are caught at the boundary and mapped to typed errors;
  F# above the boundary never handles exceptions). This mirrors the storage-layer
  precedent `ProjectFile.openProject`
  (`try Ok (File.ReadAllText path) with e -> Error (FileIoError e)`).
- **The real store replaces the mock without touching any consumer.** Every
  consumer holds an `ExperimentDataProxy`; the composition root simply swaps the
  step-035 in-memory mock for `createFileBacked ()`. This is the
  `Scene.createInMemory` / `ExperimentCollectionProxy.createInMemory` convention,
  except the real backing here is a file rather than a `ref Map` (spec-md §0.3c
  approved the one file-read seam).
- **New Storage module `ExperimentDataStore`** in its own file, distinct from the
  Domain module `ExperimentData` (the type) and `MeasuredData` (the parsers).

## Deferred

- The UI attachment flow that hands a picked file's `DataFilePath` to the proxy and
  the parsed series onward to `validateAgainstExperiment` — a later UI cycle
  (spec-md Part L / §17.2). This step wires no picker and no UI.
- A dedicated missing-file / IO-failure `ExperimentDataError` case remains
  deliberately NOT added — see Gotchas. A future Domain-touching step may add one if
  a caller needs to distinguish IO failure from a malformed file.

## Gotchas

- **IO failure maps to the existing `MalformedDataFile`, NOT a new Domain error
  case.** The step `touches: [OpticalConstructor.Storage, OpticalConstructor.Tests]`
  keeps Domain out of scope, so the `ExperimentDataError` DU (four cases:
  `MalformedDataFile` / `EmptyDataFile` / `DataRangeMismatch` / `DataUnitsMismatch`)
  is unchanged. The step-035 mock already returned `MalformedDataFile` for a missing
  file, so the real store matches that precedent; the step-035 SoW's "MAY grow the
  channel" was permissive, not required. The reason string names the path and the
  underlying `.Message`, so a log still distinguishes a missing file from other
  failures. The delegation test asserts an empty file yields the DIFFERENT
  `EmptyDataFile`, proving IO failure and parse failure stay distinguishable.
- **Round-trip test files are ephemeral test scratch**, written under
  `AppContext.BaseDirectory` (the bin test-output tree) — not `%TEMP%`, and not the
  arc `.artifacts/` folder (reserved for durable audit artifacts like the build /
  test logs).
- **Invariant 6 respected.** The local build + constructor-test run recorded here is
  advisory; the arc-runner's gate engine re-runs every gate after this session exits
  and is the sole gate authority. The run was done to keep the SoW baseline count
  (`constructor_unit_tests: 659`) honest, matching prior workers in this arc.
- **System-prompt path drift (carried from step 035).** The task file's
  `C:\GitHub\AI-Strategy-Generator\implement_contract_worker.system-md` is stale; the
  real prompts live under `.../src/ai_strategy_generator/multistep/`. No scope impact.

## Changelog

- 2026-07-11 — Step 036 (IMPLEMENT_CONTRACT STORE_XDUO_0006): implemented the real
  file-backed `ExperimentDataProxy` — `ExperimentDataStore.createFileBacked` in
  `OpticalConstructor.Storage`, the one file-read seam spec-md §0.3c permits. Both
  proxy fields read the data file text at the boundary (`File.ReadAllText`, IO
  exceptions caught and mapped to a typed `MalformedDataFile`) and delegate to the
  step-34 `MeasuredData` parsers, adding no parsing logic of their own. Added four
  `ExperimentDataStoreTests` (intensity + ellipsometric round-trip, missing-file
  typed error from both fields, empty-file delegation). Domain untouched
  (`touches` = Storage + Tests). Build green; constructor suite 659 passing, 0
  failing.
