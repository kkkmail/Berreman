# Step 035 — State of the world

## Where we are

Spec 0038 Part I, step 035 (`ADD_CONTRACT STORE_XDUO_0006 ExperimentDataProxy`,
`depends_on: 34`). Step 034 gave the Domain the pure, text-taking measured-data
PARSERS (`MeasuredData.fs`: `IntensitySeries` / `EllipsometricSeries` over
elevated point types, the `ExperimentDataError` channel, and
`validateAgainstExperiment`). This step declares the IO boundary those parsers
were written to sit behind — the `ExperimentDataProxy` load seam that resolves an
experiment's elevated `DataFilePath` (step 025) into a parsed series. It is the
DECLARED contract only (interface + mock + test); a later `IMPLEMENT_CONTRACT
STORE_XDUO_0006` builds the real file-backed adapter in `OpticalConstructor.Storage`.

## What's working

- Declare the DECLARED-lifecycle `[<ReferenceEquality>] ExperimentDataProxy` in a
  new Domain `ExperimentDataProxy.fs` (module `ExperimentData`): `tryLoadIntensity`
  / `tryLoadEllipsometric` : `DataFilePath -> Result<series, ExperimentDataError>`.
- Reuse the step-25 `DataFilePath` and the step-34 `IntensitySeries` /
  `EllipsometricSeries` / `ExperimentDataError` — no new primitives, no leakage of
  a bare `string` path or raw series into the seam.
- Register `ExperimentDataProxy.fs` last in the Domain fsproj (after
  `MeasuredData.fs`, which it depends on) and the test after `MeasuredDataTests.fs`.
- Add the mock (canned series keyed by path, fixtures built through the real
  step-34 parsers) plus a mock-driven test that drives both proxy fields through
  their exact signatures, rejects an unknown path with a typed error, and pins the
  reference equality.
- Build green (0 errors, no warnings from the new files); constructor suite 655
  passing, 0 failing (652 → 655).

## Tests

- **build** (whole solution, Release): green — 0 errors; the 10 warnings are all
  pre-existing / third-party and exempt (`NU1701`) or predate this slice; neither
  new file emits a warning.
- **constructor-unit-tests**: green — 655 passed / 0 failed / 0 skipped, including
  the 3 new `ExperimentDataProxyTests` facts (652 baseline + 3).
- **unit-tests** (BerremanTests), **ui-smoke**, **ui-tests**: unaffected by this
  Domain+Tests-only change and carried forward from the step-034 baseline; the
  whole solution (Ui.Tests included) compiled clean in the build gate. The
  arc-runner re-runs every gate as the sole authority.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 655
  ui_smoke_tests:         170
  ui_tests:               436
```

## Architecture

- **The seam is the IO edge for the pure parsers.** `MeasuredData` (step 034)
  takes the file TEXT and never touches the filesystem or throws; the
  `ExperimentDataProxy` is exactly the missing `path -> read text -> parse`
  adapter. Keeping it as pure data (a record of `Result`-returning functions)
  leaves logic that holds the proxy referentially transparent and lets a test
  substitute a canned stub — the `Library.LibraryProxy` / `Scene.SceneProxy`
  convention followed across this arc.
- **`[<ReferenceEquality>]`** because the function-valued fields have no structural
  equality; a host model (Elmish) that holds the proxy keeps its required equality,
  comparing the proxy by identity.
- **New module `ExperimentData` in its own file**, mirroring the one-file-per-store
  precedent (`Scene.fs`, `ExperimentCollectionStore.fs`) and keeping `MeasuredData.fs`
  as the pure parsers. The name avoids colliding with the `ExperimentDataProxy` type
  and the `MeasuredData` module.
- **Mock lives with its test** (`OpticalConstructor.Tests`), not the Domain — the
  step-026 ADD_CONTRACT precedent (`SceneProxyTests` supplies `makeStub`, while
  `Scene.fs` gained `createInMemory` only at the later IMPLEMENT_CONTRACT).

## Deferred

- The real disk-backed `ExperimentDataProxy.create` (open the file, read its text,
  hand it to the `MeasuredData` parser, catch IO exceptions AT the boundary and map
  them to a typed error) — a later `IMPLEMENT_CONTRACT STORE_XDUO_0006` in
  `OpticalConstructor.Storage`. That cycle MAY grow the `ExperimentDataError`
  channel with a missing-file / IO-failure case without breaking this declared
  shape.
- The UI attachment flow that hands a picked file's `DataFilePath` to the proxy and
  its parsed series onward to `validateAgainstExperiment` — a later UI cycle.

## Gotchas

- **DECLARED, not implemented.** No filesystem read, no `createInMemory`, no wiring
  this round — the seam only.
- **Mock miss semantics stay within the step-34 four-case channel.** An unknown
  path returns `MalformedDataFile` (a naming reason), NOT a new fifth error case —
  adding one would exceed the declared surface for an ADD_CONTRACT step. The real
  store may add a missing-file case later.
- **Compile order is load-bearing.** `ExperimentDataProxy.fs` must follow
  `MeasuredData.fs` (and `Experiments.fs`) — it names types from both.
- **System-prompt path drift.** The task file's
  `C:\GitHub\AI-Strategy-Generator\add_contract_worker.system-md` is stale; the real
  prompts live under `.../src/ai_strategy_generator/multistep/`. No scope impact.

## Changelog

- 2026-07-11 — Step 035 (ADD_CONTRACT STORE_XDUO_0006): declared the measured-data
  LOAD seam `[<ReferenceEquality>] ExperimentDataProxy` (`tryLoadIntensity` /
  `tryLoadEllipsometric` : `DataFilePath -> Result<series, ExperimentDataError>`)
  in a new Domain `ExperimentDataProxy.fs` (module `ExperimentData`), reusing the
  step-25 `DataFilePath` and the step-34 series/error types; added the inline mock
  (canned series keyed by path) + a mock-driven test exercising both fields through
  their exact signatures. DECLARED lifecycle — no disk read. Build green;
  constructor suite 655 passing, 0 failing.
