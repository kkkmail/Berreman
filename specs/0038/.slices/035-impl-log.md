# Step 035 — impl-log (ADD_CONTRACT STORE_XDUO_0006 ExperimentDataProxy)

## Progress

- **Read** the cross-repo system prompts (`add_contract_worker.system-md` and its
  base `arc-runner.system-md`, found under
  `AI-Strategy-Generator/src/ai_strategy_generator/multistep/`, not the repo-root
  path the task file names), the Berreman project prompt, and the slice spec. The
  `## Operator note` section is empty.
- **Surveyed** the precedents: `MeasuredData.fs` (step 034) for the series /
  point / `ExperimentDataError` types and the text-taking parsers; `Experiments.fs`
  (step 025) for the elevated `DataFilePath`; `Scene.fs` / `SceneProxyTests.fs`
  and step 028 `ExperimentCollectionStore` for the ADD_CONTRACT
  `[<ReferenceEquality>]`-proxy + inline-mock + mock-driven-test shape. Confirmed
  no pre-existing `ExperimentDataProxy` / `ExperimentData` symbol.
- **Declared** the seam in the new Domain `ExperimentDataProxy.fs` (module
  `ExperimentData`): the `[<ReferenceEquality>] ExperimentDataProxy` with
  `tryLoadIntensity` / `tryLoadEllipsometric` :
  `DataFilePath -> Result<series, ExperimentDataError>`. Registered LAST in the
  Domain fsproj (after `MeasuredData.fs`).
- **Added** the inline mock (canned series keyed by `DataFilePath.value`, fixtures
  built through the step-34 parsers) + the mock-driven test in
  `ExperimentDataProxyTests.fs`, registered after `MeasuredDataTests.fs`.
- **Verified locally** (advisory — the arc-runner's gate engine is the authority):
  the whole solution builds Release (0 errors; no warnings from either new file),
  and the constructor test project is green at 655 passed / 0 failed (652 → 655,
  the three new facts).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ExperimentDataProxy.fs`
  — **new**. The measured-data LOAD seam (module `ExperimentData`): the
  `[<ReferenceEquality>] ExperimentDataProxy` type only — DECLARED lifecycle, no
  store body / no `createInMemory` / no filesystem read.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
  — registered `ExperimentDataProxy.fs` after `MeasuredData.fs` (it names both
  `Experiments` and `MeasuredData`, which must compile first).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentDataProxyTests.fs`
  — **new**. The inline map-backed mock (canned series keyed by path) + the
  mock-driven test exercising both proxy fields through their EXACT signatures.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
  — registered `ExperimentDataProxyTests.fs` after `MeasuredDataTests.fs`.

## Testing state

`commit_ready: true`. Per the ADD_CONTRACT **Invariant 6 — act only** the worker
authors the contract, mock, and test; the arc-runner's deterministic gate engine
is the sole gate authority and re-runs every gate after this session exits.
`CLAUDE.md` mandates a green build and running tests after every change, so I ran
build + the constructor test project locally to verify MY OWN work only (advisory,
de-risking a failure-budget burn) — not to green-light a gate:

- **build** (whole solution, Release): **0 errors**. The 10 warnings are all
  pre-existing / third-party (`Dispersion.fs` FS3873, `SeriesDataTests.fs` FS1125,
  `ChartWindow.fs` FS0044, MathNet `SYSLIB0051`, `Wolfram.NETLink` `NU1701`) — the
  exempt `NU1701` plus warnings that predate this slice. **Neither new file
  (`ExperimentDataProxy.fs`, `ExperimentDataProxyTests.fs`) emits any warning.**
- **constructor-unit-tests**: green — **655 passed / 0 failed / 0 skipped** (652
  baseline + 3 new `ExperimentDataProxyTests` facts).
- **unit-tests** (BerremanTests), **ui-smoke**, **ui-tests**: unaffected by this
  Domain+Tests-only change and carried forward from the step-034 baseline; the
  whole solution (Ui.Tests included) compiled clean in the build.

Acceptance met: a mock-driven test builds a stub `ExperimentDataProxy` and
exercises `tryLoadIntensity` and `tryLoadEllipsometric` through their exact
signatures (each field is bound to an explicitly-typed local first, so the
compiler pins the declared signature), and it passes.

## Artifacts

None produced this round (a pure contract-declaration slice — no captured logs,
screenshots, or traces). The per-arc artifacts folder is
`C:\GitHub\Berreman\specs\0038\.artifacts`.

## Gotchas

- **DECLARED, not implemented.** No filesystem read, no path resolution, no
  wiring, no `createInMemory` this round. The real file-backed `create` (read the
  file text → hand it to the `MeasuredData` parser → catch IO exceptions AT the
  boundary and map to a typed error) is a later `IMPLEMENT_CONTRACT
  STORE_XDUO_0006` in `OpticalConstructor.Storage`. The mock lives with its test
  in `OpticalConstructor.Tests`, mirroring `SceneProxyTests` (the step-026
  ADD_CONTRACT precedent placed the mock in the test file, not the Domain).
- **Module name is `ExperimentData`, NOT `ExperimentDataProxy`/`MeasuredData`.**
  The proxy type is `ExperimentDataProxy`; a module by the same name would collide
  with it, and `MeasuredData` already exists (the parsers). `ExperimentData` is
  unambiguous — no `ExperimentData` type exists, and it does not shadow the
  `ExperimentDataError` type (a distinct name in `MeasuredData`).
- **Mock miss semantics stay within the step-34 four-case channel.** The declared
  `ExperimentDataError` has no dedicated "path not found" case; a future real
  store MAY grow the channel (missing file / IO failure) without breaking this
  declared shape. For the mock, an unknown path returns `MalformedDataFile` with a
  naming reason — I deliberately did NOT add a fifth error case, which would exceed
  the declared surface for an ADD_CONTRACT step.
- **Compile order.** `ExperimentDataProxy.fs` names `Experiments.DataFilePath`
  (step 025) and the `MeasuredData` series/error types (step 034), so it MUST
  follow `MeasuredData.fs` (previously last) in the Domain fsproj — verified by the
  green build.
- **Fixtures built through the real parsers.** The canned series are produced by
  `parseIntensitySeries` / `parseEllipsometricSeries` on in-memory strings (no file
  IO), so the mock's data is honest and the test needs no hand-rolled `WaveLength` /
  `Angle` construction.
- **System-prompt path drift.** The task file lists the system prompt at
  `C:\GitHub\AI-Strategy-Generator\add_contract_worker.system-md`, but it actually
  lives at `.../src/ai_strategy_generator/multistep/add_contract_worker.system-md`
  (with its `arc-runner.system-md` base alongside). Read both from the real
  location; no scope impact.
- **LF endings** verified on both new files (no CRLF churn); the only CRLF-warned
  file is the arc-runner's own `.manifest.state.json`, which the worker does not
  touch.
