# Step 007 — impl log (attempt 1)

## Progress

- [x] Read the task file, worker system prompt (`add_contract_worker.system-md` +
      shared base `arc-runner.system-md`), project prompt (Operator note: empty),
      slice spec, spec 0038 `.spec-md` Part C, the 007 gate roster (`007.gates`:
      build / unit-tests / constructor-unit-tests / ui-smoke / ui-tests), the step
      003/005/006 SoWs, and the surrounding code (the `EditorLaunchers` precedent
      at `Ui/TableAndElementRotationView.fs:116-141`, `WorkbenchSettings.fs`,
      `AppContext.fs`, the Ui.Tests headless conventions, steps 008/016 for the
      growth path the declaration must leave open).
- [x] Wrote the impl-plan (`007-impl-plan.md`).
- [x] `Ui/WindowLauncher.fs` (new) — module `WindowLauncher` (the
      `WorkbenchSettings` module-wrapping precedent) declaring SVC_XDUO_0001 at
      `declared` lifecycle: `WindowKey` (four single-instance keys +
      `MaterialEditorKey of MaterialLibrary.MaterialId` /
      `SampleEditorKey of Library.SampleId`), `EntryFreshness = NewUnsaved |
      Persisted`, the typed `WindowLauncherError` (`WindowFactoryFailed of key *
      reason`, `WindowNotRegistered of key`), the `WindowLaunchOutcome` DU
      (`CreatedWindow` / `ActivatedWindow` / `RetargetedWindow of Window` — the
      step-016 case declared now so later matches are compiler-complete), the
      `[<ReferenceEquality>]` `WindowLauncher` record (`openOrActivate`,
      `forgetWindow`, `decideSelectModality` — all camelCase, all
      Result-returning), and `createMock factory modality` (registry bookkeeping
      over a private `ref Map`, window construction delegated to the injected
      recording stub factory, NO real windowing behaviour).
- [x] `Ui.fsproj` — compile entry after `AppContext.fs` (end of the list).
- [x] `Ui.Tests/WindowLauncherTests.fs` (new) — the mock-driven contract suite:
      5 pure signature pins (gate `ui-tests`) + 3 registry-behaviour proofs over
      stub-built (never shown) windows on the shared headless session (gate
      `ui-smoke`), incl. both slice-acceptance observations.
- [x] `Ui.Tests.fsproj` — compile entry after `AppContextTests.fs`.
- [x] Diagnostic build + all four suite runs green (see Testing state); LF/BOM
      check clean.
- [x] State-of-the-world written (`007-state-of-the-world.md`).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/WindowLauncher.fs` — NEW
  (the declared SVC_XDUO_0001 surface + mock).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
  — compile entry for `WindowLauncher.fs` (last in the list).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WindowLauncherTests.fs`
  — NEW (5 pure pins + 3 headless registry proofs).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
  — compile entry for `WindowLauncherTests.fs` (after `AppContextTests.fs`).

## Testing state

Gates are executed by the arc-runner's deterministic gate engine after this worker
exits (ADD_CONTRACT Invariant 6 — the worker acts, it runs no checks). Diagnostic
verification only (not gate authority), logs in `.artifacts/`:

- `dotnet build Berreman.slnx -c Release -nologo -v:m` — **Build succeeded,
  0 errors, no MSB3277**; the touched projects (Ui / Ui.Tests) compiled with ZERO
  warnings; the 10 warning lines are exactly the step-001-catalogued pre-existing
  set in untouched files (FS1125 SeriesDataTests ×4, FS3873 Dispersion, FS0044
  ChartWindow, SYSLIB0051 vendored MathNet ×2, NU1701 Wolfram.NETLink ×2 —
  carried for the Part N sweep). (`007-diag-build.log`)
- ui-smoke (`--filter Category=ui-smoke`): **117/117** (checkpoint 114, +3 — the
  second-open-activates sweep over all six key shapes, the two-editor-keys
  acceptance, the forget-then-recreate proof). (`007-diag-ui-smoke.log`)
- ui-tests (`--filter Category!=ui-smoke`): **357/357** (checkpoint 352, +5 pure
  contract pins). (`007-diag-ui-tests.log`)
- constructor-unit-tests: **479/479** (== checkpoint; untouched project).
  (`007-diag-constructor-tests.log`)
- unit-tests (BerremanTests): **119 passed / 5 pre-existing skips** (== checkpoint).
  (`007-diag-unit-tests.log`)
- Line endings: `git diff --numstat` equals `--ignore-cr-at-eol --numstat` (the two
  fsproj edits are pure additions); both new files carry 0 CR bytes and no BOM.

No failures on the first compile/run of the new suite.

## Artifacts

- `specs/0038/.artifacts/007-diag-build.log` — diagnostic Release solution build.
- `specs/0038/.artifacts/007-diag-ui-smoke.log` — ui-smoke suite run (117 passed).
- `specs/0038/.artifacts/007-diag-ui-tests.log` — ui-tests suite run (357 passed).
- `specs/0038/.artifacts/007-diag-constructor-tests.log` — constructor suite run
  (479 passed).
- `specs/0038/.artifacts/007-diag-unit-tests.log` — BerremanTests run (119 passed,
  5 pre-existing skips).

## Gotchas

- No operator note in flight (the project prompt's Operator note section is empty).
- **The task file's system-prompt path does not exist**
  (`C:\GitHub\AI-Strategy-Generator\add_contract_worker.system-md`); the real file
  is `C:\GitHub\AI-Strategy-Generator\src\ai_strategy_generator\multistep\
  add_contract_worker.system-md` — located by glob and read in full (with the
  shared base it layers on). Interpretation, not a blocker.
- **The per-arc contract registry needed no worker edit**: `.contracts-json`
  already records `SVC_XDUO_0001` at lifecycle `declared`, declaringStep 7
  (supervisor-maintained, pre-populated for the whole arc). The step-004
  precedent (a worker-side registry surface) does not apply to a proxy — the
  declared F# surface itself is the artifact.
- **All three record fields are Result-returning** — including
  `decideSelectModality`, which can never fail today. The slice's wording
  ("record … of camelCase Result-returning functions") is read literally so the
  surface stays uniform and step 008/016 can grow failure modes without a
  signature break.
- **`RetargetedWindow of Window` is declared now, returned by nobody**: step 016
  is the first producer. Declaring the case in the ADD_CONTRACT round makes every
  intermediate match compiler-complete (FS0025 is `--warnaserror`), so step 016
  extends behaviour without re-shaping the outcome DU.
- **`forgetWindow` of an absent key is a typed error, not a no-op** (recorded
  interpretation): `WindowNotRegistered` makes a double-unregister / forget-vs-
  close race visible in a log instead of silent. Step 008's `Closed`-hook
  implementation can rely on the registry holding the key it registered.
- **The mock does registry bookkeeping but NO windowing** — it never calls
  `Activate`/`Show`/`ShowDialog` and hooks no `Closed` event; "activated" is
  observable purely through the `ActivatedWindow` outcome plus the stub factory's
  call log. That keeps the declared round free of real behaviour (Invariant 6 /
  ADD_CONTRACT family character) while still letting the acceptance observations
  run.
- **The mock lives in the product Ui module, not Ui.Tests** — the
  `createInMemory`-in-Domain precedent: later steps' tests (008/016) reuse it
  without cross-test-project references.
- **Pure pins never construct a `Window`** (their stub factories fail or throw
  before construction), so they run without the headless session under the
  `ui-tests` gate; every test whose factory builds windows wraps in
  `HeadlessSession.run` (bare `Window()` construction needs the headless
  platform) and carries the `ui-smoke` trait — the AppContextTests gate split.
- Step 002–006 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the `App` type-name ambiguity guard; the
  appsettings.json write-back into test output copies is expected).
