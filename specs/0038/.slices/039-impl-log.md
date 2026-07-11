# Step 039 — impl-log (ADD_COMPONENT UICOMP_XDUO_0011 SolverHandoffWindow)

## Progress

- [x] Ui: `SolverHandoffView.fs` — UiIds, typed `ExperimentHandoffStatus`, pure `validateExperiment`/`buildRows`, `Model`/`init`/`update`/`view`
- [x] Ui: `SolverHandoffWindow.fs` — the `HostWindow` composition root (mkSimple over the pure view)
- [x] Ui.Tests: `SolverHandoffWindowTests.fs` — 8 pure validation/init facts + 2 headless semantic-tree proofs
- [x] fsproj registrations (Ui × 2 after LibraryWindow; Ui.Tests × 1 after DataFilePickerFolderTests)
- [x] Local build + gate suites green (advisory; arc-runner is the gate authority — Invariant 6)

## Files modified

- **Ui (new):** `OpticalConstructor.Ui/SolverHandoffView.fs` — the pure MVU + view behind the window.
  - `UiIds` (`[<RequireQualifiedAccess>]`): `window` / `collectionName` / `solverMessage` /
    `summaryList` / `closeButton` literals + `summaryRow id` / `statusRow id` derivations.
  - `solverComesLaterMessage` (`[<Literal>]`) — the fixed "the actual inverse solver runs separately"
    message.
  - `ExperimentHandoffStatus` (elevated DU): `HandoffReady points` / `UnspecifiedElements` /
    `MissingDataFile` / `DataFileParseError` / `DataFileValidationError`, each with a `.text` and
    `.isReady`.
  - `validateExperiment` (public, PURE given the proxies): three checks in order — (1) every non-Sample
    element bound (`unspecifiedElements`), (2) a data file attached, (3) `ExperimentDataLoad.loadAndValidate`
    (which resolves the detector kind, reads through the `ExperimentDataProxy`, then folds in step-34
    `validateAgainstExperiment`). No solver call. `buildRows` maps it over the received experiments.
  - `SolverHandoffContext` (`[<ReferenceEquality>]`): library + experimentData proxies + the received
    `ExperimentCollectionSnapshot` + `requestClose`. `Model` + `init` (validate once) + `update`
    (`RequestClose`) + `view` (header naming the collection, the fixed message, the scrollable
    per-experiment summary — setup / detector / file / typed status — and a Close row).
- **Ui (new):** `OpticalConstructor.Ui/SolverHandoffWindow.fs` — the `HostWindow` composition root
  (the `CategoryEditorWindow` precedent): Title/Name/AutomationId = `UiIds.window`, builds the context
  (`requestClose = this.Close`), mounts the pure MVU via `Program.mkSimple`.
- **Ui:** `OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` — `Compile` the view then the window,
  after `LibraryWindow.fs`, before `TableAndElementRotationView.fs`.
- **Ui.Tests (new):** `OpticalConstructor.Ui.Tests/SolverHandoffWindowTests.fs` — 10 facts (2
  `ui-smoke`), building real experiments (captured setups) through the inverse model over mock
  `ExperimentDataProxy` shapes.
- **Ui.Tests:** `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` — `Compile` after
  `DataFilePickerFolderTests.fs`.

## Testing state

`commit_ready: true`. Every slice requirement lands this round (no self-declared split). Local
diagnostic runs (advisory — the arc-runner gate engine is the sole authority; Invariant 6):

- **build** (whole solution, Release): green — 0 errors; NO new warnings from touched projects (only
  the step-001-catalogued pre-existing NU1701 / SYSLIB0051 / FS0044 / FS3873 / FS1125 remain).
- **unit-tests** (BerremanTests): 119 passed / 5 pre-existing skips — unaffected (solver untouched).
- **constructor-unit-tests** (OpticalConstructor.Tests): 659 passed — unaffected (Domain / Storage /
  Optimization untouched; the handoff validation is exercised through the Ui.Tests over mocks).
- **ui-smoke**: 173 passed (171 → +2: the window-mounts-with-ids proof and the status-text proof).
- **ui-tests**: 461 passed (+8: the pure per-experiment validation + `init` facts).

The 10 new facts alone: `Passed: 10, Failed: 0`.

Logs under `specs/0038/.artifacts/039-*.log`.

## Artifacts

- `039-build-01.log` — full-solution Release build (0 errors).
- `039-ui-smoke-01.log`, `039-ui-tests-01.log`, `039-constructor-tests-01.log`,
  `039-unit-tests-01.log` — gate-suite runs.

## Gotchas

- **`touches` lists `OpticalConstructor.App`, but I did NOT modify App.** The ADD_COMPONENT invariant
  is "declare the surface; do not wire it into a parent view here", and `.spec-jsonl` step 047
  (WIRE_UI) EXPLICITLY owns building the `WindowLauncher` with "every window factory — Materials,
  Library, the three editors, SolverHandoff" and driving the handoff window headless from the composed
  root. So this step declares the component + its own headless test; the launcher-button / composition
  wiring lands at 45/47. `touches` is the permissive allow-set, not a per-project requirement. Decision
  recorded here per the "record the interpretation" rule.
- **The "received collection" is modelled as `ExperimentCollectionStore.ExperimentCollectionSnapshot`**
  (name + experiments) — the pure DATA shape the collection persists as (step 029), no live handle. It
  is the natural "received" value and ties the terminal screen to the persistence contract. (The live
  `Experiments.ExperimentCollection` draft-plus-list is the AUTHORING shape; the handoff consumes the
  frozen snapshot.)
- **Single-case DU / same-named case shadowing bit once** (the step-037 gotcha): in the test,
  `Experiments.ExperimentId.create n` bound the union CASE in expression position and failed to find
  `.create`; fixed by using the case constructor directly (`Experiments.ExperimentId n` — identical to
  `.create`). `DataFilePath` / `CollectionName` are constructed the same case-direct way throughout.
- **Validation order is first-failure-wins** (elements → file-present → load/validate) so each
  experiment shows exactly ONE typed status. GREEN (`HandoffReady`) requires all three to pass.
- **No solver math anywhere.** The only computation the screen runs is `loadAndValidate` (a proxy read
  + `validateAgainstExperiment`). The "no computation beyond validation" fact proves it with a
  call-counting proxy (exactly one load per validated experiment) and asserts the reported point count
  equals the RAW mock count (nothing normalized).
- **AutomationId, never Name, on the keyed per-experiment rows / status blocks** (the CategoryEditorView
  precedent) so the id survives FuncUI recycling a control onto another slot; each row is also
  `View.withKey`'d by its experiment id.
- **System-prompt path drift (carried from steps 035–038).** The task file names
  `C:\GitHub\AI-Strategy-Generator\add_component_worker.system-md`; the real prompt lives under
  `.../src/ai_strategy_generator/multistep/add_component_worker.system-md` (a thin per-family delta over
  `arc-runner.system-md`). Located and read both. No scope impact.
- **Invariant 6 respected.** The local build + suite runs recorded here are advisory; the arc-runner's
  gate engine re-runs every gate after this session exits and is the sole authority.

## Changelog

- 2026-07-11 — Step 039 (ADD_COMPONENT UICOMP_XDUO_0011): declared the inverse flow's terminal
  `SolverHandoffWindow` — a single-instance summary + basic-validation screen. `SolverHandoffView`
  (pure): a typed `ExperimentHandoffStatus`, a pure `validateExperiment` running the three basic checks
  (non-sample elements specified, data file attached, schema-valid + range-consistent through the
  `ExperimentDataProxy`) with NO solver math, and the `state → view` projection (per-experiment setup /
  detector / file / typed status + the fixed solver-comes-later message). `SolverHandoffWindow` (the
  HostWindow root). `SolverHandoffWindowTests`: 8 pure validation/init facts + 2 headless semantic-tree
  proofs over mocks (green ready statuses; missing-file + range-mismatch typed messages). Build green;
  suites 119 / 659 / 173 / 461.
