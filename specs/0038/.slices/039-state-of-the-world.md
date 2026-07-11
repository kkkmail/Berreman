# Step 039 — State of the world

## Where we are

Spec 0038 Part L (the inverse-problem FLOW, wiring only — no solver). Step 039 (`ADD_COMPONENT`,
`UICOMP_XDUO_0011 SolverHandoffWindow`, `declaring_project: OpticalConstructor.Ui`,
`depends_on: [29, 36, 37]`) declares the inverse flow's TERMINAL screen: the single-instance
handoff window that summarizes the received experiment collection and reports a TYPED per-experiment
validation status, with the fixed message that the actual solver runs separately. It builds on step 25
(the full-setup `Experiment` + `ElementDescriptor` setup + `DataFilePath`), step 29 (the
`ExperimentCollectionSnapshot` persistence shape), step 34/36 (the `MeasuredData` parsers +
`validateAgainstExperiment` + the file-backed `ExperimentDataProxy`), and step 37 (the pure
`ExperimentDataLoad.loadAndValidate` seam it reuses verbatim). Per the ADD_COMPONENT invariant the
component is DECLARED here (surface + headless test); opening it from the inverse constructor through
the `WindowLauncher` (`SolverHandoffWindowKey` already exists) and the composition-root factory wiring
belong to step 047 (WIRE_UI).

## What's working

- Declare `SolverHandoffWindow` — the inverse flow's terminal summary + basic-validation screen —
  as a FuncUI `HostWindow` over the pure `SolverHandoffView` MVU (the `CategoryEditorWindow` precedent).
- Summarize the received collection: per experiment its ordered setup, resolved detector kind,
  attached data file, and a typed validation status, plus the fixed solver-comes-later message.
- Run BASIC validation only (no solver math): every non-sample element specified, a data file
  attached, and the file schema-valid + range-consistent through the `ExperimentDataProxy` — folding
  in the step-34 `validateAgainstExperiment` via the reused `ExperimentDataLoad.loadAndValidate`.
- Report a TYPED per-experiment status (`ExperimentHandoffStatus`): green `HandoffReady` or one of
  `UnspecifiedElements` / `MissingDataFile` / `DataFileParseError` / `DataFileValidationError`.
- Prove it headless over mock proxies: a valid collection lists green statuses; a missing file and a
  range mismatch each show their typed message; the load runs exactly once per experiment (no
  computation beyond validation). Green build; suites 119 / 659 / 173 / 461.

## Tests

Gate execution is the arc-runner gate engine's job (Invariant 6 — the worker acts, runs no gates).
Local runs below are advisory, done for baseline honesty. Roster: `build`, `unit-tests`,
`constructor-unit-tests`, `ui-smoke`, `ui-tests`.

- **build**: green (0 errors); no new warnings from touched projects (only the step-001 pre-existing
  NU1701 / SYSLIB0051 / FS0044 / FS3873 / FS1125 remain).
- **unit-tests** (BerremanTests): 119 passed / 5 pre-existing skips — unaffected (solver untouched).
- **constructor-unit-tests**: 659 passed — unaffected (Domain / Storage / Optimization untouched; the
  handoff validation is exercised through the Ui.Tests over mock proxies).
- **ui-smoke**: 173 passed (171 → +2: the window-mounts-with-ids proof and the status-text proof).
- **ui-tests**: 461 passed (+8: the pure per-experiment validation + `init` facts).

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 659
  ui_smoke_tests:         173
  ui_tests:               461
```

## Architecture

- **The screen is a pure `state → view` projection over a functional-proxy Context** (the
  `CategoryEditorView` precedent). `SolverHandoffContext` carries the read-only `LibraryProxy`
  (detector-kind resolution) and the `ExperimentDataProxy` (the step-36 measured-data load seam), the
  received `ExperimentCollectionSnapshot`, and `requestClose`. `init` validates every received
  experiment ONCE and the view is a pure projection of the resulting rows — so a headless test drives
  the whole surface over mocks with no real screen or IO.
- **The validation is BASIC and reuses the step-37 seam, adding NO solver math.**
  `validateExperiment` runs three checks in first-failure-wins order: (1) every non-Sample element
  bound — the inverse sample is the unknown, allowed unbound (step 30 draws it dashed); (2) a data
  file attached; (3) `ExperimentDataLoad.loadAndValidate` — which resolves the detector kind, reads
  through the `ExperimentDataProxy` in the shape that kind fixes, then folds in the step-34
  `validateAgainstExperiment` range/units check. The only computation the screen performs is the proxy
  read + validation; the data is gathered RAW (the point count is un-normalized) — corrections belong
  to the future solver.
- **The status is an elevated typed DU** (`ExperimentHandoffStatus`), never a bare status string, with
  a `.text` the row renders and an `.isReady` guard that colours it green vs red. A parse / empty-file
  failure is `DataFileParseError`; a range / units mismatch is `DataFileValidationError`; a missing
  attachment is `MissingDataFile`; an unbound non-sample element is `UnspecifiedElements`.
- **The "received collection" is the `ExperimentCollectionSnapshot`** — the pure name-plus-experiments
  DATA shape (step 029), no live handle — so the terminal screen consumes the frozen persistence shape,
  distinct from the live `Experiments.ExperimentCollection` authoring draft.
- **Stable intent-named ids in a per-view `UiIds` module** (window / summaryList / solverMessage /
  collectionName / closeButton + per-experiment `summaryRow` / `statusRow` derivations), on
  `AutomationProperties.AutomationId` (never `Name`) for the keyed recycled rows. Step 044 consolidates
  every id into one module with a `Handoff` sub-module — a mechanical re-point later.

## Deferred

- **Opening the handoff window from the inverse constructor through the `WindowLauncher`** (single
  instance under `SolverHandoffWindowKey`) and the composition-root factory wiring — step 047 (WIRE_UI)
  owns "construct the WindowLauncher with every window factory — … SolverHandoff" and the headless
  whole-surface acceptance. This step declares the component only (ADD_COMPONENT invariant).
- **Consolidating the handoff UiIds into the single `OpticalConstructor.Controls/UiIds.fs`** module
  (with a `Handoff` sub-module) — step 044.
- **Re-validating a LOADED collection's attached files against the session** — the paths ride along
  on the snapshot; the handoff validates whatever it is handed.
- The actual inverse SOLVER (normalization + fit) — out of scope for all of spec 0038 Part L; the
  fixed message states it runs separately.

## Gotchas

- **`touches` includes `OpticalConstructor.App`, but this step does NOT modify App.** The ADD_COMPONENT
  invariant ("declare the surface; do not wire it into a parent view here") plus `.spec-jsonl` step 047
  owning the launcher/composition wiring means App is the permissive allow-set, not a requirement. See
  the impl-log Gotchas for the full rationale.
- **Single-case DU / same-named case shadowing** bit once in the test (`ExperimentId.create` bound the
  union case, not the static member) — fixed with the case constructor directly (`ExperimentId n`), the
  step-037 precedent. `DataFilePath` / `CollectionName` are constructed case-direct throughout.
- **No solver math is a hard invariant** — asserted by a call-counting proxy (exactly one load per
  validated experiment) and by the reported point count equalling the RAW mock count.
- **System-prompt path drift (carried from steps 035–038).** The task file's
  `C:\GitHub\AI-Strategy-Generator\add_component_worker.system-md` is stale; the real per-family delta
  lives under `.../src/ai_strategy_generator/multistep/`. Located and read (with its `arc-runner.system-md`
  base). No scope impact.
- **Invariant 6 respected.** The local build + suite runs recorded here are advisory; the arc-runner's
  gate engine re-runs every gate after this session exits and is the sole authority. The runs keep the
  SoW baseline counts honest (the prior-worker convention in this arc).

## Changelog

- 2026-07-11 — Step 039 (ADD_COMPONENT UICOMP_XDUO_0011): declared the inverse flow's terminal
  `SolverHandoffWindow` — a single-instance summary + basic-validation screen. Added `SolverHandoffView`
  (pure: the typed `ExperimentHandoffStatus`, the pure `validateExperiment` running the three basic
  checks with NO solver math, and the `state → view` projection — per-experiment setup / detector /
  file / typed status + the fixed solver-comes-later message) and `SolverHandoffWindow` (the HostWindow
  root). Added `SolverHandoffWindowTests` (8 pure validation/init facts + 2 headless semantic-tree
  proofs over mock proxies). No App wiring (owned by step 047). Build green; suites 119 / 659 / 173 / 461.
