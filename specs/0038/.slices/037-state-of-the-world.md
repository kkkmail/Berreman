# Step 037 — State of the world

## Where we are

Spec 0038 Part L (the inverse-problem FLOW, wiring only — no solver). Step 037 (`IMPLEMENT`,
`depends_on: [15, 25, 29, 36]`, `touches: [OpticalConstructor.Ui, OpticalConstructor.Domain,
OpticalConstructor.App, OpticalConstructor.Ui.Tests]`) wires the **inverse constructor state** and
the **experiment-collection builder** on the Main workbench, and threads the step-29/36
`ExperimentCollectionProxy` (STORE_XDUO_0005) and `ExperimentDataProxy` (STORE_XDUO_0006) through the
App composition. It builds on step 25 (the full-setup `Experiment` + `attachDataFile` +
`DataFilePath`), step 29 (in-memory `ExperimentCollectionProxy.createInMemory`), step 34/36 (the
`MeasuredData` parsers + `validateAgainstExperiment` + file-backed `ExperimentDataStore`), step 30
(the dashed unbound sample), and step 15 (the Library window Select state — the hint-bind path). The
`Inverse` launcher button and the final composition acceptance remain steps 45 / 47.

## What's working

- Add `initInverse` beside `initMainWith`: the inverse Main scene seeds a light source, an UNBOUND
  sample (step 30 draws it dashed), and a detector, in a new `InverseConstructor` mode.
- Gate the forward experiment chart on a HINT: with no hint (the sample unbound) the chart surface is
  ABSENT; binding a sample restores it — `experimentChartVisible` empties
  `experimentResult`/`chartForExperiment`/`experimentPsiDelta` when gated off. Forward Main is unchanged.
- Grow the Experiments bay into named collections (Ui layer, `ExperimentControls` untouched): name /
  save / list / load a collection of step-25 experiments through the `ExperimentCollectionProxy`.
- Attach ONE data file per experiment through a picker whose `DataFilePath` loads through the
  `ExperimentDataProxy` (intensity or ellipsometric per the experiment's detector kind) and validates
  against the experiment — a typed per-experiment parse/validation status shows inline.
- Thread the two proxies through the App composition (`AppContext` fields + record-update onto the
  Main surface), so every Main window shares one saved-collection store.
- Add a pure Domain helper `ExperimentDataLoad` (detector-kind resolve + load/validate), reusable by
  the future SolverHandoffWindow. Green build; suites 119 / 659 / 171 / 446.

## Tests

Gate execution is the arc-runner gate engine's job (IMPLEMENT Invariant 6 — the worker acts, runs no
gates). Local runs below are advisory, done for baseline honesty. Roster: `build`, `unit-tests`,
`constructor-unit-tests`, `ui-smoke`, `ui-tests`.

- **build**: green (0 errors); no new warnings from touched projects (only the step-001 pre-existing
  NU1701 / SYSLIB0051 / FS0044 / FS3873 / FS1125 remain).
- **unit-tests** (BerremanTests): 119 passed / 5 pre-existing skips — unaffected (solver untouched).
- **constructor-unit-tests**: 659 passed / 0 failed — unchanged (this step does not touch that project;
  the new Domain helper is exercised through the Ui.Tests over mock proxies).
- **ui-smoke**: 171 passed (170 → +1 inverse-builder render proof).
- **ui-tests**: 446 passed (436 → +10 inverse init / hint-restore / attach-status ×4 / collection
  round-trip / blank-name / load-miss).

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 659
  ui_smoke_tests:         171
  ui_tests:               446
```

## Architecture

- **The inverse mode is an elevated DU on the Ui model** (`ConstructorMode = ForwardConstructor |
  InverseConstructor`), not a naked bool. `initInverse` is `initMainWith` + an unbound sample + the
  inverse mode, sharing the same table/palette/snap/proxy machinery — the "same constructor in an
  inverse state" the spec (Part L) describes.
- **The chart-visibility gate is a pure projection** (`experimentChartVisible`): forward ⇒ always;
  inverse ⇒ only when a Sample element is bound (a hint). It is applied at the three chart entry
  points, so the inline surface, the pop-out, and the Ψ/Δ readout all vanish together with no hint and
  behave as today once a hint is bound. No regression to the forward scene (the gate is inverse-only).
- **The collection builder lives in the Ui layer, NOT Controls** (Controls is not in `touches`): a
  Ui-composed `collectionBuilderView` stacks BELOW the untouched `ExperimentControls.view` in the
  experiments bay. It reuses the workbench button / automation-id helpers and follows the `selectStatus`
  precedent for its status line.
- **The load/validate orchestration is a pure Domain helper** (`ExperimentDataLoad.loadAndValidate`):
  resolve the experiment's detector kind through the Library, read through the `ExperimentDataProxy` in
  the shape that kind fixes, then `validateAgainstExperiment`. Pure given the proxies (records of
  functions), so it is driven end-to-end over mock proxies with no real IO — and is reusable by the
  Part L SolverHandoffWindow. The typed status DU (`ExperimentFileStatus`) is the Ui-side display
  projection that categorises the domain error as parse vs validation.
- **The file picker is a side-effecting seam** (`pickDataFile`, the `openChartWindowHook` precedent):
  the Attach button resolves its owner window from the click and opens an Avalonia `StorageProvider`
  picker, dispatching `AttachDataFileTo` on a confirmed selection. Wrapped so an unavailable provider
  (headless) degrades to a no-op; the tests dispatch `AttachDataFileTo` directly over mocks.
- **The two proxies thread by record update, not by widening `initMainWith`'s arity.** `initMainWith`
  / `initInverse` keep their 5-proxy signature and default the experiment proxies internally; the App
  (and `AppContextTests.surfaceOf`) override them with the shared app-scope instances via `{ … with
  experimentData = …; experimentCollections = … }`. This keeps the six existing `initMainWith` test
  call sites untouched while still "threading through the App composition" (see Gotchas).

## Deferred

- The `Inverse` launcher button (`Main / Inverse / Materials / Library`) and the whole-surface
  composition acceptance — spec Part N, steps 45 / 47 (this step only wires `initInverse` and the
  builder; no launcher button is added).
- `<UICOMP:SolverHandoffWindow>` — the summary + basic-validation screen at the end of the inverse flow
  (Part L): "all elements except sample specified; every experiment has a readable, schema-valid file".
  The reusable `ExperimentDataLoad.loadAndValidate` and per-experiment status are the seam it will read.
- Wiring the file picker's persisted last-folder (`EnvironmentSettings.lastFolders`, re-typed per §0.2)
  — Part L / step 45; the picker currently opens at the provider default.
- Re-validating a LOADED collection's attached files on load (the paths ride along, but statuses are
  session-derived and clear on load — re-attach to re-validate).
- Wiring the live `ExperimentCollection` as the app-scope `VersionsInUse` source (`AppContext.fs` §0.2)
  — a later composition step; `AppContext.create` still passes `VersionsInUse.empty`.

## Gotchas

- **`touches` excludes `OpticalConstructor.Controls`**, so the collection-builder UI is rendered in the
  Ui layer around the untouched `ExperimentControls.view` (a Ui-composed `experimentsBayContent`), NOT
  by growing the Controls bay. The per-experiment data-file rows are a parallel Ui section, not new
  `ExperimentControls` rows. Decision recorded here per the "record the interpretation" rule.
- **Single-case DU / same-named case shadowing** bit twice: `ExperimentCollectionStore.CollectionName.tryCreate`
  and `Experiments.DataFilePath.create` bind the union CASE (not the type's static member) in expression
  position — the `WorkbenchSettings.QuickPickThreshold` precedent already in this file. Fixed by
  `open …ExperimentCollectionStore` (then `CollectionName.tryCreate` resolves the type) and by using the
  `DataFilePath` case constructor directly (its `.create` factory is identical).
- **`initMainWith` arity was deliberately NOT widened.** An earlier draft added the two proxies as
  params and broke six existing `initMainWith` test call sites (LibraryWindow / Materials / MainWorkbench
  / OutOfBandBadge / AppContext tests). The final shape keeps the 5-proxy signature and threads the two
  experiment proxies by record update in the App / `surfaceOf`, so those call sites are untouched — the
  low-churn interpretation of "thread through the App composition mechanically".
- **The inverse seeded source / detector are UNBOUND** (like `initMainWith`), so the seeded scene
  charts through the existing defaults (600 nm, Intensity). `defaultSeedEntry` / `SeedEntryIds` are
  defined LATER in the file than the init functions, so pre-binding them there would be a forward
  reference; a future step that wants a fully-specified inverse seed can pre-bind after that point.
- **The picker (`StorageProvider`) is net-new IO and off every gate's path** — no headless test clicks
  Attach; the acceptance drives `AttachDataFileTo` directly over mocks. The picker is wrapped in
  `try/with` so a headless / provider-less host never throws.
- **Invariant 6 respected.** The local build + suite runs recorded here are advisory; the arc-runner's
  gate engine re-runs every gate after this session exits and is the sole authority. The runs keep the
  SoW baseline counts honest (the prior-worker convention in this arc).
- **System-prompt path drift (carried from steps 035/036).** The task file's
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md` is stale; the real prompts live under
  `.../src/ai_strategy_generator/multistep/`. Located and read there. No scope impact.

## Changelog

- 2026-07-11 — Step 037 (IMPLEMENT): wired the inverse constructor state (`initInverse` — unbound
  sample, `ConstructorMode`, the `experimentChartVisible` hint gate) and the experiment-collection
  builder (Ui-layer name / save / list / load through `ExperimentCollectionProxy`; per-experiment
  data-file attach that loads + validates through `ExperimentDataProxy` into a typed
  `ExperimentFileStatus`). Added the pure Domain helper `ExperimentDataLoad`. Threaded both proxies
  through the App composition (`AppContext` + record-update). Added `InverseConstructorTests` (11
  facts, incl. one `ui-smoke`). Build green; suites 119 / 659 / 171 / 446.
