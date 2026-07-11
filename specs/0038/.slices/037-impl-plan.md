# Step 037 — impl-plan (IMPLEMENT: inverse constructor state + collection builder)

## Scope (touches: Ui, Domain, App, Ui.Tests)

Wire the **inverse constructor state** and the **experiment-collection builder** on the
Main workbench, threading the step-29/36 `ExperimentCollectionProxy` and
`ExperimentDataProxy` through the App composition. Controls is NOT in the touches list —
the collection-builder UI is rendered in the **Ui** layer around the untouched
`ExperimentControls.view`.

## Approach

### Domain (`OpticalConstructor.Domain`)
- New pure module `ExperimentDataLoad.fs` (compiled last, after `ExperimentDataProxy.fs`):
  - `detectorKindOf library experiment : DetectorKind` — resolve the experiment's detector
    descriptor preset id through the Library (default Intensity).
  - `loadAndValidate library dataProxy experiment path : Result<MeasuredSeries, ExperimentDataError>`
    — read through the `ExperimentDataProxy` in the shape the detector kind fixes, then
    `MeasuredData.validateAgainstExperiment` against the experiment's varied param + range.
  Pure given the proxies; reusable by the future SolverHandoffWindow.

### Ui (`TableAndElementRotationView.fs`)
- `ConstructorMode = ForwardConstructor | InverseConstructor` (elevate the bool) + an
  `ExperimentFileStatus` typed DU (Loaded/ParseError/ValidationError) with a `.text`.
- Model gains: `constructorMode`, `experimentData` (proxy), `experimentCollections` (proxy),
  `collectionName : string`, `savedCollections : CollectionName list`,
  `collectionStatus : string option`, `experimentDataStatus : Map<int, ExperimentFileStatus>`.
- `initWith` gains the two proxies + defaults; `initMainWith` threads them; `init`/`initMain`
  build defaults (`ExperimentDataStore.createFileBacked` + `ExperimentCollectionProxy.createInMemory`).
- `initInverse` beside `initMainWith`: seeds source + **unbound** sample + detector, palette,
  `snapChain`, `constructorMode = InverseConstructor`.
- Chart gate: `experimentChartVisible` — forward ⇒ always; inverse ⇒ only when a sample is
  bound (a hint). `experimentResult` / `experimentPsiDelta` / `chartForExperiment` return
  empty when not visible ⇒ the inline chart surface is ABSENT with no hint.
- Messages: `CollectionSetName`, `CollectionSave`, `CollectionLoad`,
  `AttachDataFileTo of ExperimentId * DataFilePath`; update arms save/list/load through the
  collection proxy and load/validate/status through `ExperimentDataLoad`.
- A Ui-layer `collectionBuilderView` (name field, Save, saved-collection list, status; per
  experiment an Attach button + typed status) stacked with `ExperimentControls.view` in the
  experiments bay. The Attach button opens a StorageProvider picker hook (the
  `openChartWindowHook` seam precedent; no-op safety default) and dispatches `AttachDataFileTo`.

### App (`Program.fs` / `AppContext.fs`)
- `AppContext` gains `experimentData` + `experimentCollections` built in `create`
  (`ExperimentDataStore.createFileBacked` + `ExperimentCollectionProxy.createInMemory`).
- `MainConstructorWindow` threads both into `initMainWith`.

### Ui.Tests
- Inverse init: unbound sample + `experimentChartVisible = false` (no chart).
- Binding a hint (BindValueIdTo on the sample) restores the chart.
- Attaching a file over a mock `ExperimentDataProxy` records the typed status (ok + failures).
- A collection round-trips through save/list/load over the in-memory proxy (incl. an
  attached `dataFileOpt`).
- Update `AppContextTests.surfaceOf` for the new `initMainWith` arity + `AppContext` fields.
- One ui-smoke render of the inverse mainView (collection builder) without throwing.

## Risks
- `initMainWith` arity change breaks Program.fs + AppContextTests — both in scope, updated.
- StorageProvider picker is net-new (Avalonia 12); wrapped in try/with, no gate exercises it.
- Model equality: new proxy fields are `[<ReferenceEquality>]`; the rest are equatable.
- Keep the new bay section throw-free so ui-smoke render stays green.
