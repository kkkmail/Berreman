# State of the world — step 028 (ADD_CONTRACT STORE_XDUO_0005 ExperimentCollectionProxy)

## Where we are

Spec 0038 Part I's persistence arc. Step 025 grew the `Experiment` into a
full-setup value (an ordered `ElementDescriptor` chain plus an optional
`DataFilePath` attachment); steps 026/027 declared and implemented the scene
persistence seam (`SceneProxy`). This step declares the sibling
experiment-collection persistence seam (`STORE_XDUO_0005`,
`ExperimentCollectionProxy`) at DECLARED lifecycle — the elevated `CollectionName`
key, the pure `ExperimentCollectionSnapshot` over a NAMED list of those step-25
experiments, the error channel, and the `[<ReferenceEquality>]` proxy — with an
inline mock and a mock-driven test. No real store; the later
`IMPLEMENT_CONTRACT STORE_XDUO_0005` builds the file-backed adapter in
`OpticalConstructor.Storage` and wires the live collection as the app-scope
`VersionsInUse` source.

## What's working

- Declare the experiment-collection persistence seam in
  `OpticalConstructor.Domain` (`ExperimentCollectionStore.fs`): `CollectionName`
  (blank/null-rejecting `tryCreate`), `ExperimentCollectionSnapshot` (name +
  step-25 full-setup `Experiment` list, each with its optional `DataFilePath`),
  `ExperimentCollectionError`, and the `[<ReferenceEquality>]`
  `ExperimentCollectionProxy`.
- Add an inline map-backed mock and a mock-driven test
  (`ExperimentCollectionProxyTests.fs`) exercising `saveCollection`,
  `tryLoadCollection` (hit + miss), and `listCollections` through their exact
  signatures, asserting the optional `DataFilePath` attachment round-trips per
  experiment.
- Register both files last in their respective `.fsproj` compile orders.
- Keep the contract DECLARED — no disk format, no store body, no `.ocproj` wiring.

## Tests

Per the ADD_CONTRACT act-only invariant, the worker ran NO gates this round;
the arc-runner's gate engine is the sole authority. Static verification only:
compile order, exhaustive matches, elevated `.value`/`tryCreate`, the
`[<ReferenceEquality>]` proxy, LF line endings, and the mock-driven test driving
all three proxy fields through their exact signatures.

Expected roster (engine-run): `build`, `unit-tests`, `constructor-unit-tests`,
`ui-smoke`, `ui-tests`. `constructor-unit-tests` gains 6 cases (a 2-case
`[<Theory>]` + four `[<Fact>]`), 612 → 618; the other gate counts are untouched.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 618
  ui_smoke_tests: 158
  ui_tests: 425
```

## Architecture

- The seam follows the `Scene.SceneProxy` (`Scene.fs`) convention verbatim: error
  channel declared first (so `CollectionName.tryCreate` can name `InvalidCollection`),
  then the elevated key, then the pure snapshot, then the `[<ReferenceEquality>]`
  functional-proxy record of camelCase `Result`-returning functions.
- `ExperimentCollectionSnapshot` REUSES `Experiments.Experiment` rather than
  re-declaring a parallel experiment shape — the snapshot is a NAMED list of the
  exact full-setup experiments the bay already builds, so a save-load is lossless
  and the `DataFilePath` attachment rides along for free.
- `saveCollection : snapshot -> Result<unit, _>` (the snapshot embeds its own
  `name`), deliberately unlike `SceneProxy.saveScene : name -> snapshot -> _` — the
  slice pins that signature.

## Deferred

- The real, file-backed `ExperimentCollectionProxy.create` in
  `OpticalConstructor.Storage` (the `IMPLEMENT_CONTRACT STORE_XDUO_0005` cycle),
  plus the `.ocproj` disk format and the wiring of the live `ExperimentCollection`
  as the app-scope `VersionsInUse` source (the `AppContext.fs` §0.2 forward
  reference). Out of scope for a DECLARED contract.

## Gotchas

- The module is `ExperimentCollectionStore`, NOT `ExperimentCollection` — the
  latter is an existing TYPE in the `Experiments` module (the editable
  draft-plus-list collection). The store name avoids the collision and parallels
  `SampleStore`/`MaterialStore`.
- DECLARED lifecycle only: no `createInMemory`, no disk format, no wiring. The
  later IMPLEMENT step builds the real store.
- The system-prompt path in the task file
  (`C:\GitHub\AI-Strategy-Generator\add_contract_worker.system-md`) is stale; the
  file actually lives under `.../src/ai_strategy_generator/multistep/`. No scope
  impact.

## Changelog

- 2026-07-11 — step 028: declared the `ExperimentCollectionProxy` persistence seam
  (`CollectionName` / `ExperimentCollectionSnapshot` / `ExperimentCollectionError`
  / `[<ReferenceEquality>]` proxy) in `ExperimentCollectionStore.fs` plus an inline
  mock and a mock-driven test in `ExperimentCollectionProxyTests.fs`; both
  registered last in their fsprojs. DECLARED lifecycle — no real store.
