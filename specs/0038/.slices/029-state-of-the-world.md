# State of the world — step 029 (IMPLEMENT_CONTRACT STORE_XDUO_0005 ExperimentCollectionProxy)

## Where we are

Spec 0038 Part I's persistence arc. Step 025 grew the `Experiment` into a
full-setup value (an ordered `ElementDescriptor` chain plus an optional
`DataFilePath` attachment); steps 026/027 declared and implemented the scene
persistence seam (`SceneProxy`); step 028 declared the sibling
experiment-collection seam (`ExperimentCollectionProxy`) at DECLARED lifecycle.
This step IMPLEMENTS it: a real, stateful in-memory
`ExperimentCollectionProxy.createInMemory` behind the seam, plus round-trip unit
tests over deterministic experiment data. The contract moves DECLARED →
implemented. Still no disk format and no `.ocproj`/`AppContext` wiring — the
file-backed `create` in `OpticalConstructor.Storage` remains a later cycle.

## What's working

- Implement the real in-memory `ExperimentCollectionProxy.createInMemory`
  (`ExperimentCollectionStore.fs`): a `ref Map<CollectionName,
  ExperimentCollectionSnapshot>` inside the closure; `saveCollection`
  re-validates the snapshot's own name and upserts, `tryLoadCollection` and
  `listCollections` read the map — the `SceneProxy.createInMemory` precedent.
- Reject a directly-constructed blank `CollectionName` at `saveCollection` as
  `InvalidCollection`, storing nothing (name re-validation at the store boundary).
- Add `ExperimentCollectionRoundTripTests.fs` (5 facts): a collection of a full
  E1 setup, a sample-less E2, and a dark E3 — with and without `DataFilePath`
  attachments — saves, lists, and loads value-identically, preserving ordered
  setups, the optional sample/source/dark shapes, and the per-experiment
  attachments; plus upsert, load-miss, and multi-collection listing.
- Register the new test file last in `OpticalConstructor.Tests.fsproj`; the
  Domain `createInMemory` is an intrinsic augmentation, so no Domain
  compile-order change.

## Tests

Per the IMPLEMENT_CONTRACT act-only invariant (Invariant 6), the worker ran NO
gates this round; the arc-runner's gate engine is the sole authority. Static
verification only: the `createInMemory` shape mirrors the green
`SceneProxy.createInMemory`; the round-trip test reuses the green
`ExperimentProxyTests.fs` fixtures and `SceneRoundTripTests.fs` assertion idioms;
exhaustive matches, used opens, and LF line endings on every touched file.

Expected roster (engine-run): `build`, `unit-tests`, `constructor-unit-tests`,
`ui-smoke`, `ui-tests`. Only `constructor-unit-tests` changes: +5 `[<Fact>]`
cases, 618 → 623. The other gate counts are untouched.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 623
  ui_smoke_tests: 158
  ui_tests: 425
```

## Architecture

- `createInMemory` follows `Scene.SceneProxy.createInMemory` (`Scene.fs`)
  verbatim: mutation (a `ref` map) is captured INSIDE the closure (the IO
  boundary), so logic that holds the proxy stays pure and a test substitutes the
  whole `createInMemory` (or a stub of the same shape). It stays an INTRINSIC
  augmentation in `ExperimentCollectionStore.fs` because it needs no type
  compiled after that module — unlike the versioned `SampleStore`/`MaterialStore`,
  whose `decideVersioning` rule forces them into their own files.
- `saveCollection` keys the map by the snapshot's OWN `name` (the declared
  signature embeds the name in the snapshot, deliberately unlike
  `SceneProxy.saveScene : name -> snapshot -> _`) and RE-validates it through
  `CollectionName.tryCreate`, mirroring `SceneProxy.saveScene`'s blank-key guard.
- The round-trip is lossless because `ExperimentCollectionSnapshot` reuses
  `Experiments.Experiment` directly — the snapshot is a NAMED list of the exact
  full-setup experiments the bay builds, so ordered setups, versioned sample
  bindings, the dark (`varied = None`) shape, and the `DataFilePath` attachment
  all ride along for free under structural equality.

## Deferred

- The real, file-backed `ExperimentCollectionProxy.create` in
  `OpticalConstructor.Storage`, the `.ocproj` disk format, and wiring the live
  `ExperimentCollection` as the app-scope `VersionsInUse` source
  (`AppContext.fs` §0.2) — a later storage/wiring cycle, exactly as the step-028
  declaration and the module doc-comment forecast.

## Gotchas

- The system-prompt path in the task file
  (`C:\GitHub\AI-Strategy-Generator\implement_contract_worker.system-md`) is
  stale; the file actually lives under
  `.../src/ai_strategy_generator/multistep/implement_contract_worker.system-md`.
  No scope impact — I read it there.
- `Map<CollectionName, _>` relies on `CollectionName` deriving `comparison` (a
  single-case DU over a `string`); this is the same derivation `SceneName` uses to
  key `Map<SceneName, SceneSnapshot>`. Do not add `[<CustomEquality>]` /
  `[<NoComparison>]` to `CollectionName` or the store stops compiling.
- The step-028 inline MOCK keyed its stub map by `CollectionName.value` (a
  `Map<string, _>`); the real store keys by the elevated `CollectionName` per the
  slice's pinned `ref Map<CollectionName, ExperimentCollectionSnapshot>`. Both
  round-trip identically — the difference is only the stub's inlining choice.
- A dark E3 carrying a `DataFilePath` is representable and intentional in the
  test (the attachment is orthogonal to the setup shape); it exercises that the
  optional file survives even on a setup-less experiment.

## Changelog

- 2026-07-11 — step 029: implemented the real in-memory
  `ExperimentCollectionProxy.createInMemory` (`ref`-map store; name-revalidating
  upsert; read-through load/list) in `ExperimentCollectionStore.fs`, moving the
  contract DECLARED → implemented; added `ExperimentCollectionRoundTripTests.fs`
  (5 facts over E1/E2/E3 with and without `DataFilePath`) and registered it last
  in the Tests fsproj. No disk format / wiring — deferred to the storage cycle.
