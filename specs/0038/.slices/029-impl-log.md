# Impl-log — step 029 (IMPLEMENT_CONTRACT STORE_XDUO_0005 ExperimentCollectionProxy)

## Progress

- [x] Read system prompt, project prompt, slice spec, and the read precedents
      (`Scene.fs` `SceneProxy.createInMemory`, `SampleStore.fs`,
      `Experiments.fs`, `SceneRoundTripTests.fs`,
      `ExperimentCollectionProxyTests.fs`, `ExperimentProxyTests.fs`).
- [x] Implement `ExperimentCollectionProxy.createInMemory` in
      `ExperimentCollectionStore.fs`.
- [x] Add `ExperimentCollectionRoundTripTests.fs` and register it in the Tests
      fsproj.
- [x] Write state-of-the-world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ExperimentCollectionStore.fs`
  — added the `ExperimentCollectionProxy.createInMemory` type augmentation: a
  `ref (Map<CollectionName, ExperimentCollectionSnapshot>)` captured inside the
  closure; `saveCollection` re-validates `snapshot.name` and upserts,
  `tryLoadCollection` / `listCollections` read the map. Contract lifecycle moves
  DECLARED → implemented.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentCollectionRoundTripTests.fs`
  — new; 5 `[<Fact>]` round-trip tests over deterministic E1/E2/E3 data.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
  — registered the new test file last in compile order.

## Decisions

- **Signature of `saveCollection`.** The declared contract's `saveCollection`
  takes the snapshot alone (`ExperimentCollectionSnapshot -> Result<unit, _>`) —
  the name lives INSIDE the snapshot — unlike `SceneProxy.saveScene : name ->
  snapshot -> _`. So `createInMemory` keys the map by `snapshot.name` and
  re-validates it through `CollectionName.tryCreate snapshot.name.value` (the
  `SceneProxy.createInMemory` re-validation habit) to reject a
  directly-constructed blank key as `InvalidCollection` rather than silently
  storing an unaddressable key.
- **Intrinsic augmentation, not a new file.** Like `SceneProxy.createInMemory`
  (and unlike the versioned `SampleStore`/`MaterialStore`, which need
  `Lifecycle.fs`'s `decideVersioning`), the store needs no type compiled after
  `ExperimentCollectionStore.fs`, so the `createInMemory` augmentation stays in
  that same file. No fsproj compile-order change in the Domain project.
- **Map keyed by `CollectionName`.** The slice pins `ref Map<CollectionName,
  ExperimentCollectionSnapshot>`; `CollectionName` is a single-case DU over a
  `string`, so structural comparison is derived (the same way `SceneName` keys
  `Map<SceneName, SceneSnapshot>`). The step-028 mock keyed by `.value` for its
  inline stub; the real store keys by the elevated type per the slice.
- **E1/E2/E3 with/without attachments.** E1 = a full setup (source + polarizer +
  sample bound BY VERSION + rotating polarizer + detector) WITH a data file; E2 =
  the sample-less variant WITHOUT a data file; E3 = an empty dark line
  (`varied = None`) WITH a calibration file — so the round-trip exercises the
  optional `DataFilePath` both present and absent, and proves the attachment
  survives even on the setup-less dark experiment. Fixtures mirror
  `ExperimentProxyTests.fs`'s E1/E2/E3.

## Testing state

Per the IMPLEMENT_CONTRACT act-only invariant (Invariant 6), the worker ran NO
gates this round — the arc-runner's deterministic gate engine is the sole gate
authority and runs the roster after exit. Verification was static, against the
green baseline precedents:

- The `createInMemory` augmentation is byte-for-byte structurally the
  `SceneProxy.createInMemory` shape (`ref`-map, upsert with name re-validation,
  read-through load/list) — a proven-green pattern.
- The round-trip test reuses the exact fixture idioms of the green
  `ExperimentProxyTests.fs` (E1/E2/E3, `glassPlateBinding`, `desc`) and the
  assertion idioms of the green `SceneRoundTripTests.fs`
  (`Assert.Equal<Snapshot>`, list/`Set` membership, blank-name rejection).
- All matches are exhaustive (no FS0025); all `open`s are used; LF endings
  verified on every touched file; x64/`net10.0` unchanged.

Expected roster (engine-run): `build`, `unit-tests`, `constructor-unit-tests`,
`ui-smoke`, `ui-tests`. Only `constructor-unit-tests` changes: +5 cases
(618 → 623). `berreman_unit_tests`, `ui_smoke_tests`, `ui_tests` are untouched.

`commit_ready: true` — every requirement of the slice (the real in-memory
`createInMemory`, and round-trip unit tests over E1/E2/E3 with and without
attachments proving ordered setups / optional shapes / the dark experiment /
`DataFilePath`) lands this round.

## Artifacts

None — a pure Domain + Tests round; no captured logs, screenshots, or traces.
