# Impl-plan — step 029 (IMPLEMENT_CONTRACT STORE_XDUO_0005 ExperimentCollectionProxy)

## Goal

Bring `ExperimentCollectionProxy` (declared at step 028) to `implemented`
lifecycle: a real, stateful **in-memory** store behind the seam, plus round-trip
unit tests over deterministic experiment data. No disk format, no `.ocproj`
wiring, no `AppContext` wiring — the file-backed `create` in
`OpticalConstructor.Storage` is a later cycle (explicitly deferred by the step-028
declaration).

## Approach

1. **`OpticalConstructor.Domain/ExperimentCollectionStore.fs`** — add the
   `ExperimentCollectionProxy.createInMemory` type augmentation, verbatim against
   the `Scene.SceneProxy.createInMemory` precedent (`Scene.fs`):
   - Close over `let store = ref (Map.empty : Map<CollectionName, ExperimentCollectionSnapshot>)`
     inside the closure (the IO boundary), so logic holding the proxy stays pure.
   - `saveCollection snapshot` upserts under the snapshot's OWN `name`,
     RE-validating it through `CollectionName.tryCreate snapshot.name.value` so a
     directly-constructed blank key is rejected as `InvalidCollection` rather than
     silently stored (mirrors `SceneProxy.saveScene`'s re-validation). Note the
     signature difference from `SceneProxy`: the name lives INSIDE the snapshot,
     so there is no separate `name` argument.
   - `tryLoadCollection name = Ok (store.Value |> Map.tryFind name)`.
   - `listCollections () = Ok (store.Value |> Map.toList |> List.map fst)`.
   - Stays an INTRINSIC augmentation in this file (needs no type compiled after
     it), exactly like `SceneProxy.createInMemory`.

2. **`OpticalConstructor.Tests/ExperimentCollectionRoundTripTests.fs`** (new) —
   round-trip tests over deterministic data, mirroring `SceneRoundTripTests.fs`:
   - E1 = a full setup (source + polarizer + SAMPLE bound by version + rotating
     polarizer + detector), WITH a `DataFilePath` attachment.
   - E2 = the sample-LESS variant (source + two polarizers + detector), WITHOUT a
     data file.
   - E3 = an empty DARK line (`varied = None`, empty setup), WITH an attached
     calibration file — proving the optional attachment rides along even on a
     setup-less experiment.
   - `createInMemory` → `saveCollection` → `listCollections` → `tryLoadCollection`
     reproduces the whole `ExperimentCollectionSnapshot` value-identically
     (`Assert.Equal<ExperimentCollectionSnapshot>`), plus explicit per-facet
     assertions on ordered setups, the sample/source/dark shapes, and the
     per-experiment `dataFileOpt`.
   - Plus upsert (overwrite same name), load-miss (`Ok None`), blank-name
     rejection (`InvalidCollection` + nothing stored), and multi-collection
     listing.
   - Register the file LAST in `OpticalConstructor.Tests.fsproj`.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ExperimentCollectionStore.fs`
  (add the `createInMemory` augmentation).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentCollectionRoundTripTests.fs`
  (new).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
  (register the new file).

## Risks / notes

- `Map<CollectionName, _>` needs `CollectionName` to support comparison — it is a
  single-case DU over a `string`, so structural comparison is derived (same as
  `SceneName` keying `Map<SceneName, SceneSnapshot>`). No `[<CustomEquality>]` in
  the way.
- The proxy record is `[<ReferenceEquality>]` (function-valued fields); the
  round-trip test never compares two proxies, only snapshots.
- Act-only invariant (IMPLEMENT_CONTRACT / Invariant 6): the worker runs NO gates.
  Correctness is by construction against the read precedents; the arc-runner's
  gate engine runs build / unit-tests / constructor-unit-tests / ui-smoke /
  ui-tests after exit.
- LF line endings only.
