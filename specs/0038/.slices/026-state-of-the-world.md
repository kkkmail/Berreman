# Step 026 — state of the world

## Where we are

Spec 0038, step 026 (ADD_CONTRACT `STORE_XDUO_0004` SceneProxy). With the versioned
material and sample stores (steps 021/022) and the grown Experiments domain (step 025)
in place, this slice declares the last store seam of the persistence family: the
**scene persistence seam**. `OpticalConstructor.Domain` now carries a `SceneProxy`
functional proxy plus the elevated `SceneName` key, the pure `SceneSnapshot` that
captures the constructor scene as data, the `SnapMode` snap flag, and the
`SceneStoreError` channel — at DECLARED lifecycle. A future storage cycle
(`IMPLEMENT_CONTRACT`) swaps in a real, file-backed store in `OpticalConstructor.Storage`
with no change to the bay/host logic that holds the proxy.

## What's working

- Declare the scene persistence seam in `OpticalConstructor.Domain` (new `Scene.fs`):
  `SceneName` (blank/null-rejecting `tryCreate`), `SceneSnapshot`, `SceneStoreError`,
  and the `[<ReferenceEquality>] SceneProxy` with `saveScene` / `tryLoadScene` /
  `listScenes` at their exact signatures.
- Elevate the snap flag to a two-case `SnapMode` DU (never a naked bool), with a
  `.value`/`create` wire mapping for the host model's `snapChain : bool`.
- Capture each scene element as `{ id; placement; zoom }`, reusing `ElementPlacement`
  (which already carries the CatalogueKind tag and the valueId Library binding) rather
  than duplicating derivable fields.
- Provide an inline map-backed mock `SceneProxy` and a mock-driven test exercising every
  proxy field through its exact signature (save → load round-trip, load miss, list).
- Leave the seam DECLARED — no disk format and no `.ocproj` wiring this round.

## Tests

Gate execution is the arc-runner gate engine's job (ADD_CONTRACT Invariant 6 — the
worker acts and runs no checks). Static verification only this round (reads/grep + a
careful compile-order and exhaustiveness review), per the step 024/025 precedent.
Step-026 roster: `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`,
`ui-tests`.

- `constructor-unit-tests` (`SceneProxyTests.fs`, new): the mock-driven acceptance test
  drives `saveScene` / `tryLoadScene` (hit + miss) / `listScenes` through their exact
  signatures over a `ref`-map stub; plus `SceneName.tryCreate` blank/null rejection and
  the happy round-trip, `SnapMode` wire-form round-trip, and the proxy's reference
  equality. Net +7 cases, no regression.
- `unit-tests` (BerremanTests), `ui-smoke`, and `ui-tests` are untouched — the change is
  confined to `Scene.fs` (Domain) and its test.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 596
  ui_smoke_tests: 158
  ui_tests: 425
```

## Architecture

- `Scene.fs` compiles LAST in the Domain fsproj (after `SampleStore.fs`). It reuses
  sibling Domain types by qualified name — `Library.ElementId`,
  `Placement.ElementPlacement`, `Table.TableViewState` — with no namespace open (the
  `Project.fs` precedent), so it introduces no new dependency edge.
- The proxy follows the established functional-proxy convention (`Library.LibraryProxy`,
  `MaterialProxy`, `SampleProxy`, `CategoryProxy`): a `[<ReferenceEquality>]` record of
  camelCase `Result`-returning functions, so an Elmish host model can hold one and keep
  its required equality (comparing the proxy by identity).
- `SceneSnapshot` is pure data only — no live element reference and no Avalonia handle —
  so a captured scene survives its authoring session and is trivially serializable by
  the future store. `SceneElement` mirrors the live Main-scene `TestElement` over Domain
  types (the Domain cannot reference the Ui).
- The snap flag is elevated to `SnapMode` (`SnapToBeam | FreePlacement`) with a wire
  `.value`, mirroring `WorkbenchSettings.SelectWindowModality`.

## Deferred

- **The real, file-backed store.** No disk format and no `.ocproj` wiring — the concrete
  `SceneProxy.create` (and the on-disk scene format) land in a later
  `IMPLEMENT_CONTRACT STORE_XDUO_0004` cycle in `OpticalConstructor.Storage`.
- **Host wiring.** Nothing in the Ui/App holds a `SceneProxy` yet; wiring the Main
  workbench's Save/Load-scene verbs to the seam is a later slice.
- **Table plate + other aggregates in the snapshot.** This slice captures exactly the
  scene fields the how-to enumerates (elements + view state + snap flag); if a future
  slice needs the `OpticalTable` plate persisted with a scene, it extends `SceneSnapshot`.

## Gotchas

- **"CatalogueKind" and "valueId binding" live inside `placement`.** The how-to lists
  them per element, but `ElementPlacement.catalogueKind` / `.valueId` already carry them,
  so `SceneElement = { id; placement; zoom }` captures all five enumerated facts through
  one reused type — it does NOT add separate `catalogueKind` / `valueId` fields.
- **`zoom` is a bare `float`** to stay consistent with the sibling display scalars
  (`TableViewState.zoom`, `TestElement.zoom`, `Vector3` components); no `Zoom` DU exists
  and adding one here alone would be inconsistent and out of scope.
- **`SnapMode`, not `bool`.** The persisted snap flag is the two-case DU; the host's
  `snapChain : bool` crosses the boundary via `SnapMode.value`/`create`.
- **The mock lives in the test** (an inline `ref`-map stub), not in the Domain — the
  DECLARED contract ships types + mock + test only; the real store is a later cycle.

## Changelog

- 2026-07-11 — Step 026 (ADD_CONTRACT `STORE_XDUO_0004`): declared the scene persistence
  seam in `OpticalConstructor.Domain` (`Scene.fs`) — `SceneName` (blank/null-rejecting
  `tryCreate`), `SnapMode` two-case DU, `SceneSnapshot` (elements + table view state +
  snap mode), `SceneStoreError`, and the `[<ReferenceEquality>] SceneProxy`
  (`saveScene`/`tryLoadScene`/`listScenes`); added an inline map-backed mock and a
  mock-driven test exercising every field. DECLARED lifecycle — no disk format, no
  `.ocproj` wiring.
