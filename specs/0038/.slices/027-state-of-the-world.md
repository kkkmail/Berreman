# Step 027 — state of the world

## Where we are

Spec 0038, step 027 (IMPLEMENT_CONTRACT `STORE_XDUO_0004` SceneProxy). Step 026 DECLARED the
scene-persistence seam (the elevated `SceneName`, the pure `SceneSnapshot`, the `SnapMode` flag,
the `SceneStoreError` channel, and the `[<ReferenceEquality>] SceneProxy` — with an inline mock).
This slice makes it REAL: a stateful in-memory `SceneProxy.createInMemory` behind that surface,
plus the pure `captureScene` / `restoreScene` pair in the workbench host that turns the live Main
scene into a `SceneSnapshot` and back. The seam is now `implemented` — the host can save and load a
scene entirely as pure data, and a future storage cycle can swap the in-memory `create` for a
disk-backed one with no change to the host.

## What's working

- Implement `SceneProxy.createInMemory` in `Scene.fs` (Domain) — a `ref (Map<SceneName,
  SceneSnapshot>)` closure (the `SampleProxy.createInMemory` precedent), starting empty:
  `saveScene` re-validates the name (blank → `InvalidScene`) then upserts, `tryLoadScene` reads
  the map, `listScenes` returns the keys; mutation stays inside the closure so the host stays pure.
- Add the pure, Avalonia-free `captureScene` / `restoreScene` pair in the workbench host: capture
  projects the live model's elements (id + placement carrying CatalogueKind and the `valueId`
  binding + zoom), the table view state, and the snap flag (elevated to `SnapMode`); restore
  applies a snapshot back, replacing elements / view / snap and resetting selection to the table.
- Verify the acceptance round-trip with deterministic data: capture → `saveScene` →
  `tryLoadScene` → `restoreScene` onto a different base model reproduces element ids, kinds,
  placements, bindings, and view state; plus upsert-overwrite, load-miss, and blank-name rejection.
- Leave the on-disk scene format and the `.ocproj` / Save-Load-verb host wiring for a later slice.

## Tests

Gate execution is the arc-runner gate engine's job (IMPLEMENT_CONTRACT Invariant 6 — the worker
acts and runs no checks; the base protocol's "run gates locally" is the legacy single-worker path,
superseded under per-anchor dispatch). Static verification only this round — reads + a careful
compile-order, exhaustiveness (`--warnaserror+:25`), and record-field-ambiguity review — per the
step 021/022/026 precedent. Step-027 roster: `build`, `unit-tests`, `constructor-unit-tests`,
`ui-smoke`, `ui-tests`.

- `constructor-unit-tests` (`SceneRoundTripTests.fs`, new): the acceptance round-trip through
  `SceneProxy.createInMemory` + `captureScene` / `restoreScene`, plus `saveScene` upsert,
  `tryLoadScene` load-miss, and the blank-name `InvalidScene` rejection. Net +4 cases, no regression.
- `build`, `unit-tests` (BerremanTests), `ui-smoke`, and `ui-tests` are untouched — the Domain and
  Ui changes are pure additions (a type-augmentation member and two pure functions), touching no
  existing view render or model reducer.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 612
  ui_smoke_tests: 158
  ui_tests: 425
```

## Architecture

- `SceneProxy.createInMemory` is an INTRINSIC augmentation staying in `Scene.fs` (the
  `CategoryProxy.createInMemory` precedent, `MaterialLibrary.fs`) — it needs no type compiled after
  `Scene.fs` (which already compiles LAST in the Domain fsproj). This differs from `SampleProxy` /
  `MaterialProxy`, whose `createInMemory` lives in a later file (`SampleStore.fs` / `MaterialStore.fs`)
  only because they apply `decideVersioning` from `Lifecycle.fs`.
- The store keys a `Map` by the elevated `SceneName` (a `string`-wrapping single-case DU, so F#
  auto-derives `comparison`), mutating a `ref` inside the closure — the IO boundary — so logic
  holding the proxy stays referentially transparent.
- `captureScene` / `restoreScene` live in the workbench host (`TableAndElementRotationView.fs`, the
  Ui) because they map between the live `Model` / `TestElement` and the Domain `SceneSnapshot` /
  `SceneElement`. They are pure and window-free — the "state is separable from the UI" discipline —
  so a headless (indeed Avalonia-free) unit test round-trips them. `restoreScene` replaces only the
  scene-owned fields (elements, view, snap) plus the two that index the replaced elements
  (selection → table, drag → cleared), leaving every injected proxy / palette / render field intact.

## Deferred

- **The on-disk scene format and host wiring.** No `.ocproj` scene section and no Save-Scene /
  Load-Scene workbench verbs — the disk-backed `SceneProxy.create` and the host verbs that reach
  the seam land in a later slice (the storage cycle in `OpticalConstructor.Storage`).
- **Table plate + other aggregates in the snapshot.** `SceneSnapshot` still captures exactly the
  step-026 fields (elements + view state + snap flag); persisting the `OpticalTable` plate or the
  experiment collection with a scene is a future extension.

## Gotchas

- **`saveScene` re-validates the name.** The `SceneName` union case is public, so a caller CAN
  build `SceneName ""` outside `tryCreate`; the store re-runs `SceneName.tryCreate name.value` and
  rejects a blank as `InvalidScene` rather than storing an unaddressable key — the how-to's
  "saveScene validates the name (blank yields InvalidScene)".
- **Record-field ambiguity.** `TestElement` (Ui) and `Scene.SceneElement` (Domain) share the exact
  field set `{ id; placement; zoom }` and are both in scope in the capture/restore functions and
  the test — every element-record literal qualifies its leading label (`Scene.SceneElement.id` /
  `TestElement.id`) to pin the intended type.
- **`restoreScene` is deliberately minimal + resets `selection`/`drag`.** The how-to says "applying
  a snapshot back onto a model"; the interpretation chosen (per "don't ask the user") replaces only
  scene-owned fields and resets the two that index the now-fresh element list, leaving all injected
  seams untouched — the safest reading, consistent with the surrounding index-safety discipline.
- **The test is in `OpticalConstructor.Tests`** (per the slice's `touches`), constructing the
  windowless `initMain ()` / `init ()` pure model constructors already exercised under plain
  `[<Fact>]` in the Ui.Tests project (no Avalonia session needed; the Tests project references Ui).
- **The contract-ids registry is not edited.** `.contract-ids/XDUO-json`'s `status: active` is the
  arc-runner-owned (`.lock`-guarded) ID-allocation state, not the declared→implemented lifecycle —
  STORE_XDUO_0002 stays `active` there despite being implemented at step 022. The arc-runner's
  contract-implementation gate records lifecycle in its own state after the worker exits.

## Changelog

- 2026-07-11 — Step 027 (IMPLEMENT_CONTRACT `STORE_XDUO_0004`): implemented the scene-persistence
  seam. Added `SceneProxy.createInMemory` (a `ref`-map in-memory store — validate-and-upsert
  `saveScene`, map-read `tryLoadScene` / `listScenes`) as an intrinsic augmentation in `Scene.fs`;
  added the pure `captureScene` / `restoreScene` pair in the workbench host
  (`TableAndElementRotationView.fs`); and added `SceneRoundTripTests.fs` driving the
  capture→save→load→restore acceptance round-trip plus upsert / load-miss / blank-name cases.
