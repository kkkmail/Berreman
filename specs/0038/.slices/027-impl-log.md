# Step 027 — impl-log (IMPLEMENT_CONTRACT STORE_XDUO_0004 SceneProxy)

## Progress

- [x] Domain: `SceneProxy.createInMemory` (ref-map store) in `Scene.fs`
- [x] Ui: `captureScene` / `restoreScene` in `TableAndElementRotationView.fs`
- [x] Tests: `SceneRoundTripTests.fs` (+ fsproj registration)
- [x] Static verification (reads / compile-order / exhaustiveness / field-ambiguity review)

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Scene.fs` — added an intrinsic
  `type SceneProxy with static member createInMemory () : SceneProxy` closing over a
  `ref (Map<SceneName, SceneSnapshot>)` (starts empty). `saveScene` re-validates the name
  through `SceneName.tryCreate` (blank → `InvalidScene`) then upserts (`Map.add`);
  `tryLoadScene` = `Map.tryFind`; `listScenes` = the map keys. Mutation stays inside the
  closure (IO boundary), so logic holding the proxy is pure.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs` — added
  the pure `captureScene (model : Model) : Scene.SceneSnapshot` and
  `restoreScene (snapshot : Scene.SceneSnapshot) (model : Model) : Model` pair after
  `initMain`. Avalonia-free; the element records are qualified (`Scene.SceneElement.id` /
  `TestElement.id`) to disambiguate the shared `{ id; placement; zoom }` shape.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SceneRoundTripTests.fs` — new; the
  acceptance round-trip plus upsert / load-miss / blank-name-rejection cases (+4 cases).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` —
  registered `SceneRoundTripTests.fs` after `SceneProxyTests.fs`.

## Testing state

Gate execution is the arc-runner gate engine's job (IMPLEMENT_CONTRACT Invariant 6 — the
worker acts and runs no checks; the base protocol's "run gates locally" is the legacy
single-worker path, superseded under per-anchor dispatch). Static verification only this
round, per the step 021/022/026 precedent — reads + a careful compile-order, exhaustiveness,
and record-field-ambiguity review. `commit_ready: true`.

New coverage lands in the `constructor-unit-tests` gate (`SceneRoundTripTests.fs`, +4 cases):
- the full acceptance round-trip — `captureScene` a deterministic model → `saveScene` →
  `tryLoadScene` → `restoreScene` onto a DIFFERENT base model (`init ()`) — asserting element
  ids, kinds, placements, bindings (`valueId`), full `TestElement` equality, and view state
  (`TableViewState` + `snapChain`) are reproduced, and the loaded snapshot equals the captured;
- `saveScene` upsert (same name overwrites, one key, latest write wins);
- `tryLoadScene` returns `Ok None` for an unsaved name;
- `saveScene` rejects a directly-constructed blank `SceneName` as `InvalidScene` and stores
  nothing.

`build`, `unit-tests` (BerremanTests), `ui-smoke`, and `ui-tests` are untouched — the Domain /
Ui changes are pure additions (a new type-augmentation member and two pure functions), so no
existing view render or model reducer changes.

## Artifacts

None captured — a source-only round (no logs / screenshots / traces produced).

## Gotchas

- **`createInMemory` stays IN `Scene.fs`** (intrinsic augmentation), unlike `SampleProxy` /
  `MaterialProxy` whose `createInMemory` sits in a later file (`SampleStore.fs` /
  `MaterialStore.fs`) only because they apply `decideVersioning` from `Lifecycle.fs`, which
  compiles after their declaring file. The scene store needs no type compiled after `Scene.fs`,
  so — the `CategoryProxy.createInMemory` precedent (`MaterialLibrary.fs`) — it is an intrinsic
  member. `Scene.fs` already compiles LAST in the Domain fsproj.
- **`saveScene` re-validates the name** even though the field type is `SceneName` (already
  built through `tryCreate`): the `SceneName` union case is public, so a caller CAN construct
  `SceneName ""` outside `tryCreate`. The store re-runs `SceneName.tryCreate name.value` and
  rejects a blank as `InvalidScene` rather than storing an unaddressable key — this is what the
  how-to's "saveScene validates the name (blank yields InvalidScene)" asks for.
- **`SceneName` is a valid `Map` key** — it wraps `string`, so F# auto-derives `comparison`; no
  `[<CustomEquality>]`/`[<ReferenceEquality>]` on it (that attribute is only on `SceneProxy`).
- **Record-field ambiguity** — `TestElement` (Ui) and `Scene.SceneElement` (Domain) share the
  exact field set `{ id; placement; zoom }`, and both are in scope in `captureScene` /
  `restoreScene` and the test. Every element-record literal qualifies its leading label
  (`Scene.SceneElement.id` when building a `SceneElement`, `TestElement.id` when building a
  `TestElement`) so the compiler pins the intended type; receiver field-access (`e.id`) resolves
  via `List.map`'s inferred element type.
- **`restoreScene` resets `selection`/`drag`** (to `TableSelected` / `NotPressed`) in addition
  to replacing `elements` / `view` / `snapChain`: the loaded element set is fresh, so a stale
  `ElementSelected i` index could point past the new list. Every other model field (injected
  proxies, palette, render config, experiment collection, launchers, pending/select session
  state) is left untouched — restore is minimal and pure, touching only what a scene owns plus
  the two fields that directly index the replaced elements. (Chosen per the "don't ask the
  user" rule — the how-to says "applying a snapshot back onto a model"; this is the
  interpretation most consistent with the surrounding index-safety discipline.)
- **The test lives in `OpticalConstructor.Tests`** (per the slice's `touches`), not
  `OpticalConstructor.Ui.Tests`. It constructs `initMain ()` / `init ()`, which are the
  windowless pure model constructors already exercised under plain `[<Fact>]` in the Ui.Tests
  project — no Avalonia session needed — and the Tests project already references the Ui project.
- **The contract-ids registry (`.contract-ids/XDUO-json`) is NOT edited** — its `status: active`
  is the ID-allocation state (arc-runner-owned, `.lock`-guarded), not the declared→implemented
  lifecycle. STORE_XDUO_0002 (SampleProxy) was implemented at step 022 yet still reads `active`
  there, so implement workers leave it alone; the arc-runner's contract-implementation gate
  tracks lifecycle in its own state after the worker exits.
