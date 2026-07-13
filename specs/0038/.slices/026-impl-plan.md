# Step 026 — impl-plan (ADD_CONTRACT STORE_XDUO_0004 SceneProxy)

## Goal

Declare, in `OpticalConstructor.Domain`, the DECLARED-lifecycle **scene persistence
seam** — the `SceneProxy` functional proxy plus its elevated key, snapshot, and error
types — and supply a mock + mock-driven test. No disk format, no `.ocproj` wiring: this
is the seam a future storage cycle (`IMPLEMENT_CONTRACT`) swaps for a real, file-backed
store in `OpticalConstructor.Storage`.

## Surface to declare (Domain)

New file `Scene.fs` (module `Scene`), compiled last in the Domain fsproj (after
`SampleStore.fs`). It reuses sibling Domain types by qualified name (the `Project.fs`
precedent — no namespace open needed):

- `SceneStoreError = InvalidScene of reason : string` (declared first so `tryCreate`
  can name it).
- `SceneName` — single-case string DU; `.value` accessor; `static member tryCreate`
  rejecting a blank/whitespace/null name → `Error (InvalidScene …)`.
- `SnapMode` — a **two-case DU** (`SnapToBeam | FreePlacement`) for the snap flag (the
  elevate-every-primitive rule forbids a naked bool); `.value`/`create` map the host
  model's `snapChain : bool` at the IO boundary (the `SelectWindowModality` precedent).
- `SceneElement = { id : Library.ElementId; placement : Placement.ElementPlacement; zoom : float }`
  — mirrors the live Main-scene `TestElement` (Ui) over Domain types. `placement`
  already carries the `catalogueKind` tag and the `valueId` Library binding, so the
  snapshot captures id + CatalogueKind + placement + valueId through ONE reused type.
- `SceneSnapshot = { elements : SceneElement list; view : Table.TableViewState; snap : SnapMode }`.
- `[<ReferenceEquality>] SceneProxy = { saveScene; tryLoadScene; listScenes }` with the
  exact signatures the how-to pins.

## Mock + test (Tests)

New file `SceneProxyTests.fs` (module `SceneProxyTests`), registered after
`LifecycleTests.fs`:

- An inline stub `SceneProxy` over a `ref (Map<string, SceneSnapshot>)` (the
  `MaterialStore` idiom) so `saveScene` genuinely inserts and the round-trip is real.
- A mock-driven test exercising `saveScene` / `tryLoadScene` (hit + miss) / `listScenes`
  through their exact signatures — the acceptance.
- Supporting tests: `SceneName.tryCreate` blank/null rejection + happy round-trip;
  `SnapMode` wire-form round-trip; the proxy's reference equality (the `CategoryProxy`
  precedent).

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Scene.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SceneProxyTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- **Record-inference ambiguity** on the common field name `id` — mitigated by explicit
  `: SceneElement` annotations on the fixture values.
- **Case/type name collision** (`SceneName`) — `tryCreate` is a static member with no
  colliding case, so `SceneName.tryCreate` resolves cleanly (the `QuickPickThreshold`
  precedent); the test routes name construction through `tryCreate`.
- **Snapshot equality** — every `SceneSnapshot` field is a pure data type (no function
  fields), so structural equality holds and `Assert.Equal<SceneSnapshot>` is valid.

## Interpretation recorded

The how-to lists "id, CatalogueKind, placement, zoom, and valueId binding" per element;
CatalogueKind and the valueId binding are the fields `ElementPlacement` already carries
(`placement.catalogueKind` / `placement.valueId`), so the snapshot reuses
`ElementPlacement` rather than duplicating derivable fields (the reuse discipline). The
table PLATE and other project aggregates stay outside the snapshot — the how-to
enumerates only elements + view state + snap flag.
