# Step 026 — impl-log (ADD_CONTRACT STORE_XDUO_0004 SceneProxy)

## Progress

- **Read** the cross-repo system prompt (`add_contract_worker.system-md` +
  `arc-runner.system-md`), the Berreman project prompt, and the slice spec. Confirmed
  the operator-note section is empty.
- **Surveyed** the seam precedents: `Library.LibraryProxy` (`ElementId.fs`) for the
  functional-proxy shape, `WorkbenchSettings.SelectWindowModality` for the elevated
  two-case bool, the versioned `CategoryProxy`/`MaterialStore` for the mock/stub + test
  style and the `ref Map` idiom, and the live Main-scene model
  (`TableAndElementRotationView`, Ui): `TestElement = { id; placement; zoom }`,
  `view : TableViewState`, `snapChain : bool`.
- **Declared** the seam in `Scene.fs` (Domain): `SceneStoreError`, `SceneName`
  (+`tryCreate`), `SnapMode` (+`value`/`create`), `SceneElement`, `SceneSnapshot`,
  `[<ReferenceEquality>] SceneProxy`. Registered it last in the Domain fsproj.
- **Added** the mock + mock-driven test in `SceneProxyTests.fs` (Tests) and registered
  it after `LifecycleTests.fs`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Scene.fs` — **new**. The scene
  persistence seam (types only; DECLARED lifecycle — no store body).
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
  — registered `Scene.fs` after `SampleStore.fs`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SceneProxyTests.fs` — **new**.
  Inline map-backed mock + the mock-driven test exercising every proxy field.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
  — registered `SceneProxyTests.fs` after `LifecycleTests.fs`.

## Testing state

`commit_ready: true`. Per the ADD_CONTRACT **Invariant 6 — act only** (and the step
024/025 precedent in this arc), the worker authors the contract, mock, and test and runs
NO gates; gate execution belongs to the arc-runner's deterministic gate engine. Static
verification only this round:

- Compile order verified — `Scene.fs` compiles after every type it names (`ElementId.fs`
  → `Library.ElementId`, `Placement.fs` → `ElementPlacement`/`CatalogueKind`, `Table.fs`
  → `TableViewState`); sibling-module qualified references need no namespace open (the
  `Project.fs` precedent).
- Every `match` is exhaustive (no FS0025), the elevated types carry `.value`/`tryCreate`,
  the error carries a `reason`, and the proxy is `[<ReferenceEquality>]`.
- The mock-driven test drives `saveScene`, `tryLoadScene` (hit + miss), and `listScenes`
  through their exact signatures — the acceptance. `ref Map` mutation matches the
  `MaterialStore` idiom; fixture records are `: SceneElement`-annotated to remove
  record-inference ambiguity on the common `id` field name.

Expected gate roster (run by the engine): `build`, `unit-tests`,
`constructor-unit-tests`, `ui-smoke`, `ui-tests`. Only `Scene.fs` (Domain) and
`SceneProxyTests.fs` (Tests) change; `constructor-unit-tests` gains 7 cases, the other
counts are untouched.

## Artifacts

None produced this round (a pure contract-declaration slice — no captured logs,
screenshots, or traces). The per-arc artifacts folder is
`C:\GitHub\Berreman\specs\0038\.artifacts`.

## Gotchas

- **"CatalogueKind" and "valueId binding" are `placement` fields.** The how-to
  enumerates them per element, but `ElementPlacement` already carries
  `catalogueKind` and `valueId`; the snapshot reuses `ElementPlacement` (as
  `Project.placements` already does) rather than duplicating derivable fields.
  `SceneElement` therefore mirrors the live `TestElement` shape (`{ id; placement; zoom }`).
- **The snap flag is elevated, not a bool.** `SnapMode` is a two-case DU per the
  elevate-every-primitive rule; the host model's `snapChain : bool` maps through
  `SnapMode.value`/`create` at the boundary.
- **`zoom` stays a bare `float`.** No `Zoom` DU exists in the repo and the sibling
  display scalars (`TableViewState.zoom`, `TestElement.zoom`, `Vector3` components) are
  all bare floats; introducing one here alone would be inconsistent and out of scope.
- **Snapshot scope is exactly elements + view state + snap flag.** The table PLATE
  (`OpticalTable`) and other project aggregates are deliberately NOT captured — the
  how-to enumerates only those three.
- **DECLARED, not implemented.** No disk format and no `.ocproj` wiring — the real
  file-backed `create` is a later `IMPLEMENT_CONTRACT STORE_XDUO_0004` in
  `OpticalConstructor.Storage`.
