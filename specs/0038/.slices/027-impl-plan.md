# Step 027 — impl-plan (IMPLEMENT_CONTRACT STORE_XDUO_0004 SceneProxy)

## Goal

Move the DECLARED scene-persistence seam (step 026) to `implemented`: a real
in-memory `SceneProxy.createInMemory`, plus the pure `captureScene` / `restoreScene`
pair in the workbench host, verified by a capture→save→load→restore round-trip test.

## Approach

1. **Domain — `Scene.fs`**: add an intrinsic `SceneProxy.createInMemory` type
   augmentation (the `CategoryProxy.createInMemory` precedent — no type compiles
   after `Scene.fs`, so it stays in-file, mirroring the `SampleProxy.createInMemory`
   `ref`-map shape from `SampleStore.fs`). Closes over a
   `ref (Map<SceneName, SceneSnapshot>)`, starts empty:
   - `saveScene` re-validates the name through `SceneName.tryCreate` (a directly-
     constructed blank `SceneName` yields `InvalidScene`, never a silent store) and
     upserts;
   - `tryLoadScene` reads the map (`Map.tryFind`);
   - `listScenes` returns the keys.

2. **Ui — `TableAndElementRotationView.fs`** (the workbench host): add the pure,
   Avalonia-free pair after `initMain`:
   - `captureScene (model : Model) : Scene.SceneSnapshot` — project the live
     `elements` (id + placement, which already carries CatalogueKind + `valueId`
     binding, + zoom), the `view` state, and the `snapChain` flag elevated to
     `SnapMode`.
   - `restoreScene (snapshot : Scene.SceneSnapshot) (model : Model) : Model` — the
     inverse: replace `elements` / `view` / `snapChain`, reset `selection` to the
     table and `drag` to `NotPressed` (the loaded element set invalidates any stale
     index), leave every injected proxy / palette / render field untouched.
   Field labels are qualified (`Scene.SceneElement.id` / `TestElement.id`) because
   the two element records share `{ id; placement; zoom }`.

3. **Tests — `OpticalConstructor.Tests`** (new `SceneRoundTripTests.fs`, registered
   in the fsproj): deterministic model (seeded src/pol/det elements with distinct
   kinds, placements, bindings, zooms + a non-default view). Assert
   capture→`saveScene`→`tryLoadScene`→restore onto a *different* base model
   reproduces element ids, kinds, placements, bindings, and view state; plus the
   loaded snapshot equals the captured one and `listScenes` lists the saved name.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Scene.fs` (augment)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs` (add pair)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SceneRoundTripTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (register)

## Risks

- Record-field ambiguity between `SceneElement` and `TestElement` (both `{ id;
  placement; zoom }`) — mitigated by qualified leading labels.
- `SceneName` as a `Map` key needs `comparison` — auto-derived (wraps `string`).
- Gate roster: build, unit-tests, constructor-unit-tests, ui-smoke, ui-tests. The
  new test lands in `constructor-unit-tests`; ui gates unaffected (pure adds).
