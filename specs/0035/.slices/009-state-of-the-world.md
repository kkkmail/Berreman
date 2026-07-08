# State of the world — slice 009 (IMPLEMENT — host-wire the category catalogue)

## Where we are

Slice 009 is the IMPLEMENT step that host-wires the category catalogue into the
Main screen. Steps 3/6/8 (its `depends_on`) had already landed the pieces: step 3
the real in-memory `CategoryProxy` (add/rename/remove with the typed
`CategoryStillReferenced` / `BuiltInNotRemovable` blocks), step 6 the
`CategoryEditorWindow` component (declared, unwired), and step 8 the reordered
full-surface Materials / Library workbench bays. This slice connects them: it
threads the `CategoryProxy` into the `TableAndElementRotationView` host model,
gives the Materials bay a **Categories…** verb that opens the editor through the
`EditorLaunchers` seam, and drives both the material-editor create picker and the
Materials-bay category facet off the live `CategoryProxy.listCategories`. It
touches `OpticalConstructor.TestWindows`, `OpticalConstructor.App`, and
`OpticalConstructor.Ui.Tests` — matching the slice `touches`.

## What's working

- Thread the step-3 `CategoryProxy` into the host: `Model.categories`,
  `initWith` / `initMainWith` gain a `categories` argument, `DefaultStores`
  composes it over `materialsReferencingCategory`, and the App root injects it.
- Add a **Categories…** verb to the Materials bay that opens the step-6
  `CategoryEditorWindow` over the live proxy through the `EditorLaunchers` seam.
- Drive the Materials-bay category facet off `CategoryProxy.listCategories`:
  option code = the `CategoryId` Guid string (stable across a rename), label =
  the category's name, re-queried on every render.
- Drive the material-editor create picker off the same live catalogue, excluding
  every `HiddenOnCreate` category (Vacuum) with a stable Guid option id.
- Re-label the facet and the create picker in the same render pass after a
  category add/rename/remove (both read `listCategories` live).
- Add headless proofs (Categories… verb opens the editor; a proxy rename
  re-labels the facet + picker) and pure projection tests.

## Tests

Per the IMPLEMENT worker role (Invariant 6 — the worker acts and runs no checks),
the `build` / `unit-tests` / `constructor-unit-tests` / `ui-smoke` / `ui-tests`
gates are executed by the arc-runner's deterministic gate engine AFTER this
worker exits; they were NOT run as gates here. The counts below are the EXPECTED
post-round baselines (observed locally as due diligence, advisory only).

- NEW `ui-smoke` proofs in `MainWorkbenchTests` (Categories… verb opens the
  `CategoryEditorWindow`; a proxy rename re-labels the Materials facet AND the
  create picker in the same render pass, found by stable Guid ids). `ui_smoke_tests`
  91 → 93.
- NEW pure tests in `MainWorkbenchTests` (Guid facet code round-trips a user
  category; the facet lists the live catalogue with Guid codes / name labels and
  re-labels on rename; the create picker excludes `HiddenOnCreate` and re-labels
  on rename). `ui_tests` 324 → 327.
- `MaterialEditorWindowTests` two direct `MaterialEditorContext` constructions
  gained the required `categories` field (in place; no count change).
- `WireUiCompositionTests` mounts the REAL App window over the now-wired
  `CategoryProxy`; the bay sweep still renders every bay (incl. the new
  Categories… button) without throwing — unchanged count.
- No `berreman_unit_tests` / `constructor_unit_tests` source changed; those
  baselines cannot regress.

## Architecture

- **The category catalogue is DATA behind a functional proxy, threaded like the
  material/sample stores.** `CategoryProxy` joins `MaterialProxy` / `SampleProxy`
  on the host `Model` and in the `DefaultStores` / App composition — samples,
  then materials (remove-block consults live samples), then categories
  (remove-block consults live materials via `materialsReferencingCategory`). The
  bay projections read the proxy on EVERY render, so a write shows in the same
  render pass — the same discipline the materials/samples lists already use.
- **The facet/picker option id is the STABLE `CategoryId` Guid string; only the
  label carries the (renamable) name.** A rename keeps the id and re-labels the
  option's TextBlock in place — FuncUI patches the text, the automation contract
  survives, and a test finds the option by its stable id. A name-derived code
  (the prior facet scheme) would move under a rename and could collide on a shared
  display name. `materialCategoryOfCode` parses the Guid directly, so it
  round-trips a USER category too.
- **The Categories… verb is a HOST-added button, not a `MaterialsControls` verb.**
  `MaterialsControls` (in `OpticalConstructor.Controls`, untouched by this slice)
  stays domain-free and material-row-scoped; catalogue management is a host
  surface (`categoriesRow`), styled like the other host-added workbench buttons —
  the same split the inline confirm / message / View-panel rows already use.
- **`MaterialEditorWindow` takes an OPTIONAL `?categories`** so the Main launcher
  passes the SHARED proxy (renames reflect) while standalone opens default to a
  fresh in-memory catalogue — keeping the 9 unrelated material-editor tests intact
  without churn.

## Deferred

- The App composition ACCEPTANCE (root-wiring the `CategoryProxy` and the
  full ui-smoke suite over the real window) is step-19 WIRE_UI's, per the slice.
  This round only made `Program.fs` build against the threaded `initMainWith`.
- Nothing from this slice's own scope is deferred; the host-wiring, the verb, and
  the live-catalogue picker/facet all land this round. There is no "round 2".

## Gotchas

- **The facet lists the WHOLE catalogue (incl. `HiddenOnCreate` Vacuum); only the
  create picker excludes `HiddenOnCreate`.** The facet filters materials (you can
  filter by Vacuum); the picker is a creation-target list. The pure tests pin both
  behaviours.
- **A rename re-labels in the same render pass only because the projections
  re-query `listCategories` on every render.** The `CategoryEditorWindow` mutates
  the shared store; the Main window reflects it on its NEXT dispatch (any bay
  interaction), which is what "same render pass" means here — the headless proof
  drives a rename through the proxy then a bay dispatch and reads the new label.
- **`materialCategoryOfCode` parses the Guid rather than searching
  `standardCategories`** — required so a user category (not in the seeded set)
  round-trips; the old lookup returned `None` for any non-seeded id.
- **MSB3277 (WindowsBase conflict) is pre-existing** (originates in
  `OpticalConstructor.Ui.fsproj`, per the step-008 SoW); this slice changed no
  project references, so it introduces no new MSB3277 and does not own the fix.
- **The removed `materialCategories` list and private `materialCategoryLabel`**
  had no external references (verified by grep); the View panel's category label
  now resolves LIVE through `liveCategoryName` so it stays consistent with a
  renamed facet.

## Changelog

- 2026-07-07 — Host-wired the step-3 `CategoryProxy` into `TableAndElementRotationView`:
  added `Model.categories`, threaded `categories` through `initWith` /
  `initMainWith` / `DefaultStores` / the App `Program.fs` composition, added the
  Materials-bay **Categories…** verb (`MatOpenCategories` → `openCategoryEditor`
  launcher opening the step-6 `CategoryEditorWindow`), and drove the material-editor
  create picker (minus `HiddenOnCreate`) and the Materials-bay category facet (Guid
  option code / name label) off `CategoryProxy.listCategories`, both re-querying
  live so an add/rename/remove re-labels them in the same render pass.
  `MaterialEditorWindow` gained an optional `?categories` ctor param;
  `MaterialEditorContext` a `categories` field. Added 2 headless + 3 pure tests in
  `MainWorkbenchTests`; updated `MaterialEditorWindowTests`' two context
  constructions.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 440
  ui_smoke_tests: 93
  ui_tests: 327
```
