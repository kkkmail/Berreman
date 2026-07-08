# 009 IMPLEMENT — impl-log

## Progress

- [x] Thread `CategoryProxy` into the host model (Model / initWith / initMainWith
      / init / initMain / DefaultStores).
- [x] `EditorLaunchers`: `openCategoryEditor` + categories on `openMaterialEditor`.
- [x] `MatOpenCategories` Msg + update arm; MatAdd/MatEdit pass categories.
- [x] Facet driven off `listCategories` (Guid code, name label); `materialsState`
      re-queries every render; view-panel label resolved live.
- [x] Categories… host button in the Materials bay (`WorkbenchIds.categoriesButton`).
- [x] Material-editor create picker driven off the proxy (excludes HiddenOnCreate,
      stable Guid option code).
- [x] Program.fs mechanical composition update (build + inject CategoryProxy).
- [x] Tests: 2 headless (Categories verb opens editor; rename re-labels picker +
      facet) + 3 pure projections.

## Files modified

- `OpticalConstructor.TestWindows/TableAndElementRotationView.fs`
  - `EditorLaunchers`: added `openCategoryEditor : CategoryProxy -> unit`; added a
    `CategoryProxy` argument to `openMaterialEditor`; `defaults` open the real
    `CategoryEditorWindow` / pass categories to `MaterialEditorWindow`.
  - `Model`: added `categories : MaterialLibrary.CategoryProxy`.
  - `DefaultStores.create`: now returns the triple, composing the `CategoryProxy`
    over `materialsReferencingCategory` of the just-built materials store.
  - `initWith` / `initMainWith` gained a `categories` param; `init` / `initMain`
    destructure the triple; the record sets `categories`.
  - New `Msg MatOpenCategories` + `update` arm (pure launch of the editor);
    `MatAdd` / `MatEdit` pass `model.categories`.
  - Facet: `materialCategoryCode` now the `CategoryId` Guid string;
    `materialCategoryOfCode` parses the Guid directly (round-trips user
    categories too); `materialsState.categoryOptions` built from
    `model.categories.listCategories ()` (leading "all" + Guid code / name
    label); removed the now-unused `materialCategories` list and the private
    `materialCategoryLabel`. Added `liveCategoryName` so the View panel's label
    resolves from the live catalogue; wired it into `materialViewPanel`.
  - `WorkbenchIds.categoriesButton` literal + `categoriesRow` host button in
    `materialsBay`.
- `OpticalConstructor.TestWindows/MaterialEditorView.fs`
  - `MaterialEditorContext`: added `categories : CategoryProxy`.
  - Added public `selectableCategories (m)` (live catalogue minus HiddenOnCreate);
    `categoryRow` drives off it, option id = the `CategoryId` Guid string.
- `OpticalConstructor.TestWindows/MaterialEditorWindow.fs`
  - Added optional `?categories` ctor param (defaults to a fresh in-memory
    catalogue) so the 9 existing material-editor headless tests stay untouched;
    the context now carries `categories`.
- `OpticalConstructor.App/Program.fs`
  - Build `CategoryProxy.createInMemory (materialsReferencingCategory materials)`
    and inject through `initMainWith` (mechanical; step-19 owns the acceptance).
- `OpticalConstructor.Ui.Tests/MainWorkbenchTests.fs`
  - `mainWith` composes + injects the categories proxy (exposed as `.categories`);
    `recordingLaunchers` and the real-launcher test gained `openCategoryEditor`
    and the new `openMaterialEditor` arity; added `labelInside` helper; added 3
    pure + 2 headless category tests.
- `OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`
  - Added the required `categories` field to the two direct `MaterialEditorContext`
    constructions (window-ctor sites unchanged via the optional param).

## Testing state

Per the IMPLEMENT worker role (Invariant 6 — the worker acts, runs no checks):
the `build` / `unit-tests` / `constructor-unit-tests` / `ui-smoke` / `ui-tests`
gates are run by the arc-runner's deterministic gate engine AFTER this worker
exits. As due-diligence (deliver working behaviour with its tests) I compiled the
solution and ran the affected Ui.Tests modules locally; observations below are
advisory, not a gate verdict.

- `dotnet build Berreman.slnx -c Release`: **0 errors**. No FS/MSB warning
  originates in any file this slice changed (verified by grep). Pre-existing
  MSB3277 (WindowsBase) is unchanged — no project references were touched.
- Local Ui.Tests: `ui-smoke` **93** (baseline 91, +2 headless), `ui-tests`
  **327** (baseline 324, +3 pure) — all green, no hang.
- `berreman_unit_tests` (119) / `constructor_unit_tests` (440) untouched — no
  Domain / Berreman / OpticalConstructor.Tests source changed; those baselines
  cannot regress.
- No CRLF churn (`git diff --numstat` == `--ignore-cr-at-eol`).

## Artifacts

None (pure code + headless tests; nothing captured to `.artifacts/`).

## Gotchas

- **The facet/picker option AutomationId is the STABLE `CategoryId` Guid string,
  not the name.** A rename keeps the id and only re-labels the option's
  TextBlock — FuncUI patches it in place, and a test finds the option by its
  stable id and reads the new label. A name-derived code would move under a
  rename (breaking the "find by stable id" contract) and could collide if two
  categories shared a display name.
- **The facet lists the WHOLE catalogue (incl. `HiddenOnCreate` Vacuum); only the
  create picker excludes `HiddenOnCreate`.** The facet is a filter — you can
  filter materials by Vacuum — while the picker is a creation target list.
- **`materialCategoryOfCode` parses the Guid directly** rather than searching
  `standardCategories`, so it round-trips a USER category too (the old
  name-in-catalogue lookup returned `None` for any non-seeded category).
- **`MaterialEditorWindow` gained an OPTIONAL `?categories` ctor param** (default:
  a fresh in-memory catalogue) rather than a required one, deliberately, so the 9
  unrelated material-editor headless tests (which construct
  `MaterialEditorWindow(materials, None)`) did not have to change. The Main-screen
  launcher always passes the SHARED proxy, so a rename in the Category editor
  re-labels the picker on the editor's next open.
- **Step-19 WIRE_UI owns the App composition acceptance.** This round only made
  `Program.fs` build against the threaded-`categories` `initMainWith`; the
  root-wiring acceptance test is step 19's, not here.
- **MSB3277 (WindowsBase conflict) is pre-existing** (originates in
  `OpticalConstructor.Ui.fsproj`, per the step-008 SoW). This slice changed no
  project references, so it introduces no new MSB3277 and does not own the fix.

## Changelog

- 2026-07-07 — Host-wired the step-3 `CategoryProxy` into the Main host: threaded
  it through `Model` / `initWith` / `initMainWith` / `DefaultStores` and the App
  composition; added the Materials-bay **Categories…** verb opening the step-6
  `CategoryEditorWindow` through the `EditorLaunchers` seam; drove the
  material-editor create picker (minus `HiddenOnCreate`) and the Materials-bay
  category facet (Guid code / name label) off `CategoryProxy.listCategories`, both
  re-querying live so an add/rename/remove re-labels them in the same render pass.
  Added 2 headless + 3 pure tests.
