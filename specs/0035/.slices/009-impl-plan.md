# 009 IMPLEMENT — impl-plan

## Goal (from 009.slice-md)

Host-wire the step-3 `CategoryProxy` into the Main-screen host
(`TableAndElementRotationView`), add a **Categories…** verb to the Materials bay
that opens the step-6 `CategoryEditorWindow` through the `EditorLaunchers` seam,
and drive **both** the material-editor create picker and the Materials-bay
category facet off `CategoryProxy.listCategories` (picker excludes
`HiddenOnCreate`; facet uses option code = `CategoryId` Guid string, label =
name). Every category add/rename/remove re-queries the proxy so the picker and
facet refresh in the same render pass.

Step-19 WIRE_UI owns the App composition *acceptance*; this round only updates
`OpticalConstructor.App/Program.fs` **mechanically** so the solution builds.

## Files to modify

1. `OpticalConstructor.TestWindows/TableAndElementRotationView.fs`
   - `EditorLaunchers`: add `openCategoryEditor : CategoryProxy -> unit`; thread
     `CategoryProxy` into `openMaterialEditor` (so the editor's create picker can
     read it); update `defaults`.
   - `Model`: add `categories : MaterialLibrary.CategoryProxy`.
   - `DefaultStores.create`: also build the `CategoryProxy` (its
     `materialsReferencingCategory` lookup over the just-built materials store);
     return the triple.
   - `initWith` / `initMainWith` / `init` / `initMain`: thread `categories`
     beside the material/sample proxies.
   - New `Msg` `MatOpenCategories` + `update` arm that calls
     `openCategoryEditor model.categories`.
   - `MatAdd` / `MatEdit`: pass `model.categories` to the launcher.
   - Facet projection (`materialsState.categoryOptions` / `selectedCategory`,
     `materialCategoryCode` / `materialCategoryOfCode`): drive off
     `model.categories.listCategories ()`, code = Guid string, label = record
     name (so renames re-label live).
   - `materialsBay`: add the **Categories…** host button (stable UiId in
     `WorkbenchIds`).

2. `OpticalConstructor.TestWindows/MaterialEditorView.fs`
   - `MaterialEditorContext`: add `categories : CategoryProxy`.
   - Add public `selectableCategories (m)` projection reading the proxy filtered
     to `<> HiddenOnCreate`; drive `categoryRow` off it, option code = Guid
     string (stable across rename), label = name.

3. `OpticalConstructor.TestWindows/MaterialEditorWindow.fs`
   - Add optional `?categories` ctor param (default: a fresh in-memory
     catalogue) so the 9 existing material-editor tests stay untouched; fill the
     context's `categories` field.

4. `OpticalConstructor.App/Program.fs`
   - Build `CategoryProxy.createInMemory (materialsReferencingCategory materials)`
     and pass it to `initMainWith` (mechanical; acceptance is step 19's).

5. `OpticalConstructor.Ui.Tests/MainWorkbenchTests.fs`
   - Thread the categories proxy through `mainWith`; extend `recordingLaunchers`
     and the real-launcher test with `openCategoryEditor` + the new
     `openMaterialEditor` arity.
   - NEW headless: the Categories… verb opens the `CategoryEditorWindow`.
   - NEW headless: a proxy rename re-labels the Materials facet **and** the
     create picker in the same render pass.
   - NEW pure: facet projection uses Guid codes / name labels and re-labels on a
     proxy rename; create-picker projection excludes `HiddenOnCreate` and
     reflects a rename.

6. `OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`
   - Add the required `categories` field to the two direct
     `MaterialEditorContext` constructions (window ctor sites unchanged via the
     optional param).

## Risks

- Signature churn: `openMaterialEditor` / `initMainWith` / `DefaultStores` /
  `MaterialEditorContext` gain a parameter/field — every call site in touched
  projects must be updated or the build breaks. Mitigated by the optional
  `?categories` ctor param that keeps the 9 unrelated material-editor headless
  tests intact.
- Facet code scheme changes lowercased-name → Guid string; the existing
  round-trip test (`materialCategoryOfCode (materialCategoryCode …)`) must still
  pass — kept green by making `materialCategoryOfCode` parse the Guid directly.
- FuncUI label re-render: the facet/picker option AutomationId is the STABLE
  Guid; only the label TextBlock text changes on rename, which FuncUI patches in
  place.
