# Step 001 — impl-plan

## Goal

Replace the closed `MaterialCategory` union (`OpticalConstructor.Domain/MaterialLibrary.fs:67`)
with DATA: a `CategoryId` identity DU plus a `MaterialCategory` record seeded from a
catalogue (`standardCategories`). No domain record or public signature may carry the closed
union; a category name resolves through the seeded catalogue by `CategoryId`; the create
picker offers only `SelectableOnCreate` categories (Vacuum removed).

## Approach

1. **MaterialLibrary.fs (Domain)** — the load-bearing change.
   - Add `CategoryId` (Guid-backed single-case DU, `.value`, `static create : unit -> CategoryId`),
     mirroring `MaterialId`.
   - Add `CategoryVisibility = SelectableOnCreate | HiddenOnCreate` and
     `CategoryOrigin = BuiltInCategory | UserCategory` (named two-case DUs, never bool).
   - Replace the union with `MaterialCategory = { id; name; visibility; origin }`.
   - Add `CategoryIds` module of FIXED literal Guids (mirrors `MaterialIds`) and
     `standardCategories`: Glass/Metal/Semiconductor/Crystal = BuiltIn/Selectable, Vacuum =
     BuiltIn/Hidden.
   - Add name-resolution seams: `tryFindCategory`, `categoryName`, `tryFindCategoryByName`.
   - Re-type `MaterialEntry.category : CategoryId`, `MaterialQuery.category : CategoryId option`.
   - Re-seed every `builtInEntries` category to the matching `CategoryIds` value.
   - Re-express `byCategory` (CategoryId filter); `byQuery` is unchanged (delegates to byCategory).

2. **Consumers** (kept building — whole-solution build is the gate):
   - `MaterialImport.fs`: `entryFromTabulated` takes `CategoryId`; formula/CSV imports seed
     `CategoryIds.glass` / `CategoryIds.semiconductor`.
   - `Report.fs`: DTO `category : string` (the display name); export writes `categoryName id`
     (still `"Glass"/…`, satisfying the schema enum); import maps the name back via
     `tryFindCategoryByName` (unknown name → typed `JsonParseError`).
   - `MaterialsView.fs` (Ui): `Filter.category`/`SetCategory`/`categoryButton` become
     `CategoryId option`; the filter buttons are built from the `SelectableOnCreate` catalogue.
   - `MaterialEditorView.fs` (TestWindows): `Model.category`/`ChooseCategory` become `CategoryId`;
     `categoryRow` offers `standardCategories` filtered to `SelectableOnCreate` (Vacuum gone);
     `categoryCode` resolves `categoryName`.
   - `TableAndElementRotationView.fs` (TestWindows): `materialCategories : CategoryId list` from
     the catalogue; `materialCategoryCode`/`Label`/`OfCode` resolve names through it; `MatSelectCategory`
     carries `CategoryId option`; facet + entry display unchanged in shape.

3. **Tests**: `MaterialProxyTests`, `DispersionModelsTests`, `MainWorkbenchTests`,
   `MaterialsPanelTests` swap union case values for `CategoryIds.*` and the
   `MaterialCategory option` annotation for `CategoryId option`.

## Files to modify

- Domain: `MaterialLibrary.fs`
- Storage: `MaterialImport.fs`, `Report.fs`
- Ui: `MaterialsView.fs`
- TestWindows: `MaterialEditorView.fs`, `TableAndElementRotationView.fs`
- Tests: `MaterialProxyTests.fs`, `DispersionModelsTests.fs`, `MainWorkbenchTests.fs`,
  `MaterialsPanelTests.fs`

## Risks

- **Schema round-trip.** `exportMaterials` must still write the category as one of
  `["Glass","Metal","Semiconductor","Crystal","Vacuum"]` (schema enum). Resolving `categoryName`
  through the built-in catalogue preserves this exactly (ExportImportTests pins it).
- **Scope beyond the `touches` list.** The slice `touches` names Domain/TestWindows/Tests, but
  `MaterialCategory` is also referenced by `.Ui` (MaterialsView) and `.Storage` (MaterialImport,
  Report). A green whole-solution build forces updating them too; recorded in Gotchas.
- **LF line endings** must be preserved.
