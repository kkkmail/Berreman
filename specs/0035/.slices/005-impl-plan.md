# Impl-plan — slice 005 (ADD_COMPONENT — UICOMP_XDUO_0005 CategoryControls)

## Goal

Add the domain-free `OpticalConstructor.Controls.CategoryControls` FuncUI component (the
Category-manager list surface that spec 0035 A.2 projects the slice-004 `CategoryEditor` onto),
plus its headless structure test in `OpticalConstructor.Ui.Tests`. Author + register only — no
wiring into a parent view (that is the later WIRE_UI step).

## Approach

Copy the shape of the sibling `MaterialsControls` / `LibraryControls` domain-free components:

- `Row` — a pure record (`categoryId : string`, `name : string`, `isBuiltIn : bool`); the host
  flattens each `MaterialCategory` into one (Controls imports no domain types).
- `State` — `{ rows : Row list; blockMessage : string }`; the block-message slot renders only while
  a message is present (the `LibraryControls.confirmPanel` precedent).
- `Handlers` — a record of unit-returning functions injected by the host (the functional-proxy seam;
  tests pass stubs): `addCategory`, `setCategoryName` (curried `id -> name -> unit`), `saveCategory`,
  `removeCategory`, `cancelCategory` (each per-row verb takes the row's category id).
- `UiIds` — a `[<RequireQualifiedAccess>]` module of `[<Literal>]` intent-named ids: `CategoriesList`,
  `AddCategoryButton`, `CategoryNameBox`, `RemoveCategoryButton`, `CategorySaveButton`,
  `CategoryCancelButton`, `CategoryBlockMessage`. The four per-row ids double as the base prefix for
  `rowNameBox` / `rowSaveButton` / `rowRemoveButton` / `rowCancelButton` helpers (the
  `MaterialsControls.row` / `LibraryControls.entry` collision-safe, reuse-safe precedent).
- `view` — an Add button, a NAMED list (`CategoriesList`) of per-row horizontal panels (inline name
  box + Save + Cancel, plus Remove only when `not isBuiltIn`), and the block-message panel. Per-row
  controls carry `AutomationProperties.AutomationId` (reuse-safe; `Name` cannot be re-set on a reused
  control — the MaterialsControls note).

The built-in row OMITS the Remove button entirely (removed, not greyed), matching the
`MaterialsControls` view-only-Edit precedent.

## Test

`CategoryControlsTests.fs` mirrors `MaterialsControlsTests.fs`: a pure contract test (empty state, the
stable UiIds literals + derived ids) plus a `[<Trait("Category","ui-smoke")>]` headless structure
proof — mount `view` over a known State (one user row + one built-in row + a block message), assert
every UiIds control is present (singletons directly; per-row ids for each known row; the built-in
row's Remove ABSENT), and assert a simulated Add click invokes the add handler (and per-row Save /
Remove / Cancel clicks dispatch with the row id).

## Files

- ADD `Berreman/OpticalConstructor/OpticalConstructor.Controls/CategoryControls.fs`
- ADD `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/CategoryControlsTests.fs`
- EDIT `OpticalConstructor.Controls.fsproj` — register `CategoryControls.fs`
- EDIT `OpticalConstructor.Ui.Tests.fsproj` — register `CategoryControlsTests.fs`

## Risks

- Per-row vs single ids: the slice lists the four verb/box ids as literals but also mandates "an
  inline name box per row". Resolved by making the literals the base intent name and deriving the
  actual per-row control id (prefix + category id) — the codebase precedent. Recorded in the
  impl-log Gotchas.
- Reuse safety: per-row list membership changes when the host adds/removes a row, so per-row controls
  use `AutomationId` (mutable, non-unique-OK), not `Name`.
- Family Invariant 6 (act only): this ADD_COMPONENT worker authors + registers and does NOT run the
  build/test gates; the arc-runner gate engine is the sole authority. Author against the known-good
  MaterialsControls idiom to keep the build green.
