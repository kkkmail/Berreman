# Impl-plan — slice 004 (IMPLEMENT — CategoryEditor)

## Goal

Add a pure, Avalonia-free Domain edit model `CategoryEditor` over the seeded
`MaterialCategory` catalogue, mirroring the message-DU discipline of
`MaterialComplexityEditor` (state record + one message DU + a `Result`-returning
apply), plus a commit projection that produces the add / rename / remove intents
the host dispatches to `CategoryProxy` (step 2/3). Unit-test every message arm
without a window.

## Approach

New file `OpticalConstructor.Domain/CategoryEditor.fs` (top-level
`module OpticalConstructor.Domain.CategoryEditor`, opens `MaterialLibrary`),
compiled after `MaterialComplexityEditor.fs` (only depends on `MaterialLibrary`
— `CategoryId` / `MaterialCategory` / `CategoryVisibility` / `CategoryOrigin` /
`CategoryError` / `validateCategory` / `standardCategories`).

Shapes:

- `CategoryEditState = { original : MaterialCategory list; rows : MaterialCategory list; editingRow : CategoryId option }`
  — `rows` is the current working list, `original` the catalogue snapshot the
  editor opened over (the commit diff baseline), `editingRow` the in-progress
  inline edit (the freshly Begin-added, not-yet-named row).
- `CategoryMsg = BeginAddCategory | SetCategoryName of CategoryId * string | RemoveCategoryRow of CategoryId`.
- `CategoryCommit = AddCategory of MaterialCategory | RenameCategory of MaterialCategory | RemoveCategory of CategoryId`
  — one case per `CategoryProxy` write verb (addCategory / updateCategory /
  removeCategory).
- `init : MaterialCategory list -> CategoryEditState`.
- `applyCategoryMsg : CategoryMsg -> CategoryEditState -> Result<CategoryEditState, CategoryError>`:
  BeginAddCategory mints `CategoryId.create ()` and appends a blank
  `UserCategory` / `SelectableOnCreate` row (marks it `editingRow`);
  SetCategoryName/RemoveCategoryRow target a row by id, reusing the typed
  `UnknownCategoryId` when the id is absent (the `NoSuchSegment` precedent).
- `commit : CategoryEditState -> Result<CategoryCommit list, CategoryError>`:
  diff `rows` vs `original` → adds (new ids) / renames (changed rows) / removes
  (dropped ids); each add/rename validated via `validateCategory` so a blank
  name surfaces the typed `InvalidCategory`.

Blank-name rejection lives at `commit`, not at each keystroke, so a name box may
be transiently cleared during editing — matching how `validateCategory` guards
the store's write functions in step 2/3.

## Files

- ADD `Berreman/OpticalConstructor/OpticalConstructor.Domain/CategoryEditor.fs`
- EDIT `.../OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (register)
- ADD `Berreman/OpticalConstructor/OpticalConstructor.Tests/CategoryEditorTests.fs`
- EDIT `.../OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (register)

## Risks

- BeginAddCategory mints a Guid (non-deterministic) — the ONE impure arm, as the
  slice requires; tests discover the minted id via `editingRow`, never a literal.
- LF line endings + zero new warnings; `--warnaserror+:25`.
