# Step 002 — impl-plan (ADD_CONTRACT STORE_XDUO_0003 CategoryProxy)

## Goal

Declare the mutating **category write-seam** as a functional proxy — a *declared*
contract only (no real in-memory store; that is the later `IMPLEMENT_CONTRACT`
step). Ship the interface (`CategoryError` + `[<ReferenceEquality>] CategoryProxy`),
the shared blank-name validator (`validateCategory`), a **mock** stub over a fixed
category list, and a **mock-driven test** exercising every field through its exact
signature.

## Approach

Mirror the step-1 catalogue neighbours and the existing `MaterialProxy` /
`SampleProxy` / `LibraryProxy` proxy convention exactly:

- **`CategoryError`** — errors-as-values DU, each case carrying `reason : string`
  (a bare error case is useless in a log): `UnknownCategoryId`,
  `DuplicateCategoryId`, `CategoryStillReferenced`, `BuiltInNotRemovable`,
  `InvalidCategory`. Parallels `MaterialError` (`MaterialLibrary.fs`) and
  `SampleError` (`ElementId.fs`), adding `BuiltInNotRemovable` for the
  refuse-to-remove-a-built-in outcome.
- **`CategoryProxy`** — `[<ReferenceEquality>]` record of camelCase
  `Result`-returning functions: `listCategories : unit -> Result<MaterialCategory list, CategoryError>`,
  `addCategory : MaterialCategory -> Result<unit, CategoryError>`,
  `updateCategory : MaterialCategory -> Result<unit, CategoryError>`,
  `removeCategory : CategoryId -> Result<unit, CategoryError>`. Reference equality
  keeps an Elmish host model that holds one comparable by identity (function-valued
  fields have no structural equality), matching `MaterialProxy` / `SampleProxy`.
- **`validateCategory`** — the `validateEntry` precedent
  (`MaterialLibrary.fs`): a blank/whitespace `name` is `InvalidCategory`; not
  `private`, so the future store augmentation (in another file) can reach it.

Placement: **beside the step-1 category catalogue** in
`OpticalConstructor.Domain/MaterialLibrary.fs`, right after `tryFindCategoryByName`
(the last catalogue helper) and before `MaterialComplexity`. Everything it needs
(`CategoryId`, `MaterialCategory`) is declared above that point.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs` —
  add `CategoryError`, `CategoryProxy`, `validateCategory`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CategoryProxyTests.fs` —
  **new** mock-driven test (stub over a fixed catalogue list, all four functions
  through their exact signatures + reference-equality check).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` —
  register the new test file in compile order (after `SampleProxyTests.fs`).

## Risks

- **Scope discipline (ADD_CONTRACT).** Declare the surface + mock + test only; do
  NOT build the real `createInMemory` store or wire any consumer. Leaving
  neighbouring code untouched is correct for this family.
- **Reference-equality struct.** `[<ReferenceEquality>]` on a record with only
  function fields is required (same as `MaterialProxy`); without it the record has
  no equality and an Elmish model holding it won't compile.
- **Compile order.** The new test file must appear in the `.fsproj` `ItemGroup`
  after the types it uses are available; it only depends on the Domain project, so
  ordering among test files is free — I place it beside `SampleProxyTests.fs`.
