# Step 003 — impl-plan (IMPLEMENT_CONTRACT STORE_XDUO_0003 CategoryProxy)

## Goal

Bring the `CategoryProxy` contract (declared in step 002) to **implemented**
lifecycle: a real, stateful in-memory category store behind the write-seam,
replacing the step-002 validate-only mock. No hollow shell — genuine mutation,
the duplicate/unknown/built-in/referenced guards, and a live referencing lookup.

## Approach

Mirror the `SampleProxy.createInMemory` / `MaterialProxy.createInMemory`
precedent (`ElementId.fs:616,686`):

1. **`CategoryProxy.createInMemory (materialsReferencingCategory : CategoryId -> MaterialEntry list)`**
   — a type augmentation appended at the end of the `MaterialLibrary` module
   (after `validateEntry`). It closes over a `ref (Map<CategoryId, MaterialCategory>)`
   seeded from `standardCategories` (the elevated `CategoryId` is the Map key
   directly). Mutation stays INSIDE the closure (the IO boundary).
   - `listCategories` — answers from the current map.
   - `addCategory` — `validateCategory` (blank-name → `InvalidCategory`), then
     hard-blocks a duplicate id (`DuplicateCategoryId`), else persists.
   - `updateCategory` — validate, then replace a known id, reject an unknown one
     (`UnknownCategoryId`). Built-ins ARE renamable (no origin guard on update).
   - `removeCategory` — refuse a `BuiltInCategory` outright (`BuiltInNotRemovable`);
     for a `UserCategory`, consult `materialsReferencingCategory` and return
     `CategoryStillReferenced` NAMING the referencing materials whenever any
     remain (never cascades, never silently deletes), else remove it.

   Placement note: unlike `MaterialProxy.createInMemory` (whose `Sample`-typed
   lookup forced it into `ElementId.fs`), the category referencing lookup is
   `MaterialEntry`-typed and `MaterialEntry` compiles above in this same file, so
   this augmentation is an INTRINSIC one that stays in `MaterialLibrary.fs`.

2. **`materialsReferencingCategory (materials : MaterialProxy) (id : CategoryId) : MaterialEntry list`**
   — a module-level composition-root lookup over `MaterialProxy.listMaterials`
   (the `samplesReferencing` precedent, `ElementId.fs:740`), filtering entries
   whose `category` equals the id. Total `listMaterials` (always `Ok`) → no error
   channel; an `Error` degrades to `[]`.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs`
  — append the `CategoryProxy.createInMemory` augmentation + `materialsReferencingCategory`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CategoryProxyTests.fs`
  — append real-store tests (the step-002 stub tests stay verbatim), mirroring
  the `MaterialProxyTests.fs` real-store section.

## Tests to add (fixed Guids for determinism)

- add/rename/remove round-trip of an unreferenced user category → `Ok`, list reflects each step.
- duplicate-add rejection (a seeded id + a re-added user id) → `DuplicateCategoryId`.
- built-ins ARE renamable → `updateCategory` on a seeded built-in → `Ok`.
- `updateCategory` / `removeCategory` on an unknown id → `UnknownCategoryId`.
- built-in-not-removable block → `BuiltInNotRemovable`, store unchanged (acceptance).
- referenced-category hard block via the composed `MaterialProxy` + `materialsReferencingCategory`
  → `CategoryStillReferenced` naming the material, store unchanged (acceptance).
- live referencing lookup — remove the referencing material, the category becomes removable.

## Risks

- Intrinsic augmentation on `[<ReferenceEquality>]` `CategoryProxy` with intervening
  declarations — proven safe by the `SampleProxy` precedent (same file/module).
- The built-in guard shadows `CategoryStillReferenced` for every seeded category
  (all `BuiltInCategory`), so the referenced-block test MUST use a `UserCategory`.
- LF line endings; zero new warnings from changed files.
