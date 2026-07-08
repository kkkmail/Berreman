# Step 003 — impl-log (IMPLEMENT_CONTRACT STORE_XDUO_0003 CategoryProxy)

## Progress

- [done] Implemented `CategoryProxy.createInMemory (materialsReferencingCategory)` as an INTRINSIC
  type augmentation at the end of the `MaterialLibrary` module in `MaterialLibrary.fs` (after
  `validateEntry`) — the real, stateful in-memory category store behind the step-002 write-seam,
  replacing the validate-only mock. Closes over a `ref (Map<CategoryId, MaterialCategory>)` seeded
  from `standardCategories` (the elevated `CategoryId` is the Map key directly). Mutation stays
  inside the closure (the IO boundary):
  - `listCategories` — from the current map.
  - `addCategory` — `validateCategory` (blank → `InvalidCategory`), then hard-blocks a duplicate id
    (`DuplicateCategoryId`), else persists.
  - `updateCategory` — validate, then replace a known id, reject an unknown one
    (`UnknownCategoryId`). No origin guard — built-ins ARE renamable.
  - `removeCategory` — refuse a `BuiltInCategory` outright (`BuiltInNotRemovable`); for a
    `UserCategory`, consult `materialsReferencingCategory` and return `CategoryStillReferenced`
    NAMING the referencing materials whenever any remain (never cascades, never silently deletes),
    else remove.
- [done] Added the module-level `materialsReferencingCategory (materials : MaterialProxy) (id : CategoryId) : MaterialEntry list`
  composition-root lookup over `MaterialProxy.listMaterials` (the `samplesReferencing` precedent),
  filtering entries whose `category` equals the id; total listing → no error channel, `Error` → `[]`.
- [done] Appended real-store tests to `CategoryProxyTests.fs` (the step-002 stub tests kept
  verbatim), plus `open OpticalConstructor.Domain.Library` for the composed `MaterialProxy`.
- [done] Built the whole solution (0 errors) and ran the new test file (11 passed).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs` — the real store
  (`CategoryProxy.createInMemory`) + the `materialsReferencingCategory` lookup, +82 lines,
  0 deletions.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CategoryProxyTests.fs` — 8 new real-store
  tests + the module doc-comment update + the `Library` open, +192 / −7 lines.

## Testing state

- **Build (minimum diligence, not a self-reported gate):**
  `dotnet build Berreman.slnx -c Release -nologo -v:m` → **0 errors, 94 warnings**. Every warning
  is pre-existing and outside this slice: `MSB3277` (WindowsBase 4.0.0.0 vs 5.0.0.0 in the
  test/app/ui-test projects — a reference-graph conflict predating this slice, out of an
  IMPLEMENT_CONTRACT slice's scope) and `FS1125` in the untouched `SeriesDataTests.fs`. No warning
  originates in `MaterialLibrary.fs` or `CategoryProxyTests.fs`.
- **New tests executed (TDD diligence):**
  `dotnet test OpticalConstructor.Tests.fsproj --no-build -c Release --filter "FullyQualifiedName~CategoryProxyTests"`
  → **11 passed, 0 failed** (3 kept step-002 stub tests + 8 new real-store tests).
- **LF verified:** `git diff --numstat` equals `git diff --numstat --ignore-cr-at-eol` for both
  changed files — no CRLF churn.
- **Gates:** Per the IMPLEMENT_CONTRACT worker's Invariant 6 ("you act; you run no checks — gate
  execution belongs to the arc-runner's deterministic gate engine, which runs the step's gates
  after you exit"), I did NOT run the `build` / `unit-tests` / `constructor-unit-tests` /
  `ui-smoke` / `ui-tests` roster as gates and do not self-report their results. The build
  compile-check and the new-test run above are the minimum diligence to avoid shipping a
  non-building contract or a test that does not pass; the gate engine is the sole authority. This
  slice only ADDS tests (never removes one), so no `count_at_least` baseline can regress.

## Gotchas

- **Placement: INTRINSIC augmentation, stays in `MaterialLibrary.fs`.** Unlike
  `MaterialProxy.createInMemory` (which lives in `ElementId.fs` because its `samplesReferencing`
  lookup is `Sample`-typed and `Sample` compiles after `MaterialLibrary.fs`), the category
  referencing lookup is `MaterialEntry`-typed and `MaterialEntry` compiles above in this same file.
  So the augmentation and the `materialsReferencingCategory` lookup both sit at the end of the
  `MaterialLibrary` module — no forward-declared type is needed. The intrinsic augmentation on the
  `[<ReferenceEquality>]` `CategoryProxy` with intervening declarations mirrors the proven
  `SampleProxy.createInMemory` precedent (same file/module).
- **The built-in guard shadows `CategoryStillReferenced` for every SEEDED category.** All five
  seeded catalogue categories are `BuiltInCategory`, so `removeCategory` on any of them returns
  `BuiltInNotRemovable` before it could reach the referenced check. `CategoryStillReferenced` is
  therefore reachable ONLY for a `UserCategory`; the acceptance's "a category referenced by a
  material" scenario is exercised by adding a fixed-Guid `UserCategory`, adding a material
  categorised under it into the composed `MaterialProxy`, and then removing the category. I read
  the acceptance's "referenced by a seeded material" as "referenced by a material entry in the
  materials store" — a built-in category cannot be the subject of `CategoryStillReferenced` because
  its guard fires first, which is the intended precedence.
- **`materialsReferencingCategory` parameter shadows the module function.** As with the
  `samplesReferencing` precedent, `createInMemory`'s parameter is named `materialsReferencingCategory`
  and shadows the later module-level `materialsReferencingCategory` inside the closure — intentional,
  so a test can inject any lookup (e.g. `fun _ -> []`).
- **Referencing lookup is LIVE, not a snapshot.** It reads the CURRENT `MaterialProxy` on every
  call, so removing / re-categorising the referencing material unblocks the category (the
  `the referencing lookup is live` test pins this).

## Changelog

- 2026-07-07 — Implemented `CategoryProxy.createInMemory` (the real `ref Map<CategoryId,
  MaterialCategory>` store seeded from `standardCategories`) + the module-level
  `materialsReferencingCategory` lookup over `MaterialProxy.listMaterials`, both at the end of the
  `MaterialLibrary` module in `MaterialLibrary.fs`. Added 8 real-store tests to
  `CategoryProxyTests.fs` (add/rename/remove round-trips with fixed Guids, duplicate-add rejection,
  built-in-renamable, unknown-id rejection, the built-in-not-removable block, the
  referenced-category hard block via the composed `MaterialProxy`, and the live referencing lookup).
  Solution builds clean (0 errors); the test file runs 11 passed.
