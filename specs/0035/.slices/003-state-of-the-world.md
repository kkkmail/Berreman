# State of the world — slice 003 (IMPLEMENT_CONTRACT STORE_XDUO_0003 CategoryProxy)

## Where we are

Slice 003 is the third step of arc 0035. Step 1 turned the material *category* into DATA (a seeded,
`CategoryId`-keyed catalogue); step 2 DECLARED the mutating category write-seam (`CategoryProxy` +
`CategoryError` + `validateCategory`, interface + mock + test only). This slice brings that contract
to **implemented** lifecycle: the real, stateful in-memory category store behind the seam, plus the
composition-root referencing lookup that backs `CategoryStillReferenced`. Any future
category-management feature now drives a working store, not a mock.

## What's working

- Implement `CategoryProxy.createInMemory (materialsReferencingCategory)` — the real `ref Map<CategoryId, MaterialCategory>` store seeded from `standardCategories`, mutation confined to the closure.
- Enforce the write guards: `addCategory` validates then hard-blocks a `DuplicateCategoryId`; `updateCategory` validates, replaces a known id, and rejects an `UnknownCategoryId` (built-ins ARE renamable); `removeCategory` refuses a `BuiltInCategory` (`BuiltInNotRemovable`) and blocks a referenced `UserCategory` (`CategoryStillReferenced`, naming the materials), else removes it.
- Add the module-level `materialsReferencingCategory` lookup over `MaterialProxy.listMaterials`, filtering entries by `CategoryId` — the live composition-root backing.
- Add 8 real-store tests to `CategoryProxyTests.fs` (add/rename/remove round-trips with fixed Guids, duplicate-add rejection, built-in-renamable, unknown-id rejection, the built-in-not-removable block, the referenced-category hard block via the composed `MaterialProxy`, and the live referencing lookup); the step-002 stub tests stay verbatim.
- Build the whole solution clean (0 errors, no new warnings from changed files); the test file runs 11 passed.

## Tests

- `build` gate: verified locally — `dotnet build Berreman.slnx -c Release` returns 0 errors and
  introduces no new `FS####`/`MSB####` warning from any changed file (`MaterialLibrary.fs`,
  `CategoryProxyTests.fs`). The 94 warnings are all pre-existing (`MSB3277` WindowsBase conflicts,
  `FS1125` in the untouched `SeriesDataTests.fs`).
- `constructor-unit-tests` (baseline `constructor_unit_tests`): the new + kept `CategoryProxyTests.fs`
  was executed locally as TDD diligence — `dotnet test … --no-build --filter "FullyQualifiedName~CategoryProxyTests"`
  → **11 passed / 0 failed**. Not a self-reported gate result; the full roster remains the gate
  engine's authority. The slice only ADDS tests, so the `count_at_least` baseline cannot regress.
- `unit-tests`, `ui-smoke`, `ui-tests`: deferred to the arc-runner's gate engine per the
  IMPLEMENT_CONTRACT worker's Invariant 6 (the worker runs no checks; the gate engine is the sole
  authority and runs the roster after exit). These paths are untouched by this slice.

## Architecture

- **The store is a functional-proxy `createInMemory`, matching the `SampleProxy` / `MaterialProxy`
  precedent** (`ElementId.fs`): a `ref Map` keyed by the elevated id directly, all mutation confined
  to the closure (the IO boundary), so the logic that will hold the proxy stays referentially
  transparent and a test substitutes an in-memory lookup for the real store. IO is modelled as pure
  data describing behaviour.
- **Intrinsic augmentation, in this file.** The augmentation stays in `MaterialLibrary.fs` (not
  `ElementId.fs`) because the category referencing lookup is `MaterialEntry`-typed and
  `MaterialEntry` compiles above in the same module — no forward-declared type is needed, unlike
  `MaterialProxy.createInMemory` whose `Sample`-typed lookup forced it downstream into `ElementId.fs`.
- **Referenced-block precedence.** `removeCategory` checks origin FIRST: a `BuiltInCategory` is
  refused unconditionally (`BuiltInNotRemovable`), so `CategoryStillReferenced` is reachable only for
  a `UserCategory`. This is the intended precedence — a shipped built-in must never be deletable,
  referenced or not.
- **The referencing lookup is LIVE.** `materialsReferencingCategory` reads the current
  `MaterialProxy` on every call — once the referencing materials are re-categorised or removed the
  category becomes removable; there is no snapshot to refresh (the `samplesReferencing` precedent).

## Deferred

- Wiring a live `CategoryProxy` into the app model / a category-management UI — out of scope for a
  store-implementation slice; a later step wires the composition root and any editor.
- A disk-backed `CategoryProxy.create` in `OpticalConstructor.Storage` (the real store here is
  in-memory only) — future persistence work, unchanged by this slice's pure seam.

## Gotchas

- **The built-in guard shadows `CategoryStillReferenced` for every SEEDED category.** All five
  seeded categories are `BuiltInCategory`; `removeCategory` returns `BuiltInNotRemovable` before it
  could reach the referenced check. The referenced-block test therefore uses a fixed-Guid
  `UserCategory` referenced by a material added to the composed `MaterialProxy` — I read the
  acceptance's "a category referenced by a seeded material" as "referenced by a material entry in
  the materials store", the only way `CategoryStillReferenced` is reachable.
- **`createInMemory`'s parameter shadows the module `materialsReferencingCategory`.** Intentional
  (the `samplesReferencing` precedent) — a test injects any lookup, e.g. `fun _ -> []`.
- **Do not confuse the store's `ref Map` with persistence.** It is in-memory and per-proxy; every
  test builds a FRESH proxy so xUnit ordering cannot leak state.

## Changelog

- 2026-07-07 — Implemented `CategoryProxy.createInMemory` (the real `ref Map<CategoryId,
  MaterialCategory>` store seeded from `standardCategories`) + the module-level
  `materialsReferencingCategory` lookup over `MaterialProxy.listMaterials`, both at the end of the
  `MaterialLibrary` module in `MaterialLibrary.fs`. Added 8 real-store tests to
  `CategoryProxyTests.fs` exercising add/rename/remove round-trips (fixed Guids), the duplicate-add
  rejection, the built-in-not-removable block, the referenced-category hard block, and the live
  referencing lookup. Solution builds clean (0 errors); the test file runs 11 passed.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 418
  ui_smoke_tests: 83
  ui_tests: 310
```
