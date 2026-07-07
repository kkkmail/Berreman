# State of the world — slice 002 (ADD_CONTRACT STORE_XDUO_0003 CategoryProxy)

## Where we are

Slice 002 is the second step of arc 0035. Step 1 turned the material *category* into DATA — a
seeded, `CategoryId`-keyed catalogue. This slice declares the mutating **category write-seam** on
top of that catalogue: the `CategoryProxy` functional proxy plus its error channel and shared
validator, at DECLARED lifecycle (interface + mock + test, no real store). It gives the later
`IMPLEMENT_CONTRACT STORE_XDUO_0003` step a pinned surface to build the real in-memory store
behind, and gives any category-management feature a single write seam to drive.

## What's working

- Declare `CategoryError` (`UnknownCategoryId` / `DuplicateCategoryId` /
  `CategoryStillReferenced` / `BuiltInNotRemovable` / `InvalidCategory`, each carrying a
  diagnostic `reason`).
- Declare the `[<ReferenceEquality>] CategoryProxy` record — `listCategories` / `addCategory` /
  `updateCategory` / `removeCategory` — following the `MaterialProxy` / `SampleProxy` functional-
  proxy convention, beside the step-1 catalogue.
- Add the shared `validateCategory` blank-name validator (the `validateEntry` precedent).
- Add a mock-driven `CategoryProxyTests.fs`: a stub `CategoryProxy` over a fixed category list
  exercises all four functions through their exact signatures, every error case, and reference
  equality.
- Build the whole solution clean (0 errors, no new warnings from changed files); the new test file
  runs 3 passed.

## Tests

- `build` gate: verified locally — `dotnet build Berreman.slnx -c Release` returns 0 errors and
  introduces no new `FS####`/`MSB####` warning from any changed file (`MaterialLibrary.fs`,
  `CategoryProxyTests.fs`, `OpticalConstructor.Tests.fsproj`).
- `constructor-unit-tests` (baseline `constructor_unit_tests`): the new `CategoryProxyTests.fs` was
  executed locally as TDD diligence — `dotnet test … --filter "FullyQualifiedName~CategoryProxyTests"`
  → **3 passed / 0 failed**. This is not a self-reported gate result; the full roster remains the
  gate engine's authority. The slice only ADDS tests, so the `count_at_least` baseline cannot
  regress.
- `unit-tests`, `ui-smoke`, `ui-tests`: deferred to the arc-runner's gate engine per the
  ADD_CONTRACT worker's Invariant 6 (the worker runs no checks; the gate engine is the sole
  authority and runs the roster after exit). These paths are untouched by this slice.

## Architecture

- The category write-seam is a **functional proxy** (`CategoryProxy`), matching the established
  `MaterialProxy` / `SampleProxy` / `LibraryProxy` shape: a `[<ReferenceEquality>]` record of
  camelCase `Result`-returning functions. Reference equality is required because function-valued
  fields have no structural equality — an Elmish host model that holds the proxy compares it by
  identity. IO is modelled as pure data describing behaviour, so the logic that will consume the
  proxy stays referentially transparent and a test substitutes an in-memory stub for the real
  store.
- **Errors are values, elevated, diagnostic.** `CategoryError` carries a `reason : string` on every
  case (a bare error case is useless in a log). `BuiltInNotRemovable` is a net-new case vs. the
  material precedent: a `BuiltInCategory` must never be deleted, so the guard is a distinct typed
  outcome, not a generic `InvalidCategory`.
- **DECLARED lifecycle.** Only the surface + mock + test ship this round; the real stateful store
  (`CategoryProxy.createInMemory`, a type augmentation like `MaterialProxy.createInMemory`) is the
  later `IMPLEMENT_CONTRACT STORE_XDUO_0003` step. No consumer is wired to the proxy yet.

## Deferred

- The real, stateful in-memory category store (`CategoryProxy.createInMemory`) and the referencing
  lookup that backs `CategoryStillReferenced` at composition — the `IMPLEMENT_CONTRACT
  STORE_XDUO_0003` step.
- Any category-management UI or wiring of a live `CategoryProxy` into the app model — out of scope
  for a contract-declaration slice.

## Gotchas

- **Placement: beside the step-1 catalogue.** The declaration sits immediately after
  `tryFindCategoryByName` (the last catalogue helper), before `MaterialComplexity` — the how-to's
  "beside the step-1 category catalogue", using `MaterialProxy` only as the shape to mirror.
- **Built-in guard shadows the referenced-guard for seeded categories.** Every seeded catalogue
  category is `BuiltInCategory`, so in the primary stub `removeCategory` returns
  `BuiltInNotRemovable` before it could reach `CategoryStillReferenced`. A dedicated third stub
  over a referenced `UserCategory` exercises the `CategoryStillReferenced` case through the same
  exact signature.
- **This is a declared contract, not an implementation.** Do not read `CategoryProxy` as a working
  store — nothing mutates real state yet. The next step behind this contract wires the in-memory
  store and the sample/material referencing lookup.

## Changelog

- 2026-07-07 — Declared the mutating category write-seam (`CategoryError` +
  `[<ReferenceEquality>] CategoryProxy` + `validateCategory`) beside the step-1 catalogue in
  `MaterialLibrary.fs`, and added a mock-driven `CategoryProxyTests.fs` (stub over a fixed category
  list) exercising all four functions through their exact signatures and every error case.
  Solution builds clean (0 errors); the new test file runs 3 passed.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 418
  ui_smoke_tests: 83
  ui_tests: 310
```
