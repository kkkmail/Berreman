# State of the world — slice 001

## Where we are

Slice 001 is the first, foundational step of arc 0035: it turns the material *category* from a
closed compile-time union into DATA — a seeded, id-keyed catalogue. Every later slice that builds
on categories (user-authored categories, category management UI, richer filtering) now has a
`CategoryId`-based domain model to hang off, and no downstream code carries the old union.

## What's working

- Replace the closed `MaterialCategory` union with a `CategoryId`-keyed record catalogue seeded by
  `standardCategories` (Glass/Metal/Semiconductor/Crystal selectable, Vacuum hidden-on-create).
- Add `CategoryId`, `CategoryVisibility`, `CategoryOrigin`, `CategoryIds` (fixed literal Guids), and
  the `categoryName` / `tryFindCategory` / `tryFindCategoryByName` resolution seams.
- Re-type `MaterialEntry.category` and `MaterialQuery.category` to `CategoryId`, re-seed all built-in
  entries, and re-express `byCategory` as an id filter.
- Re-point every consumer (Ui, Storage, TestWindows, tests) to resolve category names through the
  catalogue by id; the create picker offers `SelectableOnCreate` categories only (Vacuum removed).
- Build the whole solution clean (0 errors, no new warnings from changed files).
- Pin the new `tryFindCategoryByName` persistence seam with two `ExportImportTests` facts: category
  round-trips through `exportMaterials`/`importMaterials` back to the matching `CategoryId`, and an
  unknown category name yields a typed `JsonParseError` (no throw, no silent default).

## Tests

- `build` gate: verified locally — `dotnet build Berreman.slnx -c Release` returns 0 errors and
  introduces no new `FS####`/`MSB####` warning from any changed file.
- `constructor-unit-tests`, `ui-smoke`, `ui-tests`, `unit-tests`: deferred to the arc-runner's gate
  engine per the IMPLEMENT worker's Invariant 6 (the worker runs no checks; the gate engine is the
  sole authority and runs the roster after exit). The four tests carrying category values were
  updated to the new `CategoryId`/`CategoryIds` shape as part of the change.
- **(attempt-02)** The two net-new `ExportImportTests` facts were executed locally as TDD diligence
  for a test-only change: `dotnet test … --filter "FullyQualifiedName~ExportImportTests"` →
  **6 passed / 0 failed** (the 4 originals + the 2 new). This is not a self-reported gate result —
  the `constructor-unit-tests` roster remains the gate engine's authority.

## Architecture

- Categories are now DATA resolved by identity, not a fixed union: a name is looked up through
  `standardCategories` by `CategoryId` (the single `categoryName` seam), and the create/edit picker
  is driven by the `CategoryVisibility` facet rather than a hard-coded list. This mirrors the
  existing `MaterialId` / `MaterialIds` treatment (single-case Guid DU + fixed-literal seeds) and
  keeps every domain signature free of a closed union — the "elevate every primitive" discipline.
- The persisted/wire boundary keeps the category *display name* string (matching the `materialEntry`
  `$def` enum); the record↔name mapping lives in exactly one place (`Report.fs`, via the catalogue
  seams), so the "one JSON stack" rule holds and the schema check is unchanged.

## Deferred

- User-authored categories (`origin = UserCategory`) and any category-management UI are out of scope
  for this slice — the record and `CategoryId.create` seam exist, but nothing mints or persists a
  user category yet.
- Rebuilding an imported/persisted entry's full `OpticalPropertiesWithDisp` (the §D.9 mapping) stays
  as it was: metadata-only library entries remain view-only. Unchanged by this slice.

## Gotchas

- The slice's `touches` list names Domain/TestWindows/Tests, but `MaterialCategory` is also used by
  `OpticalConstructor.Ui` (`MaterialsView.fs`) and `OpticalConstructor.Storage`
  (`MaterialImport.fs`, `Report.fs`); those had to change too so the whole-solution `build` gate
  stays green and no closed-union path remains.
- `Report.fs` no longer relies on the fieldless-DU JSON unwrap (a record cannot use it); it persists
  the category *name* and resolves it back through the catalogue. On-disk format and schema check
  are byte-for-byte the same; an unknown category name is a typed `JsonParseError`.
- `CategoryIds` uses five brand-new fixed literal Guids (distinct from `MaterialIds`); a
  persisted/seeded category resolves to the same identity across runs.
- **(attempt-02) Pre-existing `description`-field round-trip gap, left as-is (out of scope).** The
  round-trip test surfaced that `importMaterials` cannot re-read an entry `exportMaterials` wrote with
  `description = None` (export omits the field; the DTO's `description : string option` deserializes as
  REQUIRED → `JsonParseError "Missing field … description"`). This predates the slice (the diff changed
  only `category`) and per the retry hint is NOT fixed here — a fix touches the persisted-format
  contract and belongs to its own slice. The category round-trip test exports only described built-ins
  (one per seeded category) to isolate the `tryFindCategoryByName` seam from this unrelated gap.

## Changelog

- 2026-07-07 — Category becomes DATA: `MaterialCategory` union → `CategoryId`-keyed seeded catalogue
  (record + `CategoryVisibility`/`CategoryOrigin` DUs + `CategoryIds` fixed Guids). `MaterialEntry`
  and `MaterialQuery` re-typed, built-ins re-seeded, `byCategory`/`byQuery` re-expressed, and all
  consumers (Ui/Storage/TestWindows/Tests) re-pointed to resolve names through the catalogue by id.
  Create picker offers `SelectableOnCreate` categories only. Solution builds clean.
- 2026-07-07 (attempt-02 retry) — Added two `ExportImportTests` facts pinning the new
  `tryFindCategoryByName` persistence seam (category round-trip by `CategoryId`; unknown-name →
  typed `JsonParseError`). Ran them locally — `ExportImportTests`: 6 passed. No production code
  changed this round.

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
  ui_smoke_tests: 0
  ui_tests: 0
```
