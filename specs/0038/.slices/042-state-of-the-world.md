# Step 042 — State of the world

## Where we are

Spec 0038 step 042 (`IMPLEMENT_CONTRACT STORE_XDUO_0008 SeedingProxy`,
`depends_on: 21, 22, 41`). Step 041 DECLARED the seed-push seam (the
`[<ReferenceEquality>] SeedingProxy` + the pure `seedAll` orchestrator over the EXISTING
Domain seeds) with a mock-driven test. This step moves the contract to IMPLEMENTED: a real
`SeedingProxy.createInMemory` backed by the four REAL in-memory catalogue stores, so
`seedAll` fills a BLANK catalogue with NO database and NO file IO. It is the write-path
proof that a future storage cycle needs before it can seed a real store — the DB cycle swaps
only the four field bindings, leaving `seedAll` and the seam unchanged.

## What's working

- Add four EMPTY-store constructors (`createInMemoryEmpty`) in Domain beside the self-seeding
  `createInMemory` ones — `CategoryProxy`, `MaterialProxy`, `SampleProxy`, and the library-entry
  store — sharing each seeded body via a private `createInMemoryStore <initial>` so the two
  constructors never drift.
- Give the read-only library-entry store a write verb: new `LibraryEntryStore = { proxy; addEntry }`
  and `Library.createInMemoryEmpty` (a `LibraryProxy` over a shared mutable list plus `addEntry`),
  leaving the read-only seeded `createInMemory` and the Library browser untouched.
- Implement `SeedingProxy.createInMemory () : InMemorySeedStores`: it builds the four empty stores
  (order mirrors `AppContext.create`), adapts each store's add verb into the seam's four save
  fields, maps each store error to `SeedRejected`, and returns the proxy bundled with the stores
  for read-back — no database, no file IO.
- Add `SeedingInMemoryTests.fs` (3 facts): `seedAll` over the empty stores reproduces the seeded
  catalogue counts; the fixed seed ids resolve to the same seed instances; a re-seed is rejected
  (the stores are genuinely stateful) — all `Result`-checked.
- Leave the app composition (`AppContext.create`) on the self-seeding constructors, unchanged.

## Tests

All five gates were run locally and are green (advisory; the arc-runner re-runs every gate as
the sole authority):

- **build** (whole solution, Release): green — 0 errors; the 10 warnings are all pre-existing /
  third-party (`Wolfram.NETLink` `NU1701` ×2 [exempt], `ChartWindow.fs` FS0044, MathNet
  `SYSLIB0051` ×2, `Dispersion.fs` FS3873, `SeriesDataTests.fs` FS1125 ×4). No changed or new
  file emits a warning.
- **unit-tests** (BerremanTests): green — 119 passed / 0 failed / 5 skipped (baseline 119;
  unaffected — the core solver tests do not reference `OpticalConstructor.Domain`).
- **constructor-unit-tests**: green — 672 passed / 0 failed / 0 skipped (669 baseline + 3 new
  `SeedingInMemoryTests` facts).
- **ui-smoke**: green — 173 passed (baseline 173; the Domain refactor is behaviour-preserving).
- **ui-tests**: green — 461 passed (baseline 461).

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 672
  ui_smoke_tests:         173
  ui_tests:               461
```

## Architecture

- **EMPTY-store constructors share the seeded body.** Each seeded `createInMemory` body was moved
  behind a `static member private createInMemoryStore <initial> …` (Category / Material / Sample)
  parameterised only by the INITIAL collection; the public `createInMemory` delegates with its seed
  list and the new `createInMemoryEmpty` delegates with `[]`. The two constructors are guaranteed
  identical below the seed line — no duplicated store logic — and the seeded path is byte-identical,
  so the app composition and every existing store test are unaffected.
- **The library-entry store gains a write verb without widening its read seam.** `LibraryProxy` is
  read-only (the Library browser depends on that shape). The EMPTY variant returns a new
  `[<ReferenceEquality>] LibraryEntryStore = { proxy : LibraryProxy; addEntry : LibraryEntry -> … }`
  — the read proxy over a shared mutable list plus the add verb the pipeline pushes through — the
  analogue of the catalogue stores' add verbs. A duplicate id maps to the existing `LibraryUnavailable`
  case (no `LibraryError` widening → no exhaustive-match churn).
- **`SeedingProxy.createInMemory` builds the stores and returns a bundle.** The acceptance requires
  reading the catalogue back after seeding, so the constructor returns `InMemorySeedStores` (the
  proxy + the four stores) rather than a bare `SeedingProxy`. Each save field adapts the underlying
  store's add verb and maps its typed error to `SeedRejected` via a per-error-type reason extractor
  (pattern-match, never a case reach-in). A future DB cycle swaps only these four field bindings.
- **Store build order mirrors the composition root** (samples → materials → categories) so the
  referencing lookups (`samplesReferencing` / `materialsReferencingCategory`) stay wired to the live
  stores, even though the seed path only adds.

## Deferred

- **A real disk/DB-backed `SeedingProxy`** — a store body that persists a category / material /
  sample / entry (against the step-040 EFC `OpticalConstructor.Database` or another store) and maps
  a write failure / duplicate to `SeedRejected`. A later storage cycle; it swaps only the four
  `SeedingProxy` field bindings — the seam, `seedAll`, and this in-memory path are unchanged. That
  cycle also seeds the grouping trees (`libraryTrees`), which the seed-push pipeline does not fill.
- **Wiring first-run seeding into a composition root** — the app still uses the self-seeding
  constructors; running `seedAll` over a blank store at startup is a later step.

## Gotchas

- **App composition unchanged.** `AppContext.create` keeps the self-seeding `createInMemory`
  constructors; `createInMemoryEmpty` exists SOLELY for the seeding pipeline.
- **`libraryTrees` is empty on a seeded-empty store.** The seed-push pipeline fills entries, not
  grouping trees; a later store cycle seeds trees. Read-back checks entries (counts + `tryGetEntry`).
- **Reference identity, not `=`, for seed resolution.** The seed types carry function-valued engine
  payloads (no safe structural `=`); the seeds are module-level `let` values and the store holds
  those very instances, so `Object.ReferenceEquals(box seed, box resolved)` is the strongest proof.
- **Sample seed simplified to `SeedSamples.all`** — equal to the old `seedEntries |> List.choose
  (SampleItem …)` because `seedEntries = (SeedSamples.all |> List.map SampleItem) @ [non-samples]`.
- **`SeedingProxy.createInMemory` returns `InMemorySeedStores`, not `SeedingProxy`** — deliberate, so
  the caller can read the reproduced catalogue back (the acceptance demands it).
- **System-prompt path drift.** The task file's
  `C:\GitHub\AI-Strategy-Generator\implement_contract_worker.system-md` is stale; the real prompts
  live under `.../src/ai_strategy_generator/multistep/`. No scope impact.

## Changelog

- 2026-07-11 — Step 042 (IMPLEMENT_CONTRACT STORE_XDUO_0008): implemented the real in-memory
  `SeedingProxy.createInMemory` over four EMPTY `createInMemoryEmpty` stores (added beside the seeded
  ones in Domain via a shared private `createInMemoryStore <initial>`; new `LibraryEntryStore` +
  `Library.createInMemoryEmpty` gives the read-only library-entry store a write verb). `seedAll` fills
  the blank catalogue with no database and no file IO; three new `SeedingInMemoryTests` facts prove
  the counts match the Domain seed lists, the fixed seed ids resolve to the same instances, and a
  re-seed is rejected. App composition untouched. All five gates green locally; constructor suite
  672 passing, 0 failing.
