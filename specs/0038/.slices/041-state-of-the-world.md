# Step 041 — State of the world

## Where we are

Spec 0038 step 041 (`ADD_CONTRACT STORE_XDUO_0008 SeedingProxy`, `depends_on: 14`).
The persistence arc has, across steps 026–040, declared and implemented the scene,
experiment-collection, and measured-data load seams and stood up the EFC-backed
`OpticalConstructor.Database` (step 040). This step declares the **seed-push seam**
in a brand-new `OpticalConstructor.Seeding` project — the write boundary that pushes
the app's built-in seed catalogue (categories → materials → samples → library
entries) into a store, plus the pure `seedAll` orchestrator over the EXISTING Domain
seed values. It is the DECLARED contract only (project + interface + mock + test); a
later `IMPLEMENT_CONTRACT STORE_XDUO_0008` supplies the real store behind it.

## What's working

- Add a new F# class-library project `OpticalConstructor.Seeding` (net10.0, x64,
  references `OpticalConstructor.Domain`) and register it in `Berreman.slnx`.
- Declare the seed-push seam in `Seeding.fs` (module `Seeding`): the
  `SeedingError = SeedRejected of reason` channel and the `[<ReferenceEquality>]
  SeedingProxy` (`saveCategory` / `saveMaterial` / `saveSample` / `saveLibraryEntry`,
  each `<X> -> Result<unit, SeedingError>`) over the elevated Domain types.
- Add the pure `seedAll : SeedingProxy -> Result<unit, SeedingError>` pushing the
  EXISTING Domain seeds (`standardCategories`, `builtInEntries`, `SeedSamples.all`,
  `seedEntries`) in category → material → sample → entry order, short-circuiting on
  the first `SeedRejected`; the seeds stay next to their types.
- Add a recording-stub mock-driven test proving `seedAll` pushes every seed value
  exactly once, in that order, through the exact signatures (plus reference equality
  and a reject-first short-circuit).
- Build green (0 errors, no warning from the new files); constructor suite 669
  passing, 0 failing (666 → 669).

## Tests

- **build** (whole solution, Release): green — 0 errors; the 10 warnings are all
  pre-existing / third-party (`Dispersion.fs` FS3873, `SeriesDataTests.fs` FS1125 ×4,
  `ChartWindow.fs` FS0044, MathNet `SYSLIB0051` ×2, exempt `Wolfram.NETLink`
  `NU1701` ×2). Neither new file emits a warning; the new project compiled clean.
- **constructor-unit-tests**: green — 669 passed / 0 failed / 0 skipped (666
  baseline + 3 new `SeedingProxyTests` facts).
- **unit-tests** (BerremanTests), **ui-smoke**, **ui-tests**: unaffected by this
  new-project + Tests-only change (no touch to Berreman core, Ui, or Ui.Tests) and
  carried forward from the step-040 baseline; the whole solution — Ui / Ui.Tests
  included — compiled clean in the build gate. The arc-runner re-runs every gate as
  the sole authority.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 669
  ui_smoke_tests:         173
  ui_tests:               461
```

## Architecture

- **New project for the seam** (`OpticalConstructor.Seeding`), referencing only
  `OpticalConstructor.Domain`. It sits above the Domain (reads its seed values)
  and below a future storage/wiring layer; keeping it separate lets a later
  `IMPLEMENT_CONTRACT` supply the real store (against `OpticalConstructor.Database`)
  without the Domain depending on any store.
- **Seam as pure DATA + `[<ReferenceEquality>]`** — a record of camelCase
  `Result`-returning functions, the `Library.LibraryProxy` /
  `ExperimentData.ExperimentDataProxy` convention. Function-valued fields have no
  structural equality, so the record compares by identity; logic that holds the proxy
  stays referentially transparent and a test substitutes a recording stub of the same
  shape.
- **`seedAll` is pure orchestration over the existing seeds.** The seeds STAY where
  they live next to their types; `seedAll` only reads them and pushes each through the
  proxy. Order is fixed by foreign-key dependency (category → material → sample →
  entry); a private generic `Result` combinator (`pushEach`) folds each list and the
  four groups chain with `Result.bind`, short-circuiting on the first error.
- **Mock lives with its test** (`OpticalConstructor.Tests`), not the seam project —
  the ADD_CONTRACT precedent (`SceneProxyTests` / `ExperimentDataProxyTests` supply
  their stubs in the test file; the real `create` lands only at IMPLEMENT_CONTRACT).

## Deferred

- The real `SeedingProxy` implementation — the store body that persists a category /
  material / sample / entry (against the step-040 EFC `OpticalConstructor.Database`
  or another store) and maps a write failure / duplicate id to `SeedRejected`. A
  later `IMPLEMENT_CONTRACT STORE_XDUO_0008` in the storage/database layer. That
  cycle MAY grow the `SeedingError` channel without breaking this declared shape.
- Wiring `seedAll` into a composition root / first-run seeding path — a later step.

## Gotchas

- **DECLARED, not implemented.** No real store, no persistence, no `createInMemory`,
  no consumer wiring this round — the `SeedingProxy` seam + the pure `seedAll` only.
- **Seeds stay put; Guids are frozen.** `seedAll` references the existing Domain
  module-level values; it never re-defines a seed. The seeded Guids are future
  foreign keys — the identity that crosses the seam is the one the Domain already
  froze.
- **Reference identity, not `=`, in the test.** `MaterialEntry` / `LibraryEntry`
  carry function-valued engine payloads (no safe structural `=`); the seeds are
  module-level `let` values (computed once), so `Object.ReferenceEquals` proves the
  SAME seed instance flowed through — stronger than value-equality and safe for all
  four types.
- **Order is a foreign-key contract**, not cosmetic: an entry may reference a sample,
  a sample a material, a material a category. The short-circuit fact pins that a
  rejected material stops the chain before any sample/entry is pushed.
- **Module path `OpticalConstructor.Seeding.Seeding`** (namespace + module) — mirrors
  the Domain's `namespace … + module …` precedent; no prior `Seeding` symbol existed.
- **System-prompt path drift.** The task file's
  `C:\GitHub\AI-Strategy-Generator\add_contract_worker.system-md` is stale; the real
  prompts live under `.../src/ai_strategy_generator/multistep/`. No scope impact.

## Changelog

- 2026-07-11 — Step 041 (ADD_CONTRACT STORE_XDUO_0008): declared the seed-push seam
  in a new `OpticalConstructor.Seeding` project — the `SeedingError` channel, the
  `[<ReferenceEquality>] SeedingProxy` (`saveCategory` / `saveMaterial` /
  `saveSample` / `saveLibraryEntry`), and the pure `seedAll` pushing the EXISTING
  Domain seeds (`standardCategories` / `builtInEntries` / `SeedSamples.all` /
  `seedEntries`) in category → material → sample → entry order; registered the
  project in `Berreman.slnx`; added the recording-stub mock-driven test proving every
  seed value is pushed exactly once, in order, through the exact signatures.
  DECLARED lifecycle — no real store. Build green; constructor suite 669 passing, 0
  failing.
