# Step 042 — impl-log (IMPLEMENT_CONTRACT STORE_XDUO_0008 SeedingProxy)

## Progress

- **Read** the system prompt (`implement_contract_worker.system-md` + its base
  `arc-runner.system-md`, under `AI-Strategy-Generator/src/ai_strategy_generator/multistep/`,
  NOT the repo-root path the task file names), the Berreman project prompt, and the
  slice spec (`.spec-jsonl` step 42). The `## Operator note` section is empty.
- **Surveyed** the step-041 DECLARED seam (`Seeding.fs`: `SeedingError`,
  `[<ReferenceEquality>] SeedingProxy`, pure `seedAll`) and its mock-driven test, then
  the FOUR real in-memory stores the pipeline must fill: `CategoryProxy.createInMemory`
  (`MaterialLibrary.fs`), `MaterialProxy.createInMemory` (`MaterialStore.fs`),
  `SampleProxy.createInMemory` (`SampleStore.fs`), and the read-only library-entry store
  `Library.createInMemory` (`ElementId.fs`). Confirmed each self-seeds and the composition
  root (`AppContext.create`) wires them samples → materials → categories with
  `VersionsInUse.empty`.
- **Added four EMPTY-store constructors** (`createInMemoryEmpty`) in Domain, each beside
  its seeded sibling, via a behaviour-preserving refactor to a shared body.
- **Implemented `SeedingProxy.createInMemory`** in `Seeding.fs`: it builds the four empty
  stores, adapts each store's add verb into the seam's four save fields (mapping the
  store's typed error to `SeedRejected`), and returns an `InMemorySeedStores` bundle so a
  caller seeds then reads the catalogue back — NO database, NO file IO.
- **Added `SeedingInMemoryTests.fs`** (3 facts) and registered it; updated the contract
  lifecycle for STORE_XDUO_0008 to IMPLEMENTED via the real construction.
- **Verified locally** (advisory — the arc-runner's gate engine is the authority): the
  whole solution builds Release with 0 errors and no warning from any changed/new file,
  and every gate is green.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs`
  — refactored `CategoryProxy.createInMemory` to a `static member private createInMemoryStore
  (initialCategories) (materialsReferencingCategory)` (the whole body, byte-identical, moved
  behind an `initialCategories` seed parameter), then delegated `createInMemory`
  (`standardCategories`) and added `createInMemoryEmpty` (`[]`).
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialStore.fs`
  — same refactor for `MaterialProxy`: shared `createInMemoryStore (initialEntries) …`;
  `createInMemory` passes `builtInEntries`, `createInMemoryEmpty` passes `[]`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/SampleStore.fs`
  — same refactor for `SampleProxy`: shared `createInMemoryStore (initialSamples) …`;
  `createInMemory` passes `SeedSamples.all` (equivalent to the old
  `seedEntries |> List.choose SampleItem`), `createInMemoryEmpty` passes `[]`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs`
  — **new** `LibraryEntryStore = { proxy : LibraryProxy; addEntry : LibraryEntry -> Result<unit, LibraryError> }`
  (`[<ReferenceEquality>]`) and `Library.createInMemoryEmpty () : LibraryEntryStore` — the
  read-only `LibraryProxy` over a shared mutable list plus the `addEntry` verb the pipeline
  pushes through (append preserving insertion order; duplicate id → `LibraryUnavailable`;
  empty `libraryTrees`). The read-only seeded `createInMemory` is untouched.
- `Berreman/OpticalConstructor/OpticalConstructor.Seeding/Seeding.fs`
  — added the `open`s for the store augmentations + `Lifecycle`, the
  `[<ReferenceEquality>] InMemorySeedStores` bundle (`proxy` + the four stores), and the
  intrinsic augmentation `SeedingProxy.createInMemory () : InMemorySeedStores`. It builds the
  empty stores (order mirrors `AppContext.create`), adapts `addCategory` / `saveMaterial` /
  `saveSample` / `addEntry` into the four save fields, and maps each store error to
  `SeedRejected` via a per-error-type reason extractor (pattern-match, no case reach-in).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SeedingInMemoryTests.fs`
  — **new**. Three facts: (1) `seedAll` over the empty stores is `Ok ()` and the
  category / material / sample / entry read-back counts equal the Domain seed-list lengths;
  (2) every fixed seed id resolves to the SAME seed instance (reference identity) through
  `listCategories` / `tryGetMaterial` / `tryGetSample` / `tryGetEntry`; (3) a second
  `seedAll` over the seeded stores is rejected (`SeedRejected`) — proving the stores are
  genuinely stateful, not a discarding mock. All `Result`-checked.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
  — registered `SeedingInMemoryTests.fs` after `SeedingProxyTests.fs`.

## Testing state

`commit_ready: true`. Per **Invariant 6 — act only**, the worker implements the contract
and writes its outputs; the arc-runner's deterministic gate engine is the sole gate
authority and re-runs every gate after this session exits. `CLAUDE.md` mandates a green
build and running tests after every change, so I ran ALL FIVE gates locally to verify MY
OWN work (advisory, de-risking a failure-budget burn) — not to green-light a gate:

- **build** (`dotnet build Berreman.slnx -c Release`): **0 errors**. The 10 warnings are all
  pre-existing / third-party (`Wolfram.NETLink` `NU1701` ×2 [exempt], `ChartWindow.fs`
  FS0044, MathNet `SYSLIB0051` ×2, `Dispersion.fs` FS3873, `SeriesDataTests.fs` FS1125 ×4).
  **No changed or new file emits any warning.**
- **constructor-unit-tests**: green — **672 passed / 0 failed / 0 skipped** (669 baseline +
  3 new `SeedingInMemoryTests` facts).
- **unit-tests** (BerremanTests): green — **119 passed / 0 failed / 5 skipped** (baseline 119;
  unaffected — Domain is not referenced by the core solver tests).
- **ui-smoke**: green — **173 passed** (baseline 173; the Domain refactor is behaviour-
  preserving, so `AppContext.create`'s seeded stores render unchanged).
- **ui-tests**: green — **461 passed** (baseline 461).

Acceptance met: `SeedingProxy.createInMemory ()` builds four EMPTY in-memory stores and
`seedAll` reproduces the seeded catalogue — counts match the Domain seed lists
(`standardCategories` / `builtInEntries` / `SeedSamples.all` / `seedEntries`) and the fixed
seed ids resolve to the same seed instances — all `Result`-checked, with no database and no
file IO.

## Artifacts

None produced this round (a code + test slice — no captured logs, screenshots, or traces).
The per-arc artifacts folder is `C:\GitHub\Berreman\specs\0038\.artifacts`.

## Gotchas

- **App composition is untouched.** `AppContext.create` keeps calling the SELF-SEEDING
  `createInMemory` constructors; the refactor moved each seeded body behind a shared
  `static member private createInMemoryStore <initial> …` and left the public
  `createInMemory` delegating with its seed list, so behaviour is byte-identical (the
  ui-smoke / ui-tests gates confirm no regression). The EMPTY `createInMemoryEmpty` variants
  exist SOLELY for the seeding pipeline.
- **The library-entry store had no write verb.** The read-only `LibraryProxy`
  (`entriesForKind` / `libraryTrees` / `tryGetEntry`) closes over an immutable `seedEntries`.
  Rather than widen that record (which the Library browser depends on), the EMPTY variant
  returns a new `LibraryEntryStore = { proxy; addEntry }` — the read proxy over a shared
  mutable list plus the `addEntry` verb the pipeline pushes through. A duplicate entry id maps
  to `LibraryUnavailable` (the only fitting existing `LibraryError` case; no DU widened, so no
  exhaustive-match churn). `libraryTrees` returns `Ok []` for a seeded-empty store — the
  seed-push pipeline fills ENTRIES, not grouping trees; a later store cycle handles trees.
- **`createInMemory` returns a bundle, not a bare `SeedingProxy`.** The acceptance requires
  reading the catalogue BACK after seeding (counts + Guid resolution), which a bare proxy
  cannot expose. `SeedingProxy.createInMemory () : InMemorySeedStores` returns the proxy
  bundled with the four stores it writes into — the "create the in-memory [stores + proxy]"
  reading of the name. A future DB cycle swaps only the four field bindings (the seam and
  `seedAll` are unchanged).
- **Reference identity, not `=`, for seed resolution.** `MaterialEntry` / `Sample` /
  `LibraryEntry` carry function-valued engine payloads (no safe structural `=`); the seeds are
  module-level `let` values and the store holds those very instances, so
  `Object.ReferenceEquals(box seed, box resolved)` is the strongest proof the SAME frozen seed
  flowed through — the step-041 precedent, reused.
- **Sample seed simplified to `SeedSamples.all`.** The old seeded body derived samples via
  `seedEntries |> List.choose (SampleItem …)`; since
  `seedEntries = (SeedSamples.all |> List.map SampleItem) @ [non-samples]`, that choose yields
  exactly `SeedSamples.all` in order — the seeded `createInMemory` now passes `SeedSamples.all`
  directly (equivalent, simpler seed).
- **Library-entry count read-back.** `LibraryProxy` has no "list all" verb; the count test
  unions `entriesForKind` over the kinds the seeds actually use (derived from the seeds, not
  hard-coded), deduped by `entryId` — for the seed set this yields exactly `seedEntries`.
- **System-prompt path drift.** The task file names the prompt at
  `C:\GitHub\AI-Strategy-Generator\implement_contract_worker.system-md`, but it actually lives
  under `.../src/ai_strategy_generator/multistep/` (with its `arc-runner.system-md` base
  alongside). No scope impact — same as step 041's note.
- **LF endings.** `git diff --numstat` equals `--ignore-cr-at-eol` for every edited tracked
  file (no CRLF-only churn); the new `SeedingInMemoryTests.fs` is LF-only, matching the repo
  and `.gitattributes` (`eol: lf`).

## Changelog

- 2026-07-11 — Step 042 (IMPLEMENT_CONTRACT STORE_XDUO_0008): implemented the real in-memory
  `SeedingProxy.createInMemory` over four EMPTY `createInMemoryEmpty` stores (added beside the
  seeded ones in Domain, sharing each seeded body via a private `createInMemoryStore <initial>`;
  the new `LibraryEntryStore` + `Library.createInMemoryEmpty` gives the read-only library-entry
  store a write verb). `seedAll` fills the blank catalogue with no database and no file IO;
  three new tests prove counts match the Domain seed lists, the fixed seed ids resolve to the
  same instances, and a re-seed is rejected. Build green; constructor suite 672 passing.
