# Step 042 — impl-plan (IMPLEMENT_CONTRACT STORE_XDUO_0008 SeedingProxy)

## Goal

Move `SeedingProxy` from DECLARED (step 041 — seam + pure `seedAll` + mock-driven
test) to IMPLEMENTED: a real `SeedingProxy.createInMemory` backed by the REAL
in-memory stores, so `seedAll` fills a blank catalogue with NO database and NO
file IO. Acceptance: `seedAll` over EMPTY in-memory stores reproduces the seeded
catalogue — category / material / sample / entry counts match the Domain seed
lists and the fixed seed Guids resolve — all `Result`-checked, no real IO.

## Approach

The existing store `createInMemory` constructors SELF-SEED (they close over
`standardCategories` / `builtInEntries` / `SeedSamples.all` / `seedEntries`). The
seeding pipeline needs a BLANK store to fill, so I add EMPTY-store constructors
`createInMemoryEmpty` beside each seeded one, sharing the exact store body via a
`static member private createInMemoryStore initial …` (the ONLY difference is the
initial map — seeded passes the seed list, empty passes `[]`), so the two
constructors never drift. The app composition (`AppContext.create`) keeps calling
the self-seeding `createInMemory` unchanged.

Four EMPTY constructors in Domain:

- `CategoryProxy.createInMemoryEmpty` (`MaterialLibrary.fs`) — beside `createInMemory`.
- `MaterialProxy.createInMemoryEmpty` (`MaterialStore.fs`) — beside `createInMemory`.
- `SampleProxy.createInMemoryEmpty` (`SampleStore.fs`) — beside `createInMemory`.
- `Library.createInMemoryEmpty` (`ElementId.fs`) — the library-entry store. The
  read-only seeded `LibraryProxy` has NO write verb, so the empty variant returns a
  new `LibraryEntryStore = { proxy : LibraryProxy; addEntry : LibraryEntry -> … }`
  record — a `LibraryProxy` over a shared mutable list plus the `addEntry` verb the
  pipeline pushes through. Duplicate entry id → `LibraryUnavailable` (the one fitting
  existing case); insertion order preserved so the reads match the seeded store.

`SeedingProxy.createInMemory () : InMemorySeedStores` (in `Seeding.fs`) builds the
four EMPTY stores (build order mirrors `AppContext.create`: samples → materials →
categories, keeping the referencing lookups wired to the live stores), adapts each
store's add verb (`addCategory` / `saveMaterial` / `saveSample` / `addEntry`) into
the four `SeedingProxy` save fields — mapping each store's typed error to
`SeedRejected reason` via a per-error-type reason extractor — and returns a
`[<ReferenceEquality>] InMemorySeedStores` bundling the `SeedingProxy` with the four
stores so a caller seeds then reads back. A future DB cycle swaps only these field
bindings, leaving `seedAll` and the seam unchanged.

Tests (new file `SeedingInMemoryTests.fs` in `OpticalConstructor.Tests`; the 041
`SeedingProxyTests.fs` stays untouched):

1. `seedAll` over the empty stores is `Ok ()`; category / material / sample / entry
   read-back counts equal the Domain seed-list lengths (entries counted by unioning
   `entriesForKind` over the seeds' kinds, deduped by entry id).
2. The fixed seed ids resolve to the SAME seed instances (reference identity) through
   `listCategories` / `tryGetMaterial` / `tryGetSample` / `tryGetEntry`.
3. Re-running `seedAll` over the already-seeded stores is rejected (`SeedRejected`) —
   proving the in-memory stores are genuinely stateful, not a discarding mock.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialStore.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/SampleStore.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Seeding/Seeding.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SeedingInMemoryTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (register the new test)

## Risks

- **Behaviour-preserving refactor of the seeded constructors.** Extracting a shared
  `createInMemoryStore initial …` keeps the body byte-identical; the seeded member
  delegates with the seed list, so `AppContext` behaviour is unchanged. Verified by
  the whole build + the existing Category/Material/Sample proxy tests staying green.
- **`SeedSamples.all` vs the old `seedEntries |> List.choose (SampleItem …)`** in the
  sample store: `seedEntries = (SeedSamples.all |> List.map SampleItem) @ [non-samples]`,
  so the choose yielded exactly `SeedSamples.all` in order — the seeded member passes
  `SeedSamples.all`, an equivalent, simpler seed.
- **Library entry store has no write verb.** Adding `LibraryEntryStore` + `addEntry`
  is the minimal seam change (no touch to the read-only `LibraryProxy` record the
  browser depends on).
- **`--warnaserror+:25` / zero-warnings.** No new generic-instantiation warnings; the
  reason extractors are exhaustive single-field matches; interpolated strings only.
- **LF endings** — verify no CRLF churn after editing.
