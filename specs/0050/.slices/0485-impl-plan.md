# Step 0485 — impl plan

## Approach

The Planner's candidate cache (spec 0050 S14.2; 001 §M): a key per candidate, a hashed file per key under the kit's
`.oc/planner/`, and one `KitContext` member that answers a hit from the file and computes, then writes, a miss.

1. `MeasurementPlanner.fs` (Domain), after step 483's declarations (the end of the module, so no earlier record
   construction sees the new field labels):
   - `PlannerError` gains `PlannerCacheErr of reason : string`, described as
     `the planner's cache cannot be read or written: <reason>`.
   - `PlannerCacheKey = { source : Item; plate : Item; detector : Item; observable : Observable; material : OpenedMaterial;
     targets : ParameterName list }` and `PlannerCacheEntry = { key : PlannerCacheKey; rows : CandidateRows }`.
   - `type PlannerInputs with member this.cacheKeyWith (items) (candidate)`: no polarimeter, or an id `items` lacks,
     is `UnresolvedPlannerItemErr` naming it (the item's label, worded as step 483's "the kit holds no such item").
2. New `PlannerCacheStore.fs` (Storage), compiled directly after `KitContentFiles.fs`:
   - `PlannerCacheKey.cacheFileName` is `contentHashOf this` (step 123's SHA-256 of the compact canonical JSON) plus
     `.candidate-json`.
   - `PlannerCacheEntry.fileText` is `nodeOf` rendered by `canonicalFileText`.
   - `KitFileText.parsePlannerCacheEntry` binds through `CanonicalJson.options`, mapping any failure (and a bare
     `null`) to `MalformedJsonErr`, as the lock's pair does (step 129).
   - `KitContext.cachedPlannerRows key compute`: name → `readPlannerCacheFile`; an entry of an equal key is a hit;
     no file, an unbound text or another key is a miss: `compute ()`, then under `ReadWriteKitAccess` the entry is
     written, under `ReadOnlyKitAccess` nothing. Name, read, render and write errors are `PlannerCacheErr` with their
     `describe`; a `compute` error passes through unwritten.
3. Tests: new `PlannerCacheTests.fs`, appended last in the test project, with exactly the five census facts.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/PlannerCacheStore.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerCacheTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
- Consequential, outside `effects.edits`:
  - `PlannerInputsTests.fs` and `PlannerReportTests.fs`. Each matches `PlannerError` exhaustively, and the test
    project fails on an incomplete match (`--warnaserror+:25`), so each or-pattern gains `PlannerCacheErr _`. Steps
    470, 471, 477 and 483 made the same edit.
  - `PlannerInputsTests.fs`: the kit fixtures the cache facts reuse lose `private`. The step builds its keys "from
    `PlannerInputsTests`' kit", and AGENTS.md's "Reuse, never copy" rules out a second copy of that fixture.

## Risks

- The hit compares the bound key with `=`, so `Item` and `OpenedMaterial` must survive the canonical JSON round trip
  unchanged. Both already round-trip as item and study files, and the first fact checks it end to end.
- New record labels (`source`, `plate`, `observable`, `material`, `targets`, `key`, `rows`) could capture an
  unannotated record construction later in the compile order. The build decides; every construction here is
  annotated.
- ELEV: the new members are on our types, with no free functions and no bare primitives in a signature.
