# Slice 005 — impl plan

## Goal

IMPLEMENT_CONTRACT STORE_XDUO_0002 `SampleProxy` (kind proxy → `implemented`
lifecycle): replace the step-004 validate-only mock with the REAL stateful
in-memory store — `SampleProxy.createInMemory : unit -> SampleProxy` closing
over a `ref Map<SampleId, Sample>` seeded from the samples in
`Library.seedEntries`, mutation confined to the closure (the IO boundary) —
plus deterministic round-trip tests (fixed Guids, no IO) in
`OpticalConstructor.Tests`.

## Approach

1. **TDD first** — extend `OpticalConstructor.Tests/SampleProxyTests.fs` with
   the round-trip tests the slice enumerates, written against the not-yet-
   existing `SampleProxy.createInMemory`; the compile error is the red state
   (per the worker base: a missing production symbol counts as red).
   - add-then-list: add a fixed-Guid sample → `Ok`, `listSamples` grows by one
     and contains it.
   - duplicate-add rejection: adding the SAME fixed id twice → the second is
     `DuplicateSampleId` (a stateful duplicate — an id added this run, not a
     seed).
   - update-then-get: update a seeded sample → `tryGetSample` returns the
     UPDATED record.
   - unknown-update rejection: `updateSample` under a fixed never-added id →
     `UnknownSampleId`.
   - remove-then-search: a seeded sample found by `searchSamples` before,
     `removeSample` → `Ok`, the search no longer matches, the list shrinks,
     and a second remove of the same id → `UnknownSampleId`.
   - Existing step-004 tests switch from the shared module-level mock value to
     a FRESH `SampleProxy.createInMemory ()` per test — a shared stateful
     proxy would make them order-dependent (xUnit guarantees no ordering).

2. **`OpticalConstructor.Domain/ElementId.fs`** (the `Library` module):
   replace the mock `createInMemorySampleProxy` with an intrinsic type
   augmentation `type SampleProxy with static member createInMemory () :
   SampleProxy` (the slice's exact call shape; the module-level name
   `createInMemory` is taken by the `LibraryProxy` builder — the step-004
   gotcha). The member seeds `ref (Map.ofList …)` from the `SampleItem`s of
   `seedEntries` — the elevated `SampleId` is the Map key directly. Reads
   answer from the current map; `addSample` persists a fresh sample and
   rejects a held id (`DuplicateSampleId`); `updateSample` replaces a known
   sample and rejects an unknown id (`UnknownSampleId`); `removeSample`
   deletes / rejects likewise; both writes keep the step-004 blank-name
   validation (`InvalidSample`, `validateSample` reused). Mutation is
   `store.Value <- …` inside the closure only. Doc comments on the proxy type
   and `validateSample` updated to the `implemented` lifecycle.

3. **Registry** — `specs/0033/.contracts-json`: per the IMPLEMENT_CONTRACT
   worker prompt, set `STORE_XDUO_0002.lifecycle` to `implemented` and
   `implementStep` to 5 (file is CRLF — arc-runner-owned, edit in place
   without re-ending lines).

## Risks

- A shared stateful proxy across tests is order-dependent — mitigated by a
  fresh proxy per test.
- The type augmentation must sit AFTER `seedEntries` / `validateSample` in
  the file (F# is order-dependent); intrinsic extension in the same module
  can reach the module-private `validateSample`.
- `Map` iteration orders by Guid, not seed order — no test asserts seed
  order, and the contract's `Sample list` carries no order guarantee.
- The step-004 SoW forecast the real store would land in
  `OpticalConstructor.Storage`; the slice's `touches` pins Domain + Tests
  (in-memory ref-cell store, no disk IO) — the slice wins; noted in Gotchas.
- LF-only endings on every touched source file; the registry stays CRLF.
