# Where we are

Step 0142 implements S42-OVERLAY after snapshot capture and read-only resolution. A constructor session can now compose its library, material, sample and category stores with a kit's snapshots and confirmed rebindings. The requested implementation and four authorized facts are complete; executable verification belongs to the arc-runner after worker exit.

Attempt 02 repairs the supervisor-reported FS0039 build failure by importing `OpticalConstructor.Domain.Placement` in both the overlay implementation and its tests. All implementation bodies and acceptance assertions are preserved; fresh supervisor verification remains pending.

Attempt 03 repairs the subsequent measured test failure: the divergent-version fixture expected version 2 after a description-only edit, which correctly stays at version 1. The successor fixture now changes physics using the existing Glass 1.52 catalogue value, so the pinned save takes the existing successor-version branch. All prior assertions are preserved, with explicit backing-store assertions added for both versions. Production behavior is unchanged in this attempt.

# What's working

- Add `EntryReplacement`, `KitLibraryOverlay` and `KitLibraryStores` with documented session contracts.
- Open overlays without writes, resolving rebinding targets through the existing latest-envelope import seam.
- Resolve exact versions from snapshots and id lookups from held entries, with explicit replacements taking precedence.
- Offer highest-version snapshot-only entries and categories, deriving a distinct By kind tree through the existing tree builder.
- Preserve every write field and material/sample list/search field while mapping decoder refusals to typed store errors.
- Retain the four authorized facts, repair the Placement imports, and make the successor fixture exercise pinned physics versioning with backing-store assertions.

# Tests

No build, test, analyzer or invariant gate was run by this worker, as required by IMPLEMENT Invariant 6. The supervisor owns `build`, `elevation`, all-discovered `constructor-unit-tests` and the eleven S16 invariants in `0142.gates`. No passing result or commit readiness is claimed.

The attempt-02 task reports the prior supervisor build failed with six FS0039 errors for `CatalogueKind` and its cases. The missing import is repaired in both authorized files; this is a source repair, not a measured passing build.

The attempt-03 task reports the subsequent review and build passed, then constructor-unit-tests failed with 1125 total, 1124 succeeded and one failed at the divergent-version fact's version-2 lookup. Source tracing identified an invalid fixture precondition, repaired within the authorized test file. No fresh passing result is claimed; the supervisor must execute verification after this handoff.

The four facts also assert highest-version selection, old exact-version preservation, unknown-id fallback, live latest-store reads, exclusion of held/replaced ids from snapshot offers, decoder refusal diagnostics, typed read errors and short-circuiting. They inspect unchanged kit metadata/snapshots and backing-library listings and verify that all write and material/sample list/search functions pass through unchanged.

The following repeats the supervisor's existing checkpoint `140` baseline. The supervisor's failed run discovered the expected 1125 cases; its failed run does not establish a new green baseline. No checkpoint or test census was edited.

```yaml
gates:
  constructor_unit_tests: 1121
```

# Architecture

`KitLibraryContext.openOverlay` lists snapshots once and reads each rebinding's held envelope through `CatalogueImportProxy.tryLatestEnvelope`, preserving typed kit/import errors and refusing missing targets with `UnheldEntryErr`. It works with read-only kits.

A private generic read combinator applies replacement-first precedence, then snapshot/store for exact-version references or store/highest-snapshot for id references. Each store uses the existing content decoders; no kind-neutral id is converted into a domain id outside those decoders. Library and category offers group snapshots by identity, retain the highest version and omit replacements and held ids. Library trees reuse `LibraryTree.byKind` over the five prescribed kinds, distinct by entry id.

The four store extensions compose pure proxy records. Material/sample list and search fields and every mutating field retain the original functions. No cache, write-back, storage format, migration or UI wiring is introduced.

# Deferred

None of the requested implementation is deferred. Supervisor gate execution remains pending under the worker protocol.

# Gotchas

- `specs/0050/.manifest.state.json` was modified before this worker started and was left untouched.
- Attempt 03 also inherited a modified `specs/0050/.expected-red-observed-json`; it was left untouched.
- Pinning alone does not make a description edit create version 2. The fixture must change physics to exercise exact-version store fallback, per the existing `decideVersioning` contract.
- `CatalogueKind` and its cases are declared in `Placement`; an `open Library` does not import them transitively.
- Id reads and exact-version reads deliberately have different precedence; changing either can silently substitute library content for a kit's recorded version.
- A held id stays out of snapshot offers even when absent from a kind's current listing; the library overlay consults the underlying id lookup before appending it.
- Read failures are preserved rather than treated as missing entries. Malformed snapshots become the receiving store's unavailable error with the snapshot identity and decoder diagnostic.
- The import seam takes `EntryVersionPut`, including its latest-movement decision; the latest-held rebinding fixture uses that existing contract.
- All requested output documents use the task's exact paths. Product/test changes are limited to the two authorized files, with LF line endings.

# Changelog

2026-11-07 — Implemented read-only kit library overlays, latest-held rebindings, snapshot-aware resolution and offers, unchanged write-through fields, and the four authorized facts; supervisor verification pending.

2026-11-07 — Attempt 02: repaired the missing Placement import in the implementation and tests after the supervisor's FS0039 build failure; preserved acceptance and left gates to the supervisor.

2026-11-07 — Attempt 03: corrected the divergent-version fixture to mint its successor through a pinned physics edit, added backing-store assertions for both versions, and retained every existing assertion; supervisor verification pending.
