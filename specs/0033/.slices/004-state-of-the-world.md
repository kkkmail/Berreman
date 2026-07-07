# State of the world — spec 0033, slice 004

# Where we are

Slice 004 is the second ADD_CONTRACT step of arc 0033: it DECLARES the mutating
samples write-seam `SampleProxy` (contract STORE_XDUO_0002, kind proxy) in
`OpticalConstructor.Domain`, beside the `Library` module's existing types —
with the same functional-proxy shape slice 003 established for `MaterialProxy`.
The surface — `SampleQuery`, the reason-carrying `SampleError` channel, and the
six-function `[<ReferenceEquality>]` proxy record — is pinned by an in-memory
mock over the fixed `SeedSamples.all` list and a mock/stub-driven test suite.
The contract stays at `declared` lifecycle: no persisting store was built —
that is the later IMPLEMENT_CONTRACT STORE_XDUO_0002 step (in
`OpticalConstructor.Storage`), which slots in behind this exact shape leaving
callers unchanged.

# What's working

- Declare SampleProxy (STORE_XDUO_0002): listSamples / searchSamples / tryGetSample / addSample / updateSample / removeSample, all Result-returning, per the LibraryProxy/MaterialProxy functional-proxy convention
- Add the SampleQuery record (text, substrate : SubstrateKind option) with a match-everything SampleQuery.empty
- Add SampleError = UnknownSampleId | DuplicateSampleId | InvalidSample — every case carries reason : string
- Ship the in-memory mock createInMemorySampleProxy: an inline stub over the FIXED SeedSamples.all list — reads answer from the list, writes validate and return the typed outcome without persisting
- Add SampleProxyTests (+14): mock-driven coverage of every field plus the acceptance stub exercising all six functions through their exact signatures

# Tests

All gates in the slice roster pass in the worker's local (advisory) run; the
arc-runner gate engine re-runs them authoritatively after exit.

- `build` — solution builds Release/x64, 0 errors (`--warnaserror+:25` clean,
  no warnings from touched files).
- `unit-tests` (BerremanTests) — 84 passed, 5 skipped (pre-existing skips), 0 failed.
- `constructor-unit-tests` — 339 passed, 0 failed (14 tests added this round:
  the SampleProxy mock read/write surface, the search facets incl. the empty
  Wedge result, the typed error cases, the reference-equality pin, and the
  six-function stub acceptance test; none removed).
- `ui-smoke` — 54 passed, 0 failed.
- `ui-tests` — 249 passed, 0 failed.

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 339
  ui_smoke_tests:         54
  ui_tests:               249
```

# Architecture

- **The write-seam is a functional proxy at `declared` lifecycle.**
  `SampleProxy` follows `LibraryProxy` / `ExperimentProxy` / `MaterialProxy`
  exactly: a `[<ReferenceEquality>]` record of pre-curried, `Result`-returning
  camelCase functions, so an Elmish host model holding one keeps its equality
  and a test substitutes a stub of the same shape. The real store
  (IMPLEMENT_CONTRACT, `OpticalConstructor.Storage`) will be a `create` that
  captures actual persistence into the same six fields.
- **Search input is DATA.** `SampleQuery` (case-insensitive name fragment +
  `SubstrateKind option` facet) makes `searchSamples` the single search seam —
  the samples panel drives one query record instead of composing ad-hoc
  filters, the `MaterialQuery` convention.
- **The substrate facet is the existing `SubstrateKind` DU** (ThinFilm |
  Plate | Wedge, `ElementId.fs`) wrapped in `option` — never a `bool` flag; a
  future geometry is a non-breaking new case, and `Some Wedge` today matches
  an empty (but well-typed) result over the seeds.
- **Errors carry diagnosis.** All three `SampleError` cases are
  `of reason : string`; the mock's reasons name the offending Guid string
  form, matching the slice-002/003 precedent. Unlike `MaterialError`, this is
  a new DU rather than an extension, so no existing match sites were touched.

# Deferred

- The real, persisting `SampleProxy` implementation (mutable store,
  persistence, any cross-referencing rules) — the later IMPLEMENT_CONTRACT
  STORE_XDUO_0002 step; the contract registry stays `declared`
  (supervisor-maintained, already recorded for step 4).
- Reconciling the samples write-seam with the read-only `LibraryProxy`
  sample listing (the `SampleItem` entries) — a wiring concern for the later
  steps that consume both seams; nothing consumes `SampleProxy` yet by design.
- Structure rotation in `sampleToSystem` — step 20 of this arc (unchanged
  from slices 001–003).

# Gotchas

- The sample mock is `createInMemorySampleProxy`, NOT `createInMemory` — the
  `Library` module already binds `createInMemory` for the `LibraryProxy`, and
  a duplicate module-level `let` is a compile error (FS0037).
- The mock's write functions VALIDATE but never persist — `addSample` on a
  fresh id returns `Ok ()` yet a following `listSamples` still returns the
  fixed seeded list. That is the intended `declared`-lifecycle stub behaviour,
  not a bug; do not "fix" it before IMPLEMENT_CONTRACT.
- Every `SampleError` case surfaces through both the mock and the acceptance
  stub (no `MaterialStillReferenced`-style stub-only case this time); the stub
  routes `removeSample` of an unknown id to `UnknownSampleId`.
- F# parses `Assert.False(p = q)` as a named-argument assignment (FS0505) —
  proxy reference-equality assertions need double parens:
  `Assert.False((p = q))` (same as slice 003).
- `specs/0033/.manifest.state.json` carries CRLF but is the arc-runner's own
  file — left alone (same as slices 001–003).

# Changelog

- 2026-07-05 — slice 004: SampleProxy write-seam DECLARED (STORE_XDUO_0002):
  SampleQuery (text + SubstrateKind facet), the new three-case reason-carrying
  SampleError, the [<ReferenceEquality>] six-function proxy record, the
  fixed-SeedSamples in-memory mock (createInMemorySampleProxy), and +14
  mock/stub tests incl. the six-function acceptance; all suites green.
- 2026-07-05 — slice 003: MaterialProxy write-seam DECLARED (STORE_XDUO_0001):
  DispersionFilter + MaterialQuery, MaterialError extended with three
  reason-carrying cases, the [<ReferenceEquality>] six-function proxy record,
  the fixed-list in-memory mock, and +15 mock/stub tests incl. the
  six-function acceptance; TestWindows error rendering kept exhaustive; all
  suites green.
- 2026-07-05 — slice 002: MaterialId/SampleId elevated to Guid-backed DUs;
  fixed-literal seed ids (MaterialIds / SeedSamples); resolveMaterial is a
  MaterialId lookup; UnknownMaterialId carries reason; imports mint ids;
  JSON/drag boundaries parse the Guid string form; +4 round-trip tests, all
  suites green.
- 2026-07-05 — slice 001: sample stacks made DATA (SampleStructure + typed
  material resolution); id-branching sampleToSystem replaced by total
  ResolvedSample mapping; 4 new material built-ins; 11 samples re-seeded
  structurally; host resolves once per run; +5 tests, all suites green.
