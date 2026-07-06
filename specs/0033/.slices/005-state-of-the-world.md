# State of the world — spec 0033, slice 005

# Where we are

Slice 005 is the first IMPLEMENT_CONTRACT step of arc 0033: it takes the
samples write-seam `SampleProxy` (contract STORE_XDUO_0002, declared by
slice 004) to `implemented` lifecycle. The step-004 validate-only mock
(`createInMemorySampleProxy`) is REPLACED by the real, stateful in-memory
store `SampleProxy.createInMemory : unit -> SampleProxy` in
`OpticalConstructor.Domain` (`ElementId.fs`): a closure over a
`ref Map<SampleId, Sample>` seeded from the samples in `seedEntries` — the
elevated `SampleId` is the Map key directly, mutation stays inside the closure
(the IO boundary), and the callers' shape is unchanged. Deterministic
fixed-Guid round-trip tests (add-then-list, update-then-get,
remove-then-search, plus the duplicate-add and unknown-update rejections) pin
the stateful behaviour. Next up: slice 006 does the same for `MaterialProxy`
(STORE_XDUO_0001).

# What's working

- Implement SampleProxy.createInMemory (STORE_XDUO_0002 → implemented): a stateful in-memory store over a ref Map<SampleId, Sample> seeded from the samples in seedEntries, mutation confined to the closure
- Replace the step-004 validate-only mock createInMemorySampleProxy with the real store — addSample / updateSample / removeSample now persist; reads answer from the current map
- Keep the typed rejections: DuplicateSampleId on a held id, UnknownSampleId on unknown update/remove, InvalidSample on blank names — every reason carries the offending Guid string
- Add 5 fixed-Guid round-trip tests (add-then-list, stateful duplicate-add, update-then-get, unknown-update, remove-then-search incl. remove-again-unknown) and isolate every SampleProxy test on a fresh proxy (constructor tests 339 → 344)
- Record STORE_XDUO_0002 as implemented (implementStep 5) in the contract registry

# Tests

TDD: red first (`FS0039` — the production symbol `SampleProxy.createInMemory`
did not exist), then the implementation, then green. All gates in the slice
roster pass in the worker's local (advisory) run; the arc-runner gate engine
re-runs them authoritatively after exit. Full logs in
`specs/0033/.artifacts/005-*.log`.

- `build` — solution builds Release/x64, 0 errors; no warnings from touched
  files (the 93 log warnings are the pre-existing CoreWCF.Primitives NU190x
  package-vulnerability advisories).
- `unit-tests` (BerremanTests) — 84 passed, 5 skipped (pre-existing skips),
  0 failed.
- `constructor-unit-tests` — 344 passed, 0 failed (5 round-trip tests added
  this round; every step-004 test kept, now running against the REAL store on
  a fresh proxy per test).
- `ui-smoke` — 54 passed, 0 failed.
- `ui-tests` — 249 passed, 0 failed.

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 344
  ui_smoke_tests:         54
  ui_tests:               249
```

# Architecture

- **The real store is the mock's shape, made stateful.** `createInMemory` is a
  `static member` on `SampleProxy` (an intrinsic type augmentation placed after
  `seedEntries`, which seeds it) — the module-level name `createInMemory` is
  the `LibraryProxy` builder. It captures a `ref Map<SampleId, Sample>` once
  at construction, per the repo's proxy convention ("constructed once, at the
  composition root, via a `static member create` that captures the real
  resources"); the record fields are the ONLY way to reach the cell, so logic
  holding the proxy stays pure and a test substitutes a stub of the same shape.
- **The elevated `SampleId` is the Map key directly** — no `.value` unwrap in
  the store; the Guid surfaces only inside error `reason` strings (the IO/log
  boundary).
- **In-memory ≠ Storage.** The slice's `touches` pins the implementation to
  `OpticalConstructor.Domain` (the step-004 SoW had forecast
  `OpticalConstructor.Storage`); a disk-backed store, if a later spec wants
  one, is another producer of the SAME record shape — callers never change.
- **Result order is Map (Guid) order.** The contract's `Sample list` carries
  no order guarantee; a panel wanting display order sorts explicitly.
- **Blank-name validation stays** (`validateSample`, shared by both writes):
  it is the only producer of `InvalidSample` and two step-004 tests pin it.

# Deferred

- IMPLEMENT_CONTRACT STORE_XDUO_0001 `MaterialProxy` — slice 006 (depends on
  this slice; same stateful-closure pattern, over the materials seam).
- Any disk-backed / persisting samples store (`OpticalConstructor.Storage`) —
  not in this arc's step list; the in-memory store is what steps 22/24/26
  wire against.
- Reconciling the samples write-seam with the read-only `LibraryProxy` sample
  listing (`SampleItem` entries) — a wiring concern for the later steps that
  consume both seams (unchanged from slice 004).
- Structure rotation in `sampleToSystem` — step 20 of this arc (unchanged
  from slices 001–004).

# Gotchas

- **A stateful proxy shared across tests is order-dependent.** The step-004
  tests held ONE module-level proxy; against the real store that breaks under
  xUnit's unordered execution — every `SampleProxyTests` test now builds a
  fresh `SampleProxy.createInMemory ()`. Keep it that way.
- `SampleProxy.createInMemory` must stay a TYPE AUGMENTATION below
  `seedEntries` — F# is order-dependent and the module-level `createInMemory`
  name is taken (`LibraryProxy`, the step-004 gotcha). The intrinsic extension
  reaches the module-private `validateSample` without issue.
- `createInMemorySampleProxy` is GONE (replaced, per the IMPLEMENT_CONTRACT
  obligation); its non-persisting "add returns Ok but the list never grows"
  behaviour was the declared-lifecycle placeholder, now genuinely fixed.
- `listSamples` no longer returns seed order (Map orders by Guid) — nothing
  asserts order today; don't start.
- The IMPLEMENT_CONTRACT worker updates the contract registry itself
  (`.contracts-json` → `lifecycle: implemented`, `implementStep: 5`), unlike
  ADD_CONTRACT where the supervisor pre-recorded the declaration. The file is
  CRLF (arc-runner-owned) — edited byte-exact, endings preserved.
- F# parses `Assert.False(p = q)` as a named-argument assignment (FS0505) —
  proxy reference-equality assertions need double parens:
  `Assert.False((p = q))` (same as slices 003/004).
- `specs/0033/.manifest.state.json` carries CRLF but is the arc-runner's own
  file — left alone (same as slices 001–004).

# Changelog

- 2026-07-05 — slice 005: SampleProxy IMPLEMENTED (STORE_XDUO_0002):
  SampleProxy.createInMemory closes over a ref Map<SampleId, Sample> seeded
  from seedEntries — writes persist inside the closure; the step-004
  validate-only mock replaced; +5 fixed-Guid round-trip tests, every
  SampleProxy test isolated on a fresh proxy; registry lifecycle →
  implemented; all suites green.
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
