# State of the world — spec 0033, slice 006

# Where we are

Slice 006 is the second IMPLEMENT_CONTRACT step of arc 0033: it takes the
materials write-seam `MaterialProxy` (contract STORE_XDUO_0001, declared by
slice 003) to `implemented` lifecycle. The step-003 validate-only mock is
REPLACED by the real, stateful in-memory store `MaterialProxy.createInMemory`
taking `samplesReferencing : MaterialId -> Sample list` and closing over a
`ref Map<MaterialId, MaterialEntry>` seeded from
`MaterialLibrary.builtInEntries`. Because the referencing lookup is
`Sample`-typed and `Sample` compiles after `MaterialLibrary.fs`, the store
lands in `ElementId.fs` as a type augmentation (the step-005 pattern); the
pure pieces (`byQuery`, `validateEntry`) stay in `MaterialLibrary.fs`. At
composition the lookup is the new `samplesReferencing`, backed by the LIVE
step-005 `SampleProxy` store — `removeMaterial` on a referenced material
hard-blocks with `MaterialStillReferenced` naming the referencing samples,
never cascades, never silently deletes. Both store contracts of the arc are
now implemented; next are the wiring/UI steps that consume them.

# What's working

- Implement MaterialProxy.createInMemory (STORE_XDUO_0001 → implemented): a stateful in-memory store over a ref Map<MaterialId, MaterialEntry> seeded from builtInEntries, mutation confined to the closure
- removeMaterial consults the samplesReferencing lookup: any referencing sample ⇒ Error MaterialStillReferenced naming the samples, store unchanged — never cascades, never silently deletes; unreferenced removal succeeds
- addMaterial hard-blocks a held id (DuplicateMaterialId) and persists otherwise; updateMaterial replaces/rejects-unknown; blank names stay InvalidMaterial
- Add the pure byQuery search seam (byNameContains → byCategory → DispersionFilter facet) and SampleStructure.referencedMaterials (films incl. Repeated cells, substrate plate, lower half-space); compose via samplesReferencing over the step-005 SampleProxy store
- Rewrite MaterialProxyTests for the stateful store: round-trips, stateful duplicate-add, the referenced-block acceptance incl. the live-unblock proof (constructor tests 344 → 350)
- Record STORE_XDUO_0001 as implemented (implementStep 6) in the contract registry

# Tests

TDD: red first (`FS0039` — the production symbols `MaterialProxy.createInMemory`
and `SampleStructure.referencedMaterials` did not exist), then the
implementation, then green. All gates in the slice roster pass in the worker's
local (advisory) run; the arc-runner gate engine re-runs them authoritatively
after exit. Full logs in `specs/0033/.artifacts/006-*.log`.

- `build` — solution builds Release/x64, 0 errors; no warnings from touched
  files (the log's warnings are the pre-existing NU190x package advisories,
  MSB3277, FS1125/FS0044 in untouched tests/TestWindows, and SYSLIB0051 in the
  vendored MathNetNumerics).
- `unit-tests` (BerremanTests) — 84 passed, 5 skipped (pre-existing skips),
  0 failed.
- `constructor-unit-tests` — 350 passed, 0 failed (+6 this round; every
  step-003 read/stub expectation kept, now running against the REAL store on
  a fresh proxy per test).
- `ui-smoke` — 54 passed, 0 failed.
- `ui-tests` — 249 passed, 0 failed.

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 350
  ui_smoke_tests:         54
  ui_tests:               249
```

# Architecture

- **The store is a cross-file type augmentation.** The pinned
  `samplesReferencing : MaterialId -> Sample list` parameter forces the
  implementation into `ElementId.fs` (where `Sample` lives), so
  `MaterialProxy.createInMemory` is an OPTIONAL extension of the type declared
  in `MaterialLibrary.fs` — unlike step 005's intrinsic one. Consequences:
  call sites open `OpticalConstructor.Domain.Library`, and the extension
  cannot reach `MaterialLibrary`-private bindings (`validateEntry` is now
  public; `hasDispersion` stays private behind `byQuery`).
- **One pure search seam.** `byQuery` in `MaterialLibrary.fs` composes the
  §D.8 `byNameContains` / `byCategory` filters with the `DispersionFilter`
  facet; the store answers `searchMaterials` by applying it to its current
  entries. Any future disk-backed producer reuses the same seam.
- **Referencing is a lookup the store RECEIVES, not owns.** The store never
  sees the samples collection — it takes `MaterialId -> Sample list` and the
  composition root decides what backs it. `samplesReferencing (samples :
  SampleProxy)` is the canonical backing: it filters `listSamples ()` by the
  pure `SampleStructure.referencedMaterials` on every call, so the check is
  LIVE (remove the referencing sample and the material becomes removable).
- **The elevated `MaterialId` is the Map key directly** — no `.value` unwrap
  in the store; the Guid surfaces only inside error `reason` strings (the
  IO/log boundary). Result order is Map (Guid) order; no order guarantee.
- **In-memory ≠ Storage** (unchanged from 005): the slice's `touches` pins
  Domain + Tests; a disk-backed store is another producer of the SAME record
  shape — callers never change.

# Deferred

- Any disk-backed / persisting materials store (`OpticalConstructor.Storage`)
  — not in this arc's step list; the in-memory store is what the later wiring
  steps consume.
- Wiring the composed MaterialProxy/SampleProxy pair into a host composition
  root — the UI steps (15/16/22/23/24/26) own that; today the only composer
  is the test suite.
- Reconciling the samples write-seam with the read-only `LibraryProxy` sample
  listing (`SampleItem` entries) — a wiring concern for the later steps that
  consume both seams (unchanged from slices 004/005).
- Structure rotation in `sampleToSystem` — step 20 of this arc (unchanged
  from slices 001–005).

# Gotchas

- The slice's `MaterialLibrary.fs:53,57,79` citations predate steps 001–003
  (today: 108, 112, 141) — the named symbols are unambiguous; cosmetic drift.
- `MaterialProxy.createInMemory` is an OPTIONAL type extension (the type
  lives in `MaterialLibrary.fs`, the member in `ElementId.fs`): it resolves
  only where `OpticalConstructor.Domain.Library` is open, and it cannot reach
  module-private bindings of `MaterialLibrary` — that is WHY `validateEntry`
  is public and `byQuery` exists.
- `samplesReferencing`'s pinned signature carries no error channel; the
  in-memory `listSamples` is total (always `Ok`), so the degenerate
  `Error → []` branch is unreachable today. A future store whose listing can
  fail must supply its own conservative lookup — documented on the function.
- Dispersive classification kept from step 003 (any of eps/mu/rho func case),
  a compatible superset of the slice's "epsWithDisp is the func case" —
  identical on every built-in (silicon and langasite carry the eps func case).
- glass200 is the ONLY built-in no seeded sample references; the
  unreferenced-removal acceptance leg depends on it.
- A stateful proxy shared across tests is order-dependent — every
  `MaterialProxyTests` test builds a fresh proxy (the step-005 rule); keep it
  that way.
- The step-003 mock `createInMemory` (module-level in `MaterialLibrary`) is
  GONE (replaced, per the IMPLEMENT_CONTRACT obligation); its non-persisting
  writes were the declared-lifecycle placeholder. Unqualified `createInMemory`
  now always means the `LibraryProxy` builder in `Library`.
- `specs/0033/.manifest.state.json` carries CRLF but is the arc-runner's own
  file — left alone (same as slices 001–005); `.contracts-json` edited with
  CRLF preserved (byte-checked).

# Changelog

- 2026-07-05 — slice 006: MaterialProxy IMPLEMENTED (STORE_XDUO_0001):
  MaterialProxy.createInMemory (samplesReferencing) closes over a ref
  Map<MaterialId, MaterialEntry> seeded from builtInEntries — writes persist;
  removeMaterial hard-blocks referenced materials naming the referencing
  samples via the live SampleProxy-backed samplesReferencing lookup; pure
  byQuery search seam + SampleStructure.referencedMaterials added; the
  step-003 mock replaced; +6 tests (350 constructor tests); registry
  lifecycle → implemented; all suites green.
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
