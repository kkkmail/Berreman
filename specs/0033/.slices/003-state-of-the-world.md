# State of the world — spec 0033, slice 003

# Where we are

Slice 003 is the first ADD_CONTRACT step of arc 0033: it DECLARES the mutating
materials write-seam `MaterialProxy` (contract STORE_XDUO_0001, kind proxy) in
`OpticalConstructor.Domain` beside `MaterialLibrary`, on top of slice 002's
elevated `MaterialId`. The surface — `DispersionFilter`, `MaterialQuery`, the
extended `MaterialError`, and the six-function `[<ReferenceEquality>]` proxy
record — is pinned by an in-memory mock and a mock/stub-driven test suite. The
contract stays at `declared` lifecycle: no persisting store was built — that is
the later IMPLEMENT_CONTRACT STORE_XDUO_0001 step (in
`OpticalConstructor.Storage`), which slots in behind this exact shape leaving
callers unchanged.

# What's working

- Declare MaterialProxy (STORE_XDUO_0001): listMaterials / searchMaterials / tryGetMaterial / addMaterial / updateMaterial / removeMaterial, all Result-returning, per the LibraryProxy functional-proxy convention
- Add DispersionFilter (AnyDispersion | OnlyDispersive | OnlyNonDispersive) and the MaterialQuery record (text, category option, dispersion) with a match-everything MaterialQuery.empty
- Extend MaterialError with DuplicateMaterialId, MaterialStillReferenced, InvalidMaterial — every case carries reason : string
- Ship the in-memory mock createInMemory: an inline stub over the FIXED built-in entry list — reads answer from the list, writes validate and return the typed outcome without persisting
- Add MaterialProxyTests (+15): mock-driven coverage of every field plus the acceptance stub exercising all six functions through their exact signatures
- Keep the TestWindows materialErrorText match exhaustive over the extended MaterialError (FS0025 under --warnaserror+:25)

# Tests

All gates in the slice roster pass in the worker's local (advisory) run; the
arc-runner gate engine re-runs them authoritatively after exit.

- `build` — solution builds Release/x64, 0 errors (`--warnaserror+:25` clean,
  no warnings from touched files).
- `unit-tests` (BerremanTests) — 84 passed, 5 skipped (pre-existing skips), 0 failed.
- `constructor-unit-tests` — 325 passed, 0 failed (15 tests added this round:
  the MaterialProxy mock read/write surface, the search facets, the typed
  error cases, the reference-equality pin, and the six-function stub
  acceptance test; none removed).
- `ui-smoke` — 54 passed, 0 failed.
- `ui-tests` — 249 passed, 0 failed.

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 325
  ui_smoke_tests:         54
  ui_tests:               249
```

# Architecture

- **The write-seam is a functional proxy at `declared` lifecycle.**
  `MaterialProxy` follows `LibraryProxy` / `ExperimentProxy` exactly: a
  `[<ReferenceEquality>]` record of pre-curried, `Result`-returning camelCase
  functions, so an Elmish host model holding one keeps its equality and a test
  substitutes a stub of the same shape. The real store (IMPLEMENT_CONTRACT,
  `OpticalConstructor.Storage`) will be a `create` that captures actual
  persistence into the same six fields.
- **Search input is DATA.** `MaterialQuery` (text fragment + category option +
  dispersion facet) makes `searchMaterials` the single search seam; the mock
  composes the existing §D.8 linear filters (`byNameContains`, category
  filter) rather than re-deriving search logic.
- **Dispersion classification is a pattern match, not a flag.** An entry is
  dispersive iff any of its `eps/mu/rhoWithDisp` components carries the
  function case — computed by a private helper; `DispersionFilter` is a
  three-case DU (never `bool option`) so a future facet is non-breaking.
- **Errors carry diagnosis.** All four `MaterialError` cases are
  `of reason : string`; the mock's reasons name the offending Guid string
  form, matching the slice-002 `UnknownMaterialId` precedent.

# Deferred

- The real, persisting `MaterialProxy` implementation (mutable store,
  reference tracking for `MaterialStillReferenced`, persistence) — the later
  IMPLEMENT_CONTRACT STORE_XDUO_0001 step; the contract registry stays
  `declared` (supervisor-maintained, already recorded for step 3).
- `MaterialStillReferenced` has no domain-mock producer: the mock cannot see
  the seeded samples' layer references (compile order — `SampleStructure`
  lives in `ElementId.fs`, after `MaterialLibrary.fs`); the case's shape is
  pinned through the test stub until the real store tracks references.
- Structure rotation in `sampleToSystem` — step 20 of this arc (unchanged
  from slices 001/002).

# Gotchas

- `materialErrorText` in `OpticalConstructor.TestWindows` (outside the
  declared `touches`) was an exhaustive single-case match on `MaterialError`;
  extending the DU made it incomplete — a build ERROR under
  `--warnaserror+:25` (FS0025). It now renders the three write-seam cases via
  an or-pattern over their `reason`.
- The mock's write functions VALIDATE but never persist — `addMaterial` on a
  fresh id returns `Ok ()` yet a following `listMaterials` still returns the
  fixed built-in list. That is the intended `declared`-lifecycle stub
  behaviour, not a bug; do not "fix" it before IMPLEMENT_CONTRACT.
- F# parses `Assert.False(p = q)` as a named-argument assignment (FS0505) —
  proxy reference-equality assertions need double parens:
  `Assert.False((p = q))`.
- `specs/0033/.manifest.state.json` carries CRLF but is the arc-runner's own
  file — left alone (same as slices 001/002).

# Changelog

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
