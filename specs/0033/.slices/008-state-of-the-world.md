# State of the world — spec 0033, slice 008

# Where we are

Slice 008 is the second Part B (serializable dispersion) step of arc 0033: it
adds the serializable eps tree to `Berreman/Berreman/Dispersion.fs`, consuming
the slice-007 formula blocks (`WaveLengthInterval`, `DispersionFormula`,
`ComplexDispersionFormula`) and BUILDING the engine's `EpsWithDisp` through the
existing `Eps` constructors — `ConstantEpsValue` (six descriptive constant
cases), `EpsAxisDispersion` (per-axis RealNK / ComplexEps), the three segment
records, `EpsDispersiveValue`, and the two-case `EpsWithDispValue` with
`toEpsWithDisp`. The engine unions stay byte-identical (§0.1); the diff is pure
addition. Next in Part B: the rho/mu `…Value` trees (gyration classes, Polder)
and the `DispersionModels.toEpsAxis` lowering (slice 011+) that feed this tree
from the editor-facing model catalogue.

# What's working

- Add ConstantEpsValue — six descriptive constant-eps cases (isotropic/uniaxial/biaxial × transparent/absorbing, named union fields, never a bare Eps) with toEps building through Eps.fromRefractionIndex / Eps.fromComplexRefractionIndex; uniaxial maps to the (n_o, n_e, n_o) triple (the epsLa3Ga5SiO14 precedent)
- Add EpsAxisDispersion (RealNK of n/k DispersionFormula | ComplexEps of ComplexDispersionFormula) with complexIndex : WaveLength -> ComplexRefractionIndex (RealNK → n + i·k; ComplexEps → sqrt eps)
- Add IsotropicEpsSegment / UniaxialEpsSegment / BiaxialEpsSegment — one shared wavelengthInterval per segment plus one EpsAxisDispersion per axis — and EpsDispersiveValue over homogeneous segment lists with getEps : WaveLength -> Eps
- Add EpsWithDispValue (EpsWithDispValue of EpsDispersiveValue | EpsWithoutDispValue of ConstantEpsValue) with toEpsWithDisp : EpsWithDisp — constant case short-circuits to EpsWithoutDisp; dispersive case selects the FIRST covering segment, topmost extrapolates out-of-range, no clamps, no validation (§0.4)
- Add 11 BerremanTests facts: all six constant cases equal the directly-constructed engine Eps, segment selection pinned (overlap → first; later-only → later; out-of-range → topmost extrapolates, distinguished from clamping), uniaxial and biaxial dispersive closures equal per-axis analytic values incl. the ComplexEps square-root path (unit tests 89 → 100)

# Tests

TDD: red first (`FS0039` — the production types `EpsWithDispValue`,
`IsotropicEpsSegment` and siblings did not exist; `008-red-tdd.log`), then the
implementation, then green. All gates in the slice roster pass in the worker's
local (ADVISORY) run; the arc-runner gate engine re-runs them authoritatively
after exit. Full logs in `specs/0033/.artifacts/008-*.log`.

- `build` — solution builds Release/x64, `Build succeeded.`, 0 occurrences of
  "error" in the log; no warnings from touched files.
- `unit-tests` (BerremanTests) — 100 passed, 5 skipped (pre-existing skips),
  0 failed (+11 over the 89 baseline).
- `constructor-unit-tests` — 350 passed, 0 failed (= baseline; no
  OpticalConstructor project touched).
- `ui-smoke` — 54 passed, 0 failed (= baseline).
- `ui-tests` — 249 passed, 0 failed (= baseline).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    100
  constructor_unit_tests: 350
  ui_smoke_tests:         54
  ui_tests:               249
```

# Architecture

- **The eps tree is pure data; the engine func is BUILT, never stored.**
  `EpsWithDispValue.toEpsWithDisp` is the single seam onto the engine's
  `EpsWithDisp` (§0.1): the constant case short-circuits to `EpsWithoutDisp`
  via `toEps`; the dispersive case wraps `EpsDispersiveValue.getEps` in the
  `WaveLength -> Eps` closure.
- **Descriptive constant cases instead of a bare `Eps`**: the six-case
  `ConstantEpsValue` says what the medium IS (symmetry × transparency), so a
  serializer and an editor can pattern-match the physics; assembly reuses the
  existing engine constructors only.
- **Per-axis dispersion is a two-case DU** — `RealNK` keeps transparent and
  tabulated-nk media in real formulas (k = the zero formula, an empty term
  list); `ComplexEps` exists ONLY for inherently complex models and takes the
  principal square root of ε.
- **Segments are homogeneous per symmetry** (`IsotropicDispersive` /
  `UniaxialDispersive` / `BiaxialDispersive` of the matching segment lists) and
  one `wavelengthInterval` is SHARED across a segment's axes — an axis can
  never disagree with its siblings about coverage.
- **Selection semantics live in one private `selectSegment`**: FIRST covering
  segment wins (inclusive endpoints, compared on `WaveLength.value`), topmost
  extrapolates when none covers — no clamps, no validation (§0.4); the
  uniaxial diagonal assembly is (n_o, n_e, n_o).

# Deferred

- `RhoWithDispValue` / `GyrationClass` / `GyrotropicValue` and
  `MuWithDispValue` / `PolderValue` — the remaining Part B value trees (later
  slices; rho's `toRhoWithDisp` lives in `OpticalProperties/Active.fs`).
- `Active.fs` 222 / monoclinic / triclinic builders — Part B, `OpticalProperties`.
- `DispersionModels.toEpsAxis` lowering, `SumOfTerms`, `ForouhiBloomer` /
  `BrendelBormann`, and the removal of `AnisotropicModel` — slices 011–012.
- `MaterialComplexity` / `MaterialEntry.complexity` and the re-expressed
  built-ins — slice 013.
- Serialization (JSON) of the new types — the spec's storage seam; nothing in
  this arc's core steps serializes them yet.
- Wiring/UI (Parts C–F) and RII import breadth (Part G) — later slices.

# Gotchas

- **Two uniaxial-diagonal conventions coexist in the repo**: this tree (and the
  slice) use (n_o, n_e, n_o) per the langasite precedent
  (`OpticalProperties/Dispersive.fs:52-55`), while `Active.fs:21 planarCrystal`
  builds (n11, n11, n33). Slice 013's built-in re-expression must compare
  against the tree's convention.
- **`selectSegment` throws on an empty segment list** (`List.head`) — §0.4
  forbids validation and the closure must return an `Eps`; an empty
  `EpsDispersiveValue` is a construction-site bug, not something to default.
- **Interval coverage is INCLUSIVE and compares `WaveLength.value`** — the
  `WaveLength` DU's structural comparison orders by case tag (Mkm before Nm)
  and must never be used for coverage.
- **Segment records share the `wavelengthInterval` field name** — annotate
  bindings (the last-declared record wins bare-literal inference); this is why
  `selectSegment` takes the segment list BEFORE the interval projection.
- The engine `…WithDisp` unions and slice-007 formula blocks are byte-identical
  (§0.1) — the `Dispersion.fs` diff is pure addition (115 added / 0 deleted),
  no new `open`s.
- `.manifest.state.json` shows as modified in `git status` — the arc-runner's
  own file (same as slices 001–007), left alone.

# Changelog

- 2026-07-05 — slice 008: serializable eps tree (spec 0033 Part B):
  ConstantEpsValue (six descriptive constant cases, toEps via the engine
  constructors, uniaxial → (n_o, n_e, n_o)), EpsAxisDispersion
  (RealNK | ComplexEps with complexIndex), the three shared-interval segment
  records, EpsDispersiveValue.getEps (first covering segment wins, topmost
  extrapolates), and EpsWithDispValue.toEpsWithDisp building the engine's
  EpsWithDisp — appended to Berreman/Berreman/Dispersion.fs, engine unions
  byte-identical; +11 tests (six constant-case equalities, three
  segment-selection facts, uniaxial/biaxial per-axis analytic closures);
  BerremanTests 89 → 100; all suites green.
- 2026-07-05 — slice 007: serializable dispersion formula blocks
  (spec 0033 Part B opener): WaveLengthInterval, DispersionTerm,
  DispersionFormula.evaluate (canonical-wavelength reduction + term sum), and
  the Complex mirrors added to Berreman/Berreman/Dispersion.fs — pure
  data + evaluation, engine unions byte-identical; +5 hand-computed-value
  tests (Cauchy Laurent, Sellmeier inverse via exact partial fraction,
  shifted-centre power, complex Lorentz pole, interval endpoints) under
  MatrixComparison.allowedDiff; BerremanTests 84 → 89; all suites green.
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
