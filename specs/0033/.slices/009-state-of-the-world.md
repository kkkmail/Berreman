# State of the world — spec 0033, slice 009

# Where we are

Slice 009 is the third Part B (serializable dispersion) step of arc 0033: it
adds the serializable rho (gyration) tree to `Berreman/Berreman/Dispersion.fs`
— `Handedness`, the five named generic gyration records, the seven-case
`GyrationClass<'g>` (rotation-producing symmetry classes only, no free 3x3),
`GyrotropicValue<'g>`, and the two-case `RhoWithDispValue` — and, in
`Berreman/OpticalProperties/Active.fs` (because the core cannot reference
`OpticalProperties`), the four missing crystal-class `Rho` builders (222,
monoclinic-2, monoclinic-m, triclinic-1) plus the
`RhoWithDispValue.toRhoWithDisp` type extension that assembles the imaginary
gyration matrix through the crystal-class builders. The engine unions and the
slice-007/008 blocks stay byte-identical; both diffs are pure addition. Next
in Part B: the mu `MuWithDispValue` / `PolderValue` tree and the
`DispersionModels.toEpsAxis` lowering (slices 010+).

# What's working

- Add Handedness (LeftHanded | RightHanded, sign = ∓1: the enantiomorph is one overall sign flip of g), the five NAMED generic gyration records (Uniaxial g11/g33; Orthorhombic222 g11/g22/g33; Monoclinic2 g11/g22/g33/g13; MonoclinicM g12/g23; Triclinic1 all six) each with a map combinator — never anonymous tuples
- Add GyrationClass<'g> (CubicActive | UniaxialActive | PlanarActive | Orthorhombic222 | Monoclinic2 | MonoclinicM | Triclinic1 over those records — rotation-producing classes only, no free 3x3), GyrotropicValue<'g> (gyration + hand), and RhoWithDispValue (GyrotropicValue<DispersionFormula> | GyrotropicValue<RhoValue>) to Dispersion.fs
- Add Rho.type_222_Crystal / type_2_Crystal / type_m_Crystal / type_1_Crystal builders (all via Rho.fromIm) to Active.fs, next to the existing cubic/planar/uniaxial builders
- Add RhoWithDispValue.toRhoWithDisp as an Active.fs type extension: handedness sign applied per component, CubicActive → cubicCrystal, UniaxialActive → the diagonal type_3_4_6_Crystal (NOT type_32_42_62_Crystal), PlanarActive → planarCrystal, the four new classes → the new builders; the dispersive case evaluates each component's DispersionFormula at the wavelength, the constant case short-circuits to RhoWithoutDisp
- Add 11 BerremanTests facts: the quartz class-32 example (g11 +5.9e-5, g33 −10.1e-5) assembles diag(g11, g11, g33); every GyrationClass case pinned against an independently-built Rho.fromIm literal; LeftHanded negates the quartz and full-triclinic tensors; the dispersive path evaluates formulas at the wavelength and respects handedness (unit tests 100 → 111)

# Tests

TDD: red first (`FS0039` — the production types `RhoWithDispValue`,
`Handedness`, `GyrationClass` and the gyration records did not exist;
`009-red-tdd.log`), then the implementation, then green. All gates in the
slice roster pass in the worker's local (ADVISORY) run; the arc-runner gate
engine re-runs them authoritatively after exit. Full logs in
`specs/0033/.artifacts/009-*.log`.

- `build` — solution builds Release/x64, exit 0, 0 occurrences of "error" in
  the log; no warnings from touched files (only pre-existing NU19xx package
  advisories).
- `unit-tests` (BerremanTests) — 111 passed, 5 skipped (pre-existing skips),
  0 failed (+11 over the 100 baseline).
- `constructor-unit-tests` — 350 passed, 0 failed (= baseline; no
  OpticalConstructor project touched).
- `ui-smoke` — 54 passed, 0 failed (= baseline).
- `ui-tests` — 249 passed, 0 failed (= baseline).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    111
  constructor_unit_tests: 350
  ui_smoke_tests:         54
  ui_tests:               249
```

# Architecture

- **The rho tree is pure data; the engine func is BUILT, never stored.**
  `RhoWithDispValue.toRhoWithDisp` is the single seam onto the engine's
  `RhoWithDisp`: the constant case short-circuits to `RhoWithoutDisp`; the
  dispersive case wraps a `WaveLength -> Rho` closure that evaluates each
  component's `DispersionFormula` and assembles per call — mirroring the
  slice-008 eps tree exactly.
- **Symmetry classes instead of a free 3x3**: `GyrationClass<'g>` admits ONLY
  the rotation-producing point-group forms, each multi-component case carrying
  a NAMED record — a serializer and an editor pattern-match the physics, and an
  unconstrained tensor is unrepresentable.
- **One generic component parameter `'g`** (`RhoValue` constant,
  `DispersionFormula` dispersive) with `map` combinators on the records and the
  class — the dispersive evaluation is one `map`, not a second copy of the
  assembly.
- **Handedness is a core-side two-case DU with a `sign` member**; the sign is
  applied per component BEFORE assembly (`Rho` has no scalar multiplication),
  which equals one overall flip of the assembled tensor.
- **Assembly lives in `OpticalProperties/Active.fs`** as a private
  `assembleRho` + the `toRhoWithDisp` type extension — the core project cannot
  reference `OpticalProperties`, and the crystal-class builders (`cubicCrystal`,
  `planarCrystal`, `type_3_4_6_Crystal`, and the four new `type_222 / type_2 /
  type_m / type_1` builders) stay the only place gyration matrices are written
  down.

# Deferred

- `MuWithDispValue` / `PolderValue<'g>` — the remaining Part B value tree
  (later slice).
- `DispersionModels.toEpsAxis` lowering, `SumOfTerms`, `ForouhiBloomer` /
  `BrendelBormann`, and the removal of `AnisotropicModel` — slices 011–012.
- `MaterialComplexity` / `MaterialEntry.complexity` and the re-expressed
  built-ins — slice 013.
- Serialization (JSON) of the new types — the spec's storage seam; nothing in
  this arc's core steps serializes them yet.
- Wiring/UI (Parts C–F, incl. the symmetry-class-only gyration editor) and RII
  import breadth (Part G) — later slices.

# Gotchas

- **The gyration records share field labels** (g11/g22/g33/g23/g13/g12
  subsets): a bare `{ g11 = …; g33 = … }` literal infers the LAST-declared
  record with those labels (`Triclinic1Gyration`) and fails on missing fields —
  annotate every construction site, as the tests do.
- **`planarCrystal` is antisymmetric, the four new builders are symmetric** —
  the point-group forms differ; do not harmonize them.
- **UniaxialActive must route through `type_3_4_6_Crystal`** —
  `type_32_42_62_Crystal` needs a g12 the two-component uniaxial record cannot
  supply.
- **`Active.fs` / `MatrixComparison.fs` sat CRLF in the working copy before
  this round** (index LF; `git status` hides it, `git ls-files --eol` shows
  it) — converted back to whole-file LF with BOMs preserved; zero diff noise.
- **Negative literals after a constructor need parens** (`RhoValue (-10.1e-5)`).
- `.manifest.state.json` shows as modified in `git status` — the arc-runner's
  own file (same as slices 001–008), left alone.

# Changelog

- 2026-07-05 — slice 009: serializable rho (gyration) tree (spec 0033 Part B):
  Handedness (enantiomorph = one overall sign flip), five named generic
  gyration records + GyrationClass<'g> (rotation-producing classes only) +
  GyrotropicValue<'g> + two-case RhoWithDispValue in Dispersion.fs; four new
  crystal-class Rho builders (222 / monoclinic-2 / monoclinic-m / triclinic-1,
  all Rho.fromIm) and the toRhoWithDisp type extension (per-component sign,
  UniaxialActive → diagonal type_3_4_6_Crystal, dispersive case evaluates
  formulas at the wavelength) in OpticalProperties/Active.fs; +11 tests
  (quartz class-32 diag(+5.9e-5, +5.9e-5, −10.1e-5), every class pinned,
  handedness negation, dispersive evaluation); BerremanTests 100 → 111; all
  suites green.
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
