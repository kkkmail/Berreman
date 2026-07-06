# State of the world — spec 0033, slice 010

# Where we are

Slice 010 is the fourth Part B (serializable dispersion) step of arc 0033: it
adds the serializable mu (Polder / gyromagnetic) tree to
`Berreman/Berreman/Dispersion.fs` — `GyrationAxis` (AlongZ = Faraday, the
default; transverse = Voigt), ONE generic `PolderValue<'g>` record reused by
both the constant and dispersive cases, `ConstantMuValue`
(ScalarMu | GyromagneticMu), and the two-case `MuWithDispValue` whose
`toMuWithDisp` assembles the Polder tensor rows [mu, +i·g, 0], [-i·g, mu, 0],
[0, 0, muParallel] permuted by axis through `Mu.create`. Unlike the rho tree
(slice 009), the assembly lives in the core: `Mu.create` and `MuValue` are in
`MaterialProperties`, so no `OpticalProperties` seam is needed. No solver work
— the Berreman matrix already reads off-diagonal mu. The engine unions and the
slice-007/008/009 blocks stay byte-identical; the diff is pure addition. Next
in Part B: the `DispersionModels.toEpsAxis` lowering (slices 011+).

# What's working

- Add GyrationAxis (AlongX | AlongY | AlongZ, static defaultValue = AlongZ — Faraday geometry; transverse = Voigt) to Dispersion.fs
- Add PolderValue<'g> (muDiagonal, muParallel, gyration, axis) with a map combinator — ONE generic Polder record reused by the constant ('g = MuValue) and dispersive ('g = DispersionFormula) cases
- Add private polderMu assembling rows [mu, +i·g, 0], [-i·g, mu, 0], [0, 0, muParallel] via Mu.create, cyclically permuted for AlongX / AlongY so the ±i·g pair stays right-handed about the magnetization axis
- Add ConstantMuValue (ScalarMu of MuValue → mu × identity | GyromagneticMu of PolderValue<MuValue>) and MuWithDispValue with toMuWithDisp : MuWithDisp — constant cases short-circuit to MuWithoutDisp, the dispersive case evaluates each formula at the wavelength and assembles per call
- Add 8 BerremanTests facts: all three axis permutations pinned against independent Mu.create literals (distinct mu/muParallel/g so a wrong permutation cannot alias), ScalarMu = scaled identity, structural short-circuit both ways, defaultValue = AlongZ, and a dispersive Polder value at 500 nm equalling the constant assembly with the same magnitudes (unit tests 111 → 119)

# Tests

TDD: red first (`FS0039` — the production types `GyrationAxis`, `PolderValue`,
`MuWithDispValue` did not exist; `010-red-tdd.log`), then the implementation,
then green. All gates in the slice roster pass in the worker's local
(ADVISORY) run; the arc-runner gate engine re-runs them authoritatively after
exit. Full logs in `specs/0033/.artifacts/010-*.log`.

- `build` — solution builds Release/x64, exit 0, 0 errors; no warnings from
  touched files (only pre-existing MSB3277/NU19xx advisories and the
  slice-007 FS3873).
- `unit-tests` (BerremanTests) — 119 passed, 5 skipped (pre-existing skips),
  0 failed (+8 over the 111 baseline).
- `constructor-unit-tests` — 350 passed, 0 failed (= baseline; no
  OpticalConstructor project touched).
- `ui-smoke` — 54 passed, 0 failed (= baseline).
- `ui-tests` — 249 passed, 0 failed (= baseline).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 350
  ui_smoke_tests:         54
  ui_tests:               249
```

# Architecture

- **ONE generic Polder record, not two** — `PolderValue<'g>` carries the
  physics shape (two transverse diagonal slots, one axis slot, one gyration
  magnitude, the axis); `'g = MuValue` is the constant case and
  `'g = DispersionFormula` the dispersive one, so the dispersive evaluation is
  one `map (evaluate w |> MuValue)` into the same assembly — the slice-009
  pattern.
- **`polderMu` is the single place the Polder tensor is written down** — a
  private core-side function `PolderValue<MuValue> -> Mu` through `Mu.create`;
  components stay elevated up to the assembly seam. The transverse axes are
  cyclic permutations of the Faraday form (off-diagonal
  `mu_jk = i·g·ε_jkl·n_l`), preserving the right-handed sense — AlongY gets
  `-i·g` at (1,3) and `+i·g` at (3,1), not a block copy.
- **Assembly lives in `Dispersion.fs`, not `OpticalProperties`** — unlike rho
  (whose crystal-class builders live in `Active.fs`), everything mu needs
  (`Mu.create`, `MuValue`) is in `MaterialProperties`, visible to the core;
  `toMuWithDisp` is a plain member, not a downstream type extension.
- **The mu tree is pure data; the engine func is BUILT, never stored** —
  `toMuWithDisp` short-circuits the constant cases to `MuWithoutDisp` and
  wraps a `WaveLength -> Mu` closure for the dispersive case; the engine
  unions stay unchanged and non-serializable.
- **The default is a named member** (`GyrationAxis.defaultValue = AlongZ`) so
  the later serialization/UI slices reference one home for the Faraday
  default instead of re-hardcoding it.

# Deferred

- `DispersionModels.toEpsAxis` lowering, `SumOfTerms`, `ForouhiBloomer` /
  `BrendelBormann`, and the removal of `AnisotropicModel` — slices 011–012.
- `MaterialComplexity` / `MaterialEntry.complexity` and the re-expressed
  built-ins — slice 013.
- Serialization (JSON) of the new types — the spec's storage seam; nothing in
  this arc's core steps serializes them yet.
- Wiring/UI (Parts C–F, incl. any gyromagnetic-mu editor) and RII import
  breadth (Part G) — later slices.

# Gotchas

- **Pipe-into-match precedence**: `match axis with … -> [rows] |> Mu.create`
  binds the pipe to the LAST branch only — bind the rows with a `let`, pipe
  once after the match (as `polderMu` does).
- **`PolderValue` shares the `gyration` field label with
  `GyrotropicValue<'g>`** (slice 009): a partial literal infers the wrong
  record — annotate construction sites (the tests do); all four PolderValue
  fields together are unique to it.
- **AlongY signs are the cyclic permutation, not a copy** — `-i·g` sits at
  (1,3) and `+i·g` at (3,1); swapping them silently flips the precession
  sense and no diagonal entry would catch it (the tests pin distinct
  magnitudes AND the off-diagonal signs per axis).
- **Negative literals after a function value need parens** (`im (-gyr)`;
  `im -gyr` parses as subtraction).
- `.manifest.state.json` shows as modified in `git status` — the arc-runner's
  own file (same as slices 001–009), left alone.

# Changelog

- 2026-07-05 — slice 010: serializable mu (Polder / gyromagnetic) tree
  (spec 0033 Part B): GyrationAxis (AlongX | AlongY | AlongZ,
  defaultValue = AlongZ Faraday; transverse = Voigt), ONE generic
  PolderValue<'g> (muDiagonal / muParallel / gyration / axis, map),
  private polderMu (rows [mu, +i·g, 0], [-i·g, mu, 0], [0, 0, muParallel]
  via Mu.create, cyclic permutations per axis), ConstantMuValue
  (ScalarMu → mu × identity | GyromagneticMu), MuWithDispValue.toMuWithDisp
  (constant short-circuits to MuWithoutDisp, dispersive evaluates per call) —
  all appended to Berreman/Berreman/Dispersion.fs, engine unions
  byte-identical; +8 tests (axis permutations pinned, scalar identity,
  short-circuit shape, default axis, dispersive = constant assembly at
  500 nm); BerremanTests 111 → 119; all suites green.
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
