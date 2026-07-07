# State of the world — spec 0033, slice 007

# Where we are

Slice 007 opens Part B (serializable dispersion) of arc 0033: it adds the
generic serializable formula blocks to `Berreman/Berreman/Dispersion.fs` —
`WaveLengthInterval`, `DispersionTerm`, `DispersionFormula` (with
`evaluate : WaveLength -> double`), and the `ComplexDispersionTerm` /
`ComplexDispersionFormula` mirrors over `System.Numerics.Complex` — pure data
plus evaluation, from which later Part B steps' `…Value` types will BUILD the
engine's `…WithDisp` funcs. The engine unions in the file are byte-identical
(binding constraint §0.1); the diff is pure addition. Parts A's proxies
(slices 001–006) are unaffected; next come the eps/rho/mu `…Value` trees that
consume these blocks.

# What's working

- Add WaveLengthInterval (elevated WaveLength endpoints, pure data — no validation/clamping per §0.4)
- Add DispersionTerm (lambda/coefficients/power/multiplier) evaluating multiplier·(Σ coefficients[k]·(x−lambda)^k)^power — Horner inner sum, negative powers give the inverse/Laurent shapes
- Add DispersionFormula (terms + wavelengthScale = metres per coefficient unit) with evaluate : WaveLength -> double reducing the canonical wavelength to the coefficient unit and summing the terms
- Add ComplexDispersionTerm / ComplexDispersionFormula mirroring the real blocks over System.Numerics.Complex (private complexPown — pown does not resolve on Complex)
- Add 5 BerremanTests facts pinning Cauchy-shaped Laurent, Sellmeier-shaped inverse (via its exact partial fraction), shifted-centre power, the complex Lorentz-shaped mirror, and the interval — all against inline hand-computed values within MatrixComparison.allowedDiff (unit tests 84 → 89)

# Tests

TDD: red first (`FS0039` — the production types `DispersionFormula` and
siblings did not exist; `007-red-tdd.log`), then the implementation, then
green. All gates in the slice roster pass in the worker's local (ADVISORY)
run; the arc-runner gate engine re-runs them authoritatively after exit. Full
logs in `specs/0033/.artifacts/007-*.log`.

- `build` — solution builds Release/x64, `Build succeeded.`, 0 occurrences of
  "error" in the log; no warnings from touched files.
- `unit-tests` (BerremanTests) — 89 passed, 5 skipped (pre-existing skips),
  0 failed (+5 over the 84 baseline).
- `constructor-unit-tests` — 350 passed, 0 failed (= baseline; no
  OpticalConstructor project touched).
- `ui-smoke` — 54 passed, 0 failed (= baseline).
- `ui-tests` — 249 passed, 0 failed (= baseline).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    89
  constructor_unit_tests: 350
  ui_smoke_tests:         54
  ui_tests:               249
```

# Architecture

- **Formula blocks are pure data + evaluation, appended AFTER the engine
  unions** in the same `Dispersion` module — the unions stay unchanged and
  non-serializable (§0.1); later `…Value` types build `(WaveLength -> …)`
  closures FROM this data, never the other way round.
- **One reduction convention.** `wavelengthScale` is METRES PER COEFFICIENT
  UNIT; `DispersionFormula.evaluate` computes
  `x = WaveLength.value / (wavelengthScale · 1.0<meter>)` (the Dispersive.fs
  precedent for stripping the measure) and every term sees the same reduced x.
  Terms are unit-blind; only the formula owns the unit.
- **The term grammar is one shape**: `multiplier · (polynomial in (x − lambda))
  ^ integer power`. Constants (power 1), Cauchy/Laurent (negative power over a
  monomial), and Sellmeier resonances (partial-fraction inverse) are all
  instances — the raw `SumOfTerms` escape hatch of the later
  `DispersionModels` lowering maps 1:1 onto term lists.
- **Complex mirrors are field-for-field** (`lambda`/`coefficients`/`multiplier`
  → `Complex`; `power` and `wavelengthScale` stay real) and exist ONLY for
  inherently complex models (Lorentz/Drude ε); real formulas stay real.
- **No validation anywhere in the blocks** (§0.4): intervals don't order-check,
  poles evaluate to infinity, overlap/extrapolation semantics belong to the
  consuming segment logic of later Part B steps.

# Deferred

- The eps tree (`ConstantEpsValue`, `EpsAxisDispersion`, `EpsWithDispValue`),
  `RhoWithDispValue` / `GyrationClass` and `MuWithDispValue` / `PolderValue` —
  the later Part B steps that consume these blocks.
- `Active.fs` 222/monoclinic/triclinic builders and
  `RhoWithDispValue.toRhoWithDisp` — Part B, lives in `OpticalProperties`.
- `DispersionModels.toEpsAxis` lowering, `ForouhiBloomer` / `BrendelBormann` /
  `SumOfTerms` — Part B domain steps.
- Serialization (JSON) of the new blocks — the spec's storage seam; nothing in
  this arc's core steps serializes them yet.
- Wiring/UI (Parts C–F) and RII import breadth (Part G) — later slices.

# Gotchas

- `pown` does NOT resolve against `System.Numerics.Complex` (fsi-verified
  FS0001) — the complex mirror uses the private `complexPown`
  (repeated-multiplication fold, inverts on negative exponents) and an explicit
  `List.fold … Complex.Zero` instead of `sumBy`. Don't "simplify" them back.
- `DispersionTerm` / `ComplexDispersionTerm` (and the two formula records)
  share identical field NAMES — a bare record literal infers the last-declared
  type; annotate the binding (`let f : DispersionFormula = …`) as the tests do.
- `wavelengthScale` DIVIDES the metre-valued canonical wavelength (e.g.
  `1.0e-6` for µm coefficients); inverting it is silent — the tests pin the
  direction (nm-created wavelengths against µm coefficients).
- The slice pins raw-`double` term fields (dimensionless model coefficients
  whose unit is defined by `wavelengthScale`); the named record is the
  CLAUDE.md elevation, and `WaveLength` stays elevated at the API
  (`evaluate`, the interval endpoints) — recorded interpretation.
- Several tracked files in this checkout are `w/crlf` in the working copy
  (index `i/lf`, diffs normalization-invisible) even where untouched;
  `Dispersion.fs` was normalized to `w/lf` this round, untouched files left
  alone.
- `.manifest.state.json` shows as modified in `git status` — the arc-runner's
  own file (same as slices 001–006), left alone.

# Changelog

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
