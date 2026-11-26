# Step 0005 — CLOSE_RED — impl log

## Progress

- [x] Recorded the G17 reference literals from the start tree's `DispersionModel.evaluate`, before
  anything moved. The step-4 build output (07:15) had unchanged production code. The script and
  its output are in `.artifacts/`.
- [x] Core: `AbscissaKind.abscissaAt`, `TaucLorentzLaw`, `GaussianOscillatorLaw`, and the two axis
  cases (structural `Equals`, hash tags 3 and 4)
- [x] Domain: `ofUnit` moved above the coefficient records; the intrinsic `toTaucLorentzLaw` /
  `toGaussianOscillatorLaw`; the `baseIndex` arms answer the law (bodies deleted there);
  `toEpsAxis` lowers to the law cases; `parameters` returns `[]`; doc comments updated
- [x] `LibraryFacets.facetKey` maps both law cases to `"Evaluated (transcendental)"`.
  `MaterialComplexity.isEditable` treats both as no term data.
- [x] Facts: three new law facts, two changed expectations
- [x] Build. The first local ELEV run found ELEV010 on `abscissaAt : float`; the fix
  elevated the abscissa (see Decisions). Rebuilt, and re-ran every local check.

## Files modified

- `Berreman/Berreman/Dispersion.fs`
  - `AbscissaValue` (new single-case DU)
  - `AbscissaKind.abscissaAt`
  - `TaucLorentzLaw` and `GaussianOscillatorLaw` (both `NumericCarrier`, each with `complexIndex`)
  - `EpsAxisDispersion` gains `TaucLorentzAxis` and `GaussianOscillatorAxis` in its cases,
    `complexIndex`, `Equals` and `GetHashCode`, and its doc comment
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs`
  - the `AbscissaKind.ofUnit` extension moved above the coefficient records, unchanged
  - `TaucLorentzCoefficients.toTaucLorentzLaw` and
    `GaussianOscillatorCoefficients.toGaussianOscillatorLaw`
  - `baseIndex`'s two arms answer the law; their kernel bodies are deleted
  - `toEpsAxis` lowers to the law cases
  - `parameters`' `SumOfTerms` arm answers `[]`
  - doc comments: the module header, `baseIndex`, `toEpsAxis`, `toEpsValue`,
    `toOpticalProperties`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryFacets.fs`: `facetKey` and its
  doc comment
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialComplexityEditor.fs`:
  `isEditable` and its doc comment
- `Berreman/BerremanTests/EpsWithDispValueTests.fs`: the `electronVolt` abscissa, the
  `verifyLawReferences` helper, and the three law facts
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DispersionModelsTests.fs`: the two
  AC-B5 facts' match patterns and comments (assertions unchanged)

## Decisions

- **`abscissaAt` returns `AbscissaValue`, not `float` (deviation from the slice text).** The slice
  prescribes `member abscissaAt (w : WaveLength) : float`. The ELEV analyzers reject that signature:
  `ELEV010 Dispersion.fs:304:21 - abscissaAt mixes bare primitive(s) float with elevated types in
  one signature`. AGENTS.md rules that the analyzers win where a document disagrees (spec 0048
  `.manual/002` R6), so the abscissa is elevated at its origin:
  - `AbscissaValue` is a single-case DU over `float` with `.value`.
  - `abscissaAt` wraps the value it computes.
  - Each law kernel unwraps it by pattern (`let (AbscissaValue x) = …`).

  The arithmetic (`w.value / scale` and `r.numerator / (w.value / r.wavelengthScale)`) is exactly
  what the slice prescribes, so the abscissa stays bit-identical to `Units.fromMeters`. The three
  exact-literal facts pin this.
- **The law kernels use the self-identifier `c`** (`member c.complexIndex`), so each body is its
  `baseIndex` arm moved character for character over `x`.
- **`ofUnit` moved, not copied.** `toTaucLorentzLaw` / `toGaussianOscillatorLaw` are intrinsic
  members of the coefficient records (a cross-type reuse towards an earlier type, here the core
  law). Intrinsic members can only see what is declared above them, so the `AbscissaKind.ofUnit`
  extension moved up from the lowering section to just after `ThermoOptic`. Its text is
  unchanged, and the old copy is deleted.
- **Micrometre Tauc-Lorentz set** (slice: "a band gap between 0.3 µm and 2.0 µm"): εInf 2.25,
  A 60, E0 1.1, C 0.6, Eg 0.7 µm. Three grid points fall below the gap (√ε∞ = 1.5 exactly) and two
  absorb.
- **Exact equality on the elevated value.** The law facts compare `ComplexRefractionIndex`
  values with `Assert.Equal<ComplexRefractionIndex>`. The references are born elevated, so no
  `.value` is needed. This is the exact equality the slice asks for.

## Testing state

These are local observations, made to confirm the change. The arc-runner's gate engine runs and
records the gates.

- `build`: 0 errors. The first (full) build showed only the two vendored MathNet SYSLIB0051
  warnings that were already on the start tree. The rebuild after the ELEV010 fix: 0 warnings,
  0 errors.
- `unit-tests` (EpsWithDispValueTests, DispersionFormulaTests): 24/24 succeeded, the three new
  law facts included (19 + 5 facts).
- `constructor-unit-tests` (whole suite): 733/733 succeeded. That includes the step-4 red
  `equal Tauc-Lorentz and Gaussian-oscillator models lower to equal axis data`, and the
  unchanged AC-B6 and LibraryFacetsTests facts.
- `ui-smoke`: 208/208 succeeded.
- `ui-tests` (MaterialEditorWindowTests, NkDispersionChartTests): 41/41 succeeded.
- `elevation`: the first run found ELEV010, fixed as above. The re-run printed `ELEV-GATE: OK - 0 new
  finding(s) in 11 project(s)`. Core changed, so the run covered core and its dependents.
- The three s01 invariants each find nothing.

## Artifacts

- `.artifacts/0005-reference-literals/evaluate.fsx`: the script that read the start tree's
  `DispersionModel.evaluate`
- `.artifacts/0005-reference-literals.txt`: the G17 literals it printed
- `.artifacts/0005-local-build.log`, `0005-local-unit-tests.log`,
  `0005-local-constructor-unit-tests.log`, `0005-local-ui-smoke.log`, `0005-local-ui-tests.log`,
  `0005-local-elevation.log`

## Gotchas

- The slice's prescribed `abscissaAt : float` fails ELEV010. See Decisions: the elevated
  `AbscissaValue` keeps the arithmetic unchanged.
- The slice's line citations into `DispersionModelsTests.fs` (`:300-337`, `:378-379`) had drifted
  since step 4 hoisted the coefficient builders. The facts were found by name.
- The fact `AC-B5 transcendental models lower to the evaluated case and match evaluate` keeps
  its id, which still says "evaluated case". Only its match and comment changed, because the
  test id is the contract in the step's effects.
