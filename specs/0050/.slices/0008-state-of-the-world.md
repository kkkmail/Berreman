# Step 0008 — IMPLEMENT — state of the world

## Where we are

This is step 8 of spec 0050, section S1.1 ("Every dispersion axis is data: tabulated and
transcendental cases"). Step 7 made every `DispersionModel` lower to data. The coded Silicon and
Langasite presets still carried `EpsAxisEvaluated` closures; this step turns them into data.
Their indices are the engine's published closed forms. Those closed forms moved from
`OpticalProperties/Dispersive.fs` into the core as the closed set `PublishedIndexLaw`. The engine
presets and the library complexities both read it, so each closed form exists once (.spec-md
§3.1). Nothing built in carries `EpsAxisEvaluated` now; step 9 deletes the case.

## What's working

- Moved Silicon's and Langasite's published closed-form indices into the core as `PublishedIndexLaw` (wire tags 1, 2, 3)
- `EpsAxisDispersion` gains `PublishedLawAxis`, compared and hashed structurally
- The engine `Silicon` / `Langasite` presets read the laws, bit-for-bit unchanged (eps, g11 and g33)
- The library's Silicon and Langasite complexities carry `PublishedLawAxis`; the re-stated closures are deleted
- `parameters`, `facetKey` and `isEditable` take the case: same facet key, still view-only
- Added three facts in `EpsWithDispValueTests`: exact G17 law literals, the engine presets' eps, and the wire tag

## Tests

These runs are local observations, made to confirm the change. The arc-runner's gate engine runs
and records the gates.

- `build`: 0 errors. The first full build showed only the two vendored MathNet SYSLIB0051
  warnings that were already on the start tree. The final rebuild: 0 warnings, 0 errors.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 11 project(s)`.
- `unit-tests` (EpsWithDispValueTests, DispersionFormulaTests): 29/29 succeeded. The three new
  facts pass, and the laws equal their literals exactly.
- `constructor-unit-tests` (whole suite): 734/734 succeeded. That includes the Silicon and
  Langasite reproduction facts and LibraryFacetsTests, all unchanged.
- `ui-tests` (MaterialsWindowTests, FsxAcceptanceTests): 49/49 succeeded.
- `ui-smoke`: 208/208 succeeded.
- The three s01 invariants (`elev-list-empty`, `no-vstest`, `no-kit-kit-json`) each find nothing.
- A bit-for-bit check of the changed presets against the start-tree values (eps, g11 and g33 at
  400–1000 nm) found 0 differences.

```yaml
gates:
  berreman_unit_tests:     29
  constructor_unit_tests: 734
  ui_smoke_tests:         208
  ui_tests:                49
```

`berreman_unit_tests` and `ui_tests` are the counts under this step's class filters.

## Architecture

- **The coded presets' laws are core data.**
  - `PublishedIndexLaw` lives in `Berreman/Berreman/Dispersion.fs`, after the four
    transcendental laws. It is a fieldless closed set: `value : int` is its wire tag (1, 2, 3),
    and `tryOfWire` answers `None` for any other tag.
  - Its `complexIndex` arms are the `Dispersive.fs` closed forms, moved verbatim. Silicon's k is
    a ratio of polynomials; Langasite's k is a Gaussian in λ centred at 280 nm.
- **The engine presets read the laws.**
  - `Silicon`'s eps is `Eps.fromComplexRefractionIndex` of `SiliconIndexLaw.complexIndex`.
  - `Langasite`'s eps is the (nₒ, nₑ, nₒ) diagonal of the two laws.
  - Its g11 / g33 share the class-private `realIndexSum` (Re nₒ + Re nₑ), which they used to
    re-state inline.
- **The library carries the law, not a closure.** The Silicon and Langasite complexities name
  the law. The re-statements in `MaterialLibrary.fs` are gone, and the file no longer names
  `EpsAxisEvaluated`.
- **Equality.** `PublishedLawAxis` compares structurally (hash tag 7). Only `EpsAxisEvaluated`
  still compares by closure reference.
- **Facet and editability are unchanged.** `facetKey` files the case under
  "Evaluated (transcendental)", so Silicon's facet key is the same. `isEditable` treats it as
  data without term coefficients, so Silicon and Langasite stay view-only.

## Deferred

- Nothing in this step's scope is deferred. `EpsAxisEvaluated` stays because the spec gives its
  deletion to step 9. Only two test fixtures construct it now (`OutOfBandDiagnosticTests`,
  `OutOfBandBadgeTests`).

## Gotchas

- **The start tree's Langasite doc comments were swapped.** The ordinary let was documented as
  "extraordinary", and the other way round. The code was consistent, and the laws keep the
  code's naming.
- **Comments outside this step's effects still describe a coded closure.** They are
  `OutOfBandDiagnostic.fs:21`, `MaterialComplexityTests.fs:153` and `LibraryFacetsTests.fs:489`.
  The behaviour they describe is unchanged, and this step's effects do not include those files.
- **A pipe after a match binds to the last arm only.** Each `complexIndex` arm pipes its own
  value into `ComplexRefractionIndex`; keep it that way when you edit the law.

## Changelog

- 2026-11-05 — step 0008: Silicon's and Langasite's published closed-form indices moved into the
  core as `PublishedIndexLaw` / `PublishedLawAxis`. The engine presets and the library
  complexities both read them, and the presets are bit-for-bit unchanged. Added three facts in
  `EpsWithDispValueTests`.
