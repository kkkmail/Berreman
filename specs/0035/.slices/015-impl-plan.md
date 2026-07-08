# Step 015 — impl-plan

## Goal

Make the four transcendental dispersion models (TaucLorentz, GaussianOscillator,
ForouhiBloomer, BrendelBormann) first-class in a dispersive segment by adding an
**evaluated-directly** third case to `EpsAxisDispersion`, so `toEpsAxis` becomes
**total** (all ten models lower) and the `NotAFiniteTermSum` / `SegmentNotLowerable`
rejection path disappears.

## Approach

1. **`Berreman/Berreman/Dispersion.fs`** — add
   `EpsAxisEvaluated of (WaveLength -> ComplexRefractionIndex)` to `EpsAxisDispersion`
   (Dispersion.fs:306); `complexIndex` for it applies the closure. A function-typed
   union case kills structural equality type-wide, so mark the type
   `[<CustomEquality; NoComparison>]` with `Equals`/`GetHashCode` that compares
   `RealNK` / `ComplexEps` structurally and the evaluated case by closure reference
   (`LanguagePrimitives.PhysicalEquality` / `PhysicalHash`). This keeps the
   structural equality `DispersionModel`, `EpsWithDispValue` and `MaterialComplexity`
   auto-derive (they delegate to this `Equals`).

2. **`OpticalConstructor.Domain/DispersionModels.fs`** —
   - `toEpsAxis` (:605) becomes total (`EpsAxisDispersion`, no `Result`); the four
     transcendental arms return `EpsAxisEvaluated (evaluate model)`.
   - Remove `EpsAxisLoweringError` / `NotAFiniteTermSum` (:504-505).
   - `toEpsValue` (:700) and `toOpticalProperties` (:716) become total.
   - `modelParameters` `SumOfTerms` arm (:360) gets an `EpsAxisEvaluated _ -> []`
     branch (no editable term scalars) so the match stays exhaustive.

3. **`OpticalConstructor.Domain/MaterialComplexityEditor.fs`** —
   - Drop `SegmentNotLowerable` from `MaterialComplexityEditError` (keep
     `NoSuchSegment` / `LastSegmentNotRemovable`).
   - `lowerAxis` / the `traverse` plumbing collapse: `dispersiveEps` becomes total
     (lowering never fails); `toComplexity` keeps its `Result` signature (always
     `Ok` now — the `ofComplexity` "kept for the consumer contract" precedent) so
     its ~6 call sites are untouched.

4. **Compile-driven consumers of the removed symbols** (outside the touch list but
   required for a green `build` gate over the whole `.slnx`):
   - `OpticalConstructor.Storage/MaterialImport.fs` — `sellmeierAxis` matched
     `toEpsAxis`'s `Ok`/`Error`; now `Ok (toEpsAxis model)`.
   - `OpticalConstructor.TestWindows/MaterialEditorView.fs` — `editErrorReason`
     drops the `SegmentNotLowerable` alternative. `complexitySummary` now shows a
     derived dispersive eps for a transcendental pick (still compiles).

5. **Tests** —
   - `OpticalConstructor.Tests/DispersionModelsTests.fs`: migrate every `Ok`/`Error`
     match on `toEpsAxis` / `toEpsValue` to the total outcome; rewrite the two
     "typed lowering error" tests (:304, :530) and the toEpsValue tl arm (:376) so
     the four transcendental models lower to `EpsAxisEvaluated` and its
     `complexIndex` matches `evaluate` across the grid; `Assert.Equal(Ok axis, …)`
     (:349) → `Assert.Equal<EpsAxisDispersion>(axis, …)`.
   - `OpticalConstructor.Tests/MaterialComplexityTests.fs`: add the required test —
     a `MaterialComplexity` carrying a transcendental dispersive segment derives
     (no `SegmentNotLowerable`) and evaluates matching `evaluate`.
   - `OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`: migrate the
     Sellmeier `toEpsAxis` match; rewrite the ForouhiBloomer/BrendelBormann pure
     test and the ui-smoke test to the new derive-successfully behaviour.

## Risks

- **Equality contract.** The custom `Equals` must keep `RealNK`/`ComplexEps`
  structural so the existing round-trip `Assert.Equal(Ok c, toComplexity st)` tests
  hold; the evaluated case is reference-equal, which is only exercised where the
  same closure threads through (`ofComplexity`→`toComplexity` passes the identical
  `f`). Built-ins carry only finite-term data, so their round-trips stay structural.
- **Exhaustiveness / warnings-as-errors.** Adding the case forces new match arms in
  `complexIndex` and `modelParameters`; `formulaOfModel` already has a `_` fallback.
- **Gate counts.** No tests are removed (rewrites are in place; one new test added),
  so no `count_at_least` regression.
