# Impl-plan — slice 010 (IMPLEMENT: dispersive gyration ρ and Polder μ editing)

## Goal (Part C)

Extend the optically-active and magnetic rungs of the pure `MaterialComplexityEditor`
edit model to a **Constant vs Dispersive** sub-branch, mirroring the eps ladder.
Each symmetry-allowed gyration component and each Polder component gains a
`DispersionFormula` facet (edited through the 0033 raw `SumOfTerms` / `modelParameters`
surface), stored INDEPENDENTLY of the constant facet so unchecking Dispersive restores
the constant losslessly. `toComplexity` builds `RhoWithDispValue` / `MuWithDispValue`
under the sub-toggle; `ofComplexity` seeds from a dispersive ρ / μ; the
`UnsupportedComplexity` view-only fallback is DELETED (§0.2).

## Approach

**Parallel-field generalisation** (not a combined `GyrationClass<edit>`): keep the
existing CONSTANT fields (`gyration : GyrationClass<RhoValue>`, `polder : PolderValue<MuValue>`)
untouched so the `MaterialEditorView` constant panels and every existing test keep
compiling and passing verbatim. Add parallel DISPERSIVE facets alongside:

- `activityDispersion : ComponentDispersion` (new 2-case DU `ConstantComponents | DispersiveComponents`)
- `gyrationDispersion : GyrationClass<DispersionFormula>` — class-synced with `gyration`
- `magneticDispersion : ComponentDispersion`
- `polderDispersion : PolderValue<DispersionFormula>`

The engine's dispersive value types consume these directly:
`RhoWithDispValue of GyrotropicValue<DispersionFormula>` and
`MuWithDispValue of PolderValue<DispersionFormula>` — zero lossy conversion, exact round-trip.
A gyration/Polder component is a REAL SCALAR, whose dispersion IS exactly a
`DispersionFormula` (the payload of `SumOfTerms (RealNK …)`), per spec §C.0.

Class-sync invariant (`gyration` and `gyrationDispersion` share the symmetry class) is
maintained at the 3 mutation points via `syncGyrationDispersion`:
`ChooseAnisotropy`, `SetActivity ActivityOn`, `ChooseGyrationClass`.

## Files to modify

1. `OpticalConstructor.Domain/MaterialComplexityEditor.fs` (primary):
   - Add `ComponentDispersion` DU.
   - Add 4 fields to `MaterialComplexityEditState`; update `defaultState`.
   - DELETE `UnsupportedComplexity` from `MaterialComplexityEditError`.
   - Add messages: `SetActivityDispersion`, `SetGyrationComponentDispersion`,
     `SetMagneticDispersion`, `SetMuDiagonalDispersion`, `SetMuParallelDispersion`,
     `SetMuGyrationDispersion`.
   - Generalise `gyrationComponents` / `setGyrationComponent` to `GyrationClass<'g>`.
   - Add `defaultGyrationFormula`, `defaultPolderDispersion`, `syncGyrationDispersion`.
   - `applyMaterialComplexityMsg`: modify `ChooseAnisotropy`, `SetActivity`,
     `ChooseGyrationClass`, `ChooseGyrationAxis`; add the 6 new arms.
   - `toComplexity`: active + magnetic branches honour the sub-toggle.
   - `ofComplexity`: seed dispersive ρ / μ verbatim; delete `UnsupportedComplexity`.
2. `OpticalConstructor.TestWindows/MaterialEditorView.fs`: remove
   `UnsupportedComplexity` from the `editErrorReason` OR-pattern (forced by the deletion).
3. `OpticalConstructor.Tests/MaterialComplexityTests.fs`: add the acceptance tests.

## Tests (OpticalConstructor.Tests)

- `toComplexity` of an on-Dispersive gyration builds `RhoWithDispValue` whose
  `toRhoWithDisp` per-wavelength assembly equals the engine builder over a grid.
- `toComplexity` of an on-Dispersive Polder builds `MuWithDispValue` whose
  `toMuWithDisp` per-wavelength assembly equals the engine builder over a grid.
- `ofComplexity` → `toComplexity` round-trips a dispersive ρ and a dispersive μ by
  sampled tensor value over a grid.
- The lossless-uncheck of the new sub-toggles (constant restored, edits preserved).

## Risks

- **Build gate compiles the whole solution** — must not break the Ui/TestWindows.
  Mitigated by the parallel-field approach (constant path untouched) and the one
  forced `editErrorReason` edit.
- **Class-sync invariant** — localised to `syncGyrationDispersion` at 3 points; the
  round-trip test pins it.
- **Structural equality** of `MaterialComplexityEditState` (used by the render-loop
  guard + `Assert.Equal`): all new fields are pure data (`DispersionFormula`), so
  equality is preserved.
