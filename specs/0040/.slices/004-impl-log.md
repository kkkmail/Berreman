# Step 004 — impl-log

## Progress

- [x] Re-seed Silicon / Langasite / Vacuum in `MaterialLibrary.fs` (`complexity = Some`, `properties = complexity.toProperties`).
- [x] Add `isEditableComplexity` predicate in `MaterialComplexityEditor.fs`.
- [x] Route the two UI editability seams through the predicate (keep coded presets view-only).
- [x] Coded presets declare an unbounded band so the out-of-band diagnostic never flags them.
- [x] Update `OpticalConstructor.Tests` (complexity, facets, propagation, out-of-band).
- [x] Verify all gates green (build + 4 test suites), no new warnings, no CRLF churn.

## Files modified

### Domain (declared scope)
- `OpticalConstructor.Domain/MaterialLibrary.fs` — re-seed. Added `codedPresetInterval`
  (spectrum-spanning), `siliconRefractionIndex` (copies `nSi`/`xiSiFinal`),
  `langasiteOrdinary`/`langasiteExtraordinary` (copy the La3Ga5SiO14 indices),
  `siliconComplexity` (Isotropic dispersive), `langasiteComplexity` (Uniaxial dispersive
  + representative optical activity), `vacuumComplexity` (Isotropic transparent constant).
  Re-pointed the three entries to `complexity = Some …` / `properties = ….toProperties`.
  Added `open Berreman.Constants` (`meter`/`nm`) and `open Berreman.MathNetNumericsMath`
  (`cplx`/`complexI`/`createComplex`).
- `OpticalConstructor.Domain/MaterialComplexityEditor.fs` — added `isEditableComplexity`
  (a complexity carrying an opaque `EpsAxisEvaluated` eps segment is view-only).
- `OpticalConstructor.Domain/OutOfBandDiagnostic.fs` — doc comments only (the coded presets
  now declare an unbounded band rather than `complexity = None`; behaviour unchanged —
  `definedSegmentIntervals` still reads each segment's interval).

### Ui (SCOPE EXTENSION — see Decisions)
- `OpticalConstructor.Ui/MaterialsWindowView.fs` — `editableSelection` and the view-panel
  editability label gate on `isEditableComplexity` instead of `complexity` presence.
- `OpticalConstructor.Ui/MaterialEditorView.fs` — `init` opens an entry editable only when
  `isEditableComplexity`; otherwise view-only.

### Tests (declared scope, `OpticalConstructor.Tests`)
- `MaterialComplexityTests.fs` — replaced the `complexity = None` assertion with an
  "every built-in carries Some" assertion; added a silicon+langasite ε reproduction guard
  (pins the copied indices to the engine closures at 1e-12). Added `open OpticalProperties.Dispersive`.
- `LibraryFacetsTests.fs` — updated the anisotropy/transparency/dependent-facet/activity/
  dispersion-model/lifted-facet tests to the post-re-seed reality; added a direct
  `anisotropyOf` acceptance test (Silicon Isotropic, Langasite Uniaxial, Vacuum Isotropic).
- `PropagationTests.fs` — langasiteSilicon's legacy-expected film now pins the re-seeded
  material's own properties (its ρ is representative); Silicon (lower) stays pinned to the
  engine preset (ε reproduced bit-for-bit).
- `OutOfBandDiagnosticTests.fs` — the coded-preset test now asserts the unbounded-band
  never-flag behaviour instead of `complexity = None`.

## Decisions / interpretations (recorded per arc-runner "don't ask the user")

1. **Presets kept VIEW-ONLY despite `complexity = Some`.** Both UI editability seams
   (`MaterialEditorView.init`, `MaterialsWindowView.editableSelection`) inferred editability
   from `complexity` presence, so the re-seed would flip Silicon/Langasite to *editable* and
   break the view-only UI tests (`MaterialEditorWindowTests`, `MaterialsWindowTests`). The
   step's gate roster runs `ui-smoke` + `ui-tests`, so a green gate REQUIRES the seams learn
   a new signal. Added the pure Domain predicate `isEditableComplexity` (an opaque evaluated
   eps segment cannot be losslessly edited → view-only) and routed both seams through it. This
   keeps the existing view-only UI tests passing unchanged and is the direct, unavoidable
   consequence of the mandated Domain re-seed. **This required editing `OpticalConstructor.Ui`,
   which is outside the slice's declared `touches: [Domain, Tests]`** — recorded here as a
   necessary scope extension (no `OpticalConstructor.Ui.Tests` change was needed).

2. **Langasite ρ is representative, not reproduced.** The engine's dispersive gyration
   `rhoLa3Ga5SiO14` is transcendental and `RhoWithDispValue` carries only `DispersionFormula`
   components — it has NO closure escape as ε's `EpsAxisEvaluated` does — so the value tree
   CANNOT reproduce Langasite's dispersive ρ. Per the spec's own reuse precedent
   (`activeCrystalComplexity`), ρ is re-expressed as a class-correct constant `UniaxialActive`
   gyration that keeps Langasite optically active for the facets; its ε is reproduced
   faithfully. The langasiteSilicon exact-equality `PropagationTest` is updated accordingly
   (Silicon's ε half-space stays bit-for-bit; the Langasite film is pinned to the re-seeded
   material's properties). This is the one physics difference this slice introduces, and it is
   spec-sanctioned by the `activeCrystalComplexity` mirror directive.

3. **ε reproduced through the ladder's evaluated rung by re-stating the published formula.**
   The engine exposes only the Eps-VALUED closure of Silicon/Langasite, not the scalar index,
   so faithful reproduction re-states the closed-form index inside `EpsAxisEvaluated` — the
   same case `DispersionModels.toEpsAxis` lowers the transcendental models to. This is
   REPRODUCTION, never CLASSIFICATION: the anisotropy is read from the value-tree CASE, never
   by evaluating the engine ε tensor (spec §0.4 / B.1). The copied indices are guarded by the
   existing `DispersionModelsTests` (Silicon, 1e-12) and a new `MaterialComplexityTests`
   reproduction test (Silicon + Langasite ε, 1e-12), so a transcription slip fails a test.

4. **Coded presets declare an unbounded band (out-of-band).** Rather than teach the
   diagnostic to skip evaluated segments (which would have rippled into the OutOfBand test
   helpers in `OpticalConstructor.Ui.Tests`), the coded presets carry a single
   spectrum-spanning segment so no request can leave their defined range — preserving the
   "coded preset never flags" behaviour with the diagnostic logic UNCHANGED.

## Testing state

Diagnostic verification only (Invariant 6 — the worker acts, the arc-runner's gate engine
runs the gates after exit):
- `dotnet build Berreman.slnx -c Release` — 0 errors; only exempt warnings
  (4× NU1701, 4× SYSLIB0051), zero from our code.
- `BerremanTests` — 119 passed, 5 skipped, 0 failed.
- `OpticalConstructor.Tests` — 677 passed, 0 failed (was 675; +2 net: a Silicon/Langasite ε
  reproduction test and the `anisotropyOf` acceptance test; no test removed).
- `OpticalConstructor.Ui.Tests` — 656 passed, 0 failed across both categories (view-only UI
  tests pass UNCHANGED). One `TableRotationTests` Shift+wheel headless test flaked once, then
  passed on the clean re-run — unrelated to this slice (no rotation/table code touched).
- `git diff --numstat` equals `--ignore-cr-at-eol` on every touched `.fs` — no CRLF churn.

## Gotchas

- **`NkDispersionChartTests.fs:25`** carries a now-stale comment ("Silicon … `complexity =
  None`"). It is a COMMENT only — the test passes (it passes `Biaxial` explicitly, not via
  `anisotropyOfEntry`). Left untouched to keep `OpticalConstructor.Ui.Tests` unmodified; Part C
  (step 006) rewrites `NkDispersionChart` and its tests anyway.
- **`NkDispersionChart.anisotropyOfEntry`** now returns Isotropic (Silicon/Vacuum) / Uniaxial
  (Langasite) for these three instead of the `complexity = None` → Biaxial fallback, so the
  Materials view-panel n/k chart draws fewer (correct) series for them. No test pins that
  series count (the chart tests pass the anisotropy argument explicitly).

## Artifacts

None captured (build/test output inspected inline; nothing persistent to route to
`C:\GitHub\Berreman\specs\0040\.artifacts\`).
