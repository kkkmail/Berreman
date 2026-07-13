# Step 004 — impl-plan

## Goal (from the slice spec + spec-md Part B)

Re-seed the three coded-preset materials (Silicon, Langasite, the Vacuum
spacer) so their single-valued class is DATA readable without the engine
tensor. In `MaterialLibrary.fs builtInEntries`, change Silicon, Langasite and
Vacuum from `complexity = None` to `complexity = Some <valueTree>` with
`properties = <valueTree>.toProperties`, encoding the correct anisotropy so
`LibraryFacets.anisotropyOf` returns **Isotropic** (Silicon), **Uniaxial**
(Langasite), **Isotropic** (Vacuum). No classifier that evaluates the engine ε
tensor.

## Approach

- **Value trees (`MaterialLibrary.fs`)**
  - **Vacuum** — `constantComplexity (IsotropicTransparent RefractionIndex.vacuum)`.
    `toProperties` is byte-identical to `OpticalProperties.vacuum`
    (`Eps.vacuum = Eps.fromRefractionIndex 1.0`), so no physics changes.
  - **Silicon** — Isotropic, dispersive, absorbing: a one-segment
    `IsotropicDispersive` whose axis is the `EpsAxisEvaluated` rung of the
    dispersion-model ladder, re-stating the published `nSi`/`xiSiFinal`
    formulas (`OpticalProperties/Dispersive.fs`). The engine exposes only the
    Eps-valued closure, not the scalar index, so faithful reproduction through
    the ladder re-states the closed-form index. `properties =
    complexity.toProperties` reproduces `siliconOpticalProperties` bit-for-bit.
  - **Langasite** — Uniaxial, dispersive, optically active: a one-segment
    `UniaxialDispersive` (ordinary/extraordinary `EpsAxisEvaluated` re-stating
    the published La3Ga5SiO14 indices) plus a retained optical-activity
    component (`UniaxialActive`, mirroring `activeCrystalComplexity`). NOTE: the
    engine's dispersive gyration ρ (`rhoLa3Ga5SiO14`) is transcendental and
    `RhoWithDispValue` has NO closure escape (unlike ε's `EpsAxisEvaluated`), so
    ρ is re-expressed as a REPRESENTATIVE class-correct constant — the value
    tree cannot reproduce the dispersive ρ exactly. This is the one physics
    difference; the ε is reproduced faithfully.

- **Keep the coded presets VIEW-ONLY (`MaterialComplexityEditor`, two UI seams).**
  Both editability seams (`MaterialsWindowView.editableSelection`,
  `MaterialEditorView.init`) currently infer editability from `complexity`
  presence, so `complexity = Some` would flip these presets to *editable* and
  break the view-only UI tests. Add a pure Domain predicate
  `isEditableComplexity` (a complexity carrying an opaque `EpsAxisEvaluated`
  eps segment cannot be losslessly edited → view-only) and route both seams
  through it. The nine finite-term/constant built-ins stay editable; the
  evaluated presets stay view-only, so the existing UI tests pass unchanged.

- **OutOfBand diagnostic.** An `EpsAxisEvaluated` segment declares no band
  (a closure valid everywhere), so `definedSegmentIntervals` skips evaluated
  segments — the coded presets keep exposing NO defined segments and never
  flag, preserving the step-031 behaviour. Doc comments updated.

## Files

- Domain: `MaterialLibrary.fs` (re-seed + closures), `MaterialComplexityEditor.fs`
  (`isEditableComplexity`), `OutOfBandDiagnostic.fs` (skip evaluated + docs).
- Ui (scope extension — see Risks): `MaterialsWindowView.fs`,
  `MaterialEditorView.fs` (editability seams via the predicate).
- Tests: `MaterialComplexityTests.fs`, `LibraryFacetsTests.fs`,
  `PropagationTests.fs`, `OutOfBandDiagnosticTests.fs`,
  `DispersionModelsTests.fs` (verify) in `OpticalConstructor.Tests`.

## Risks / recorded interpretations

- **Scope extension beyond declared `touches: [Domain, Tests]`.** The re-seed
  unavoidably ripples into `OpticalConstructor.Ui` (the two complexity-presence
  editability seams). The step's gate roster runs `ui-smoke` + `ui-tests`, so a
  green gate REQUIRES the UI seams learn the predicate. Extending minimally into
  Ui (no Ui.Tests change — the existing view-only tests keep passing) is the
  only way to keep every gate green; this is the direct, unavoidable
  consequence of the mandated Domain change.
- **Langasite ρ.** Faithful reproduction of the dispersive gyration is
  impossible via the value tree (no `RhoWithDispValue` closure escape), so ρ is
  representative. The `PropagationTests` langasiteSilicon exact-equality pin is
  updated for the langasite film's new properties; Silicon (lower half-space)
  stays bit-for-bit.
- **Formula re-statement.** Silicon's copied ε is guarded by the existing
  `DispersionModelsTests` 1e-12 reproduction test; a langasite-ε reproduction
  test is added so a transcription slip is caught, not silent.
