# Impl-plan — spec 0042, slice 007 (IMPLEMENT — per-row Mueller-reconstruction glue)

## Approach

Step 007 ports the reference §2.2 / Part-D glue (`matrix_glue.py`,
`cpl_cpl_analyzer.py` from the sibling `optics-mueller` repo) into the existing
`OpticalConstructor.Domain.MuellerReconstruction` module as *many tiny pure
functions*, then adds one +90/−90 invariance `[<Fact>]` in `BerremanTests`.

The reference chain is:

- `parse_experiment_name` (regex grammar) → `family_from_experiment` +
  `object_rotation_from_parsed` → per-family `SourceModel` / `AnalyzerModel` →
  `s_base` / `a_base` → `s_eff = R(φ)·s_base`, `a_eff = a_base·R(−φ)` →
  `lincoef = kron(s_eff, a_eff)`.

Every effective state is built ONLY from the existing `Propagation` seams
(`rotationMueller`, `analyzerMueller IdealLinear`, `muellerElement`) plus the
step-001 `retarderMueller` and the step-002 `kron4` — no new 4×4 algebra.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
  — add `open System.Text.RegularExpressions` + `open OpticalConstructor.Domain.Library`
  (for `PolarizerKind`/`IdealLinear`), and a new Part-D section at the end of the
  module with:
  - types `Family`, `MatrixKind`, `ParsedObject`, `ParsedExperiment`, `DialSign`
    (`.value : float`), `SourceModel`, `AnalyzerModel`;
  - functions `parseExperiment`, `family`, `matrixKind`, `objectPhi`,
    `sourceBaseStokes`, `physicalAngle`, `analyzerBaseRow`, `effectiveSource`,
    `effectiveAnalyzer`, `designRow`.
- `Berreman/BerremanTests/MuellerReconstructionTests.fs` — two component-wise
  closeness helpers + one `[<Fact>]` pinning the acceptance
  (`CPL-(QZ#90-LR#90)-CPL` → `CplCpl` / `QzLrProduct` / φ=90 and the +90≡−90
  effective-state invariance).

## Risks / choices

- `family` / `matrixKind` are declared TOTAL (return `Family` / `MatrixKind`, not
  `Result`) per the slice's named signatures; the reference raises on unsupported
  labels. Only LP/CPL sources+analyzers and QZ/LR/AIR objects exist in the data,
  so the total mapping is faithful; unknown tokens degrade to Lp / Air (recorded).
- `physicalAngle` computes in radians (`Angle.value`) rather than the reference's
  degrees; the map is linear so it is identical.
- Calibration constants that populate `SourceModel`/`AnalyzerModel` come from
  Stage-1 AIR fitting (a later slice); this slice ships only the glue functions.
