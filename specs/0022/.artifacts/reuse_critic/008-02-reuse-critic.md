# Reuse critique -- 008.slice-md cycle 2

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (engine `Berreman/Berreman/`, `Analytics/`, `OpticalConstructor/*`, `BerremanTests/`, `OpticalProperties/`).
- Files inspected: ~18/200 (targeted, not exhaustive — the worker-added/edited sources plus the engine seams they touch and the candidate-duplication sites: `Fields.fs`, `Geometry.fs`, `FieldFunctions.fs`, `MatrixComparison.fs`, `SolverTests.fs`, `MaterialImport.fs`, `SourceSpec.fs`). Cap NOT reached.
- Extensions: `.fs` (the `.md`/`.json` artefacts carry no reusable helpers).
- Cycle-2 delta: cleanup-only re-spawn (`FieldFunctions.fs:93` EOL churn reverted; `MuellerMatrixTests` switched to the shared tolerance). No analysis code or test logic changed, per the SoW Changelog.

## Findings

The engine-seam reuse this slice is judged on is clean throughout: `absorptance` calls `Solution.func`; `stokesSystem` assembles the existing `StokesSystem` record from `Solution.stokesI/stokesR/stokesT`; `degreeOfPolarization` reads the `StokesVector` components; `psiDelta`/`ncs` form ρ from `EmField.amplitudeP/amplitudeS` off the reflected field; `fieldDepthProfile`/`layerAbsorptance` step `EmField.propagate`/`EmField.intensity` off `BaseOpticalSystemSolver`; `totalAbsorbedPower` re-anchors to `AnalysisFunctions.absorptance`; and `em.muellerMatrix` is a pure two-token delegation to `MuellerMatrix.fromEmFields`. No 4×4 algebra, s/p decomposition, or extra solver call is re-derived. The three notes below are all low-severity; cycle 2 already resolved the actionable half of cycle-1's F1.

### F1: Mueller test comparator still diverges from the `MatrixComparison` convention (tolerance half RESOLVED this cycle)

- **Worker added:** `MuellerMatrixTests.assertMuellerEqual` (`BerremanTests/MuellerMatrixTests.fs:20-24`) — a hand-rolled nested `for i in 0..3 / for j in 0..3` element loop asserting `abs (expected.[i,j] - actual.[i,j]) < allowedDiff`.
- **Existing helper:** the `verify*Equality` family in `BerremanTests/MatrixComparison.fs:22-71`, and the established Mueller-assertion precedent `SolverTests.fs` `runTestMuellerMatrixR`, which verifies a Mueller result through the Stokes round-trip (`mr * i` + `verifyVectorEqualityStokes`) rather than a fresh element loop. The project prompt pins `MatrixComparison.fs` as the tolerance/comparison home (`arc-runner.user-md` Code style §).
- **Why it matters:** cycle 1 flagged two things here — a fresh `1.0e-10` tolerance literal AND a hand-rolled comparator. **Cycle 2 fixed the tolerance half**: `MuellerMatrixTests.fs:10,24` now `open BerremanTests.MatrixComparison` and assert against the shared `allowedDiff`, so the third-tolerance hazard is gone. What remains is purely the comparator shape — an inline element loop instead of the module's `verify*Equality` convention. There is no existing verifier typed for `RealMatrix4x4` (the `MuellerMatrix` payload), so this is *pattern divergence*, not a reuse the worker skipped.
- **Suggested action:** advisory only — optionally lift the comparator into `MatrixComparison.fs` as `verifyMuellerEquality`/`verifyRealMatrix4x4Equality` to match its siblings, or leave it local (the delegation-identity check is legitimately exact). Cosmetic; not blocking. The substantive cycle-1 nit is already cleared.

### F2: `Colorimetry.Illuminant` DU + embedded D65/A table parallels the existing `StandardIlluminant`

- **Worker added:** `type Illuminant = D65 | IlluminantA` and the embedded `illuminantTable` of D65/A relative spectral power at 10 nm (`Analytics/Colorimetry.fs:17-19, 67-111`).
- **Existing helper:** `StandardIlluminant = D65 | A` and its weight source `planck` in `OpticalConstructor.Domain/SourceSpec.fs:55-70, 134-136` (slice 007, Part E), which already models these same two CIE illuminants.
- **Why it matters:** the tree now carries two definitions of "the D65/A illuminants" — a tabulated 10 nm CIE curve here vs Planckian approximations (6504 K / 2855.54 K) in `SourceSpec`. They will not agree numerically, so a future consumer mixing §E source weighting with §F.8 colour could see an inconsistent D65. Two strong mitigations: (a) the dependency direction blocks clean reuse — `Analytics` is consumed *by* `OpticalConstructor.*`, so `Colorimetry` cannot reference `OpticalConstructor.Domain.SourceSpec` without inverting the dependency; and (b) AC-F8 / R-8 explicitly authorise the embedded CIE/illuminant tables as "the only net-new numeric data this part introduces." The worker followed the spec.
- **Suggested action:** leave as-is (spec-sanctioned, assembly-boundary-constrained); surfaced so the cross-module illuminant divergence is on record for the judge and for Part H, not a silent fact. No re-spawn warranted on this.

### F3: `Colorimetry.interp` is a near-duplicate of the storage layer's clamped linear-in-λ interpolation (carried from cycle 1)

- **Worker added:** `Colorimetry.fs:114` `let private interp (table : (float * float)[]) (wl : float)` — clamped piecewise-linear interpolation over a tabulated `(λ nm, value)` curve.
- **Existing helper:** `OpticalConstructor.Storage/MaterialImport.fs:55-78` `buildTabulatedClosure`, documented as "Linear-in-λ interpolation over the tabulated samples, clamped to the ends" — the same bracket-and-lerp scheme.
- **Why it matters:** two implementations of the same numerical scheme now exist and can drift silently. But reuse is not reachable: `buildTabulatedClosure` is `private`, lives in the downstream `OpticalConstructor.Storage` assembly (wrong dependency direction), and returns an `Eps` closure, not a scalar; the engine tree exposes no shared scalar-interp primitive for `Analytics` to call. With no reachable helper, the rubric makes this unactionable today.
- **Suggested action:** leave as-is / document; if a shared scalar interp ever lands in the engine math seam, fold both sites onto it. Not blocking.

## Bottom line

Cycle 2 is a cleanup-only re-spawn that resolved the one actionable item from the cycle-1 reuse pass (the Mueller test now reuses `MatrixComparison.allowedDiff` instead of a third tolerance literal) and introduced no new duplication. The engine-seam reuse the slice exists to enforce is clean. The three residual notes are a cosmetic comparator-convention divergence (F1) and two cross-assembly near-misses (F2 spec-sanctioned, F3 with no reachable helper) — none rises to a reuse reason for a third cycle. My read: shippable on reuse grounds; the judge decides routing.
