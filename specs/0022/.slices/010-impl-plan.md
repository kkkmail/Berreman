# 010 — Fitting core — impl-plan

## Approach

Build the fitting core on top of the slice-009 §G.1 interface
(`OptimizationInterface`), §G.2 ALGLIB adapter (`AlglibAdapter`), and §G.3
design-variable mapping (`DesignParameters`), all in the gated, UI-less
`OpticalConstructor.Optimization` project. Four new files, in compile order
after the slice-009 trio:

1. **`Ellipsometry.fs` (§G.6, [Standard])** — net-new `EllipsometricFunction`
   DU (`Psi | Delta`) with an evaluator over a `Solution`. The evaluator
   SHARES the §F.5 derivation by calling `Analytics.AnalysisFunctions.psiDelta`
   (ρ = r_p/r_s on `EmFieldSystem.reflected.amplitudeP/amplitudeS`,
   Ψ = atan|ρ|, Δ = arg ρ) — it does NOT re-derive Ψ/Δ and does NOT edit the
   engine's closed `OpticalFunction`. Compiled before `MeritFunction` because
   `FitTarget` must reference `EllipsometricFunction`.

2. **`MeritFunction.fs` (§G.4, [Core])** — `FitTarget` record carrying the
   quantity (`Photometric of OpticalFunction | Ellipsometric of
   EllipsometricFunction`), the sample point as an `IncidentLightInfo` (its
   `waveLength : WaveLength` is canonical-SI via `.value`), `desiredValue`,
   `weight`, `tolerance`, and `kind : TargetKind` (`Equality | Inequality of
   InequalityTarget`). `modelValue` runs `OpticalSystemSolver` on the G.3-mapped
   system at the sample point and reads `Solution.func` (the SAME evaluator the
   charts use). `buildResidual` assembles the weighted-LSQ residual vector
   (`weight*(model-desired)/max(tol,ε)` for equality, one-sided hinge for
   inequality) satisfying the G.1 `Residual` signature directly. No cache.

3. **`LocalRefinement.fs` (§G.5, [Core])** — `refine` (LM default) /
   `refineWith` (LM/quasi-Newton/simplex selectable). Builds the residual,
   passes G.3 `bounds` as `ParameterBounds` and the FitTargets' inequality
   targets as G.1 `InequalityTarget`s, calls `AlglibAdapter.optimize`, and folds
   the solution back through `DesignParameters.applyVector`. No thread spawned
   (cancellation is G.10/J.10, slice 011).

4. **`InverseFit.fs` (§G.7, [Standard])** — parse measured R/T or Ψ,Δ CSV/tab
   via `FSharp.Data.CsvFile` (matching the `SpectralImport` precedent), convert
   wavelength to canonical SI through the SOLE `Units.toWaveLength` seam, turn
   each row into a `FitTarget`, hand the list to `LocalRefinement.refine`. No
   format registry/autodetection beyond the single CSV/tab path.

Tests in `LocalRefinementTests.fs`: AC-G3 (bounded refinement: no negative
thickness, inequality hinge to zero) and AC-G4 (inverse fit reduces χ² with
sample points in meters), plus a small Ψ/Δ sharing check.

## Files to modify

- New: `OpticalConstructor.Optimization/{Ellipsometry,MeritFunction,LocalRefinement,InverseFit}.fs`
- New: `OpticalConstructor.Tests/LocalRefinementTests.fs`
- Edit: `OpticalConstructor.Optimization.fsproj` — register the four files
  (Ellipsometry → MeritFunction → LocalRefinement → InverseFit) and add the
  `FSharp.Data` 8.1.14 PackageReference.
- Edit: `OpticalConstructor.Tests.fsproj` — register `LocalRefinementTests.fs`.

## Risks / decisions

- **Reuse the chart evaluator**: `modelValue` MUST read `Solution.func` so fit
  and chart never diverge — load-bearing.
- **Ψ/Δ shared, not forked**: call `AnalysisFunctions.psiDelta`; do not re-derive.
- **`refine` initial vector**: G.3 `DesignParameter` carries no getter (and
  `DesignParameters.fs` is out of scope to edit), so `refine` takes the initial
  design-variable vector explicitly — the optimizer needs a start point.
- **Inequality flow**: per-scalar hinge composed into the residual (R-1,
  matching the slice-009 adapter guidance) AND the FitTargets' inequality
  targets passed through to the request as G.1 `InequalityTarget`s (R-2).
- LM is the default method; quasi-Newton/simplex selectable via `refineWith`.
