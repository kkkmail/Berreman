# 011 — impl-log (Part G §G.8–G.11)

## Retry round (attempt 02) — code-judge route-back

The cycle-1 code-judge routed back on a single numerical-correctness finding in
`FitQuality.residualJacobian` (the rest of the slice was accepted). Changes this
round, all confined to the judge's steering:

- **Fixed the forward-difference step** (`FitQuality.fs`): was
  `h = 1.0e-7 * (max 1.0 (abs x.[p]))`, whose `1.0` floor perturbs a canonical
  meter-scale thickness (~2.5e-7 m) by ~40 %, making the covariance / std-errors /
  CIs / correlation numerically meaningless at the project's canonical scale
  (§0 #3). Now `h_p = √eps · max(|x_p|, 1e-12)` — a per-parameter RELATIVE step
  with NO absolute meter-scale floor; the `1e-12` floor only keeps the step
  nonzero for an exactly-zero parameter (sub-picometre, never reached by a real
  thickness fit). Added `private sqrtMachineEps` / `private stepFloor`.
- **Added a regression test that catches a mis-scaled Jacobian**
  (`SynthesisFitPageTests.fs`, `residualJacobian and covariance stay accurate for
  a meter-scale parameter`): a NONLINEAR residual rᵢ(x)=sin(ωᵢx)−dᵢ at x0=2.5e-7
  with a closed-form Jacobian (ωᵢ cos(ωᵢx)) and standard error. Asserts the
  numerical Jacobian matches analytic (rel < 1e-5), the covariance diagonal is
  finite and strictly positive, AND `reportFrom`'s standard error matches its
  analytic closed form (rel < 1e-3). **Verified RED against the old step** (Jacobian
  row-0 rel error 6.1 % » 1e-5) and **GREEN with the fix** — a real failing test,
  not a placeholder. Unlike AC-G5 (CI brackets the estimate, unit diagonal — true
  for any non-negative σ) this assertion is sensitive to the step scaling.
- **Consolidated the triplicated `sumSq`** (reuse-critic F1): one
  `let internal sumSq` in `OptimizationInterface`; removed the byte-identical
  private copies in `AlglibAdapter`, `FitQuality`, `Synthesis` (all three `open`
  `OptimizationInterface`). The load-bearing Σ residualᵢ² definition is now stated
  once, so the optimizer's objective, the synthesis merit, and `FitReport.chiSquared`
  cannot drift.
- **Recorded the needle gradient interpretation** (SoW Architecture/Gotchas): §G.9
  says "evaluate the merit-function gradient w.r.t. inserting a thin layer at each
  candidate depth"; the implementation evaluates the actual post-insertion merit at
  each candidate depth and keeps the lowest (an exact discrete evaluation of that
  gradient direction), which satisfies AC-G6 and is a defensible reading of §G.9.
- Left **F2** (duplicated `thicknessMeters`), **F3** (duplicated test fixtures),
  and the **singular-`JᵀJ` guard** as Deferred notes per the judge's steering.

Gates re-run this round: build 0 errors; BerremanTests 84 passed / 5 skipped;
OpticalConstructor.Tests 124 passed (123 → +1 the new regression test).

## Progress

- [x] FitQuality.fs (§G.8 R-1)
- [x] Synthesis.fs (§G.9 R-2)
- [x] SynthesisFitPage.fs (§G.10 R-3)
- [x] BerremanTests/OptimizationTests.fs (§G.11 R-4 + AC-F3/F5/F7/F8)
- [x] OpticalConstructor.Tests/SynthesisFitPageTests.fs (AC-G5/G7/G8)
- [x] .fsproj registrations + BerremanTests→Optimization ProjectReference
- [x] Gates: build, unit-tests, constructor-unit-tests — all green

## Files modified

New:
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/FitQuality.fs` —
  `FitReport` (χ²/MSE, reduced χ², covariance, std errors, 95% CIs, correlation,
  per-sample residuals); `residualJacobian` (forward difference); covariance
  Cov = reducedχ²·(JᵀJ)⁻¹ via the `MathNetNumericsMath` `RealMatrix` seam;
  `toBinz`/`fromBinz` via `Softellect.Sys.Core.serialize BinaryZippedFormat`.
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/Synthesis.fs` —
  `needleInsertion`, `tunnel`, `simulatedAnnealing`, `geneticAlgorithm`; all
  global loops observe `shouldCancel`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/SynthesisFitPage.fs` —
  Avalonia-free MVU model/update + six-item §G.10 checklist helpers.
- `Berreman/BerremanTests/OptimizationTests.fs` — AC-G9 (R/T + Ψ self-consistency),
  AC-G3, AC-G6, AC-F3, AC-F5, AC-F7, AC-F8.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SynthesisFitPageTests.fs` —
  AC-G5, AC-G7 (model-level + loop-level cancel), AC-G8, nav entry.

Edited (attempt-02):
- `OptimizationInterface.fs` — added one `let internal sumSq` (reuse-critic F1
  consolidation); removed the three private copies from `AlglibAdapter`,
  `FitQuality`, `Synthesis`.
- `FitQuality.fs` — fixed the `residualJacobian` forward-difference step to a
  per-parameter relative `√eps · max(|x|, 1e-12)` (no absolute meter-scale floor).
- `SynthesisFitPageTests.fs` — added the mis-scaled-Jacobian regression test.

Edited:
- `OpticalConstructor.Optimization.fsproj` — registered `FitQuality.fs`,
  `Synthesis.fs`; added `Softellect.Sys` 10.0.101.41 PackageReference (the `.binz`
  serializer; NOT reachable transitively).
- `OpticalConstructor.Ui.fsproj` — registered `SynthesisFitPage.fs`.
- `BerremanTests.fsproj` — registered `OptimizationTests.fs`; added a
  ProjectReference to `OpticalConstructor.Optimization` (enabling edit for AC-G9/
  G3/G6; no UI reference, per R-4).
- `OpticalConstructor.Tests.fsproj` — registered `SynthesisFitPageTests.fs`.

NOT edited:
- `.gitignore` — already ignores `*.binz` (line 336, slice 012 / Part I). Rule NOT
  duplicated; the fit-history sidecar path is covered.

## Decisions

- **§G.11 / AC-G9 → path (b)** (self-consistency/regression). No operator-supplied
  Wolfram fixture under `specs/0022/.manual/`; §G.0 places the Wolfram workflow
  outside the repo. Synthetic targets generated from the REAL `OpticalSystemSolver`,
  parameters perturbed, recovered via `LocalRefinement` + `LevenbergMarquardt` over
  the full `DesignParameters`→`MeritFunction`→`optimize`→`AlglibAdapter`→solver
  path. No fabricated Wolfram numbers.
- **AC-F3/F5/F7/F8 in `BerremanTests/OptimizationTests.fs`** —
  `AnalysisFunctionsTests.fs:20-22` / `MuellerMatrixTests.fs:17` explicitly defer the
  full equivalence suite here. **AC-G5/G7/G8 in `OpticalConstructor.Tests`** (they
  touch the `SynthesisFitPage` model and the UI project; the BerremanTests file MUST
  NOT depend on the UI project, R-4).
- **Covariance through the engine math seam** — J and Jᵀ built as `RealMatrix`, the
  multiply and `.inverse` routed through `MathNetNumericsMath` (§A.8: NOT ALGLIB).
- **`BerremanTests` → `OpticalConstructor.Optimization` ProjectReference** added so
  `OptimizationTests.fs` can drive `LocalRefinement`/`Synthesis` with the real
  solver. Necessary enabling edit beyond the spec's "planned" `.fsproj` edit list.
- **A new test file in `OpticalConstructor.Tests`** (`SynthesisFitPageTests.fs`) for
  AC-G5/G7/G8 — beyond the spec's "Files in scope" list, but the testing plan
  assigns these ACs to the constructor-unit-tests gate and they cannot live in the
  UI-free BerremanTests file.

## Testing state

All three gates pass locally (Release):
- `build`: `dotnet build Berreman.slnx -c Release` — 0 errors.
- `unit-tests` (`BerremanTests`): 84 passed, 5 skipped, 0 failed (prior baseline 76
  → +8 in `OptimizationTests.fs`).
- `constructor-unit-tests` (`OpticalConstructor.Tests`): 124 passed, 0 failed (prior
  baseline 118 → +6 in `SynthesisFitPageTests.fs`; attempt-02 adds the mis-scaled-
  Jacobian regression test).

## Gotchas

- **Needle "gradient" is evaluated as the post-insertion merit (attempt-02).** §G.9
  says needle optimization "evaluate[s] the merit-function gradient w.r.t. inserting
  a vanishingly thin layer at each candidate depth". `Synthesis.needleInsertion`
  evaluates the ACTUAL merit of the system grown by a thin needle at each candidate
  depth and keeps the lowest — an exact discrete evaluation of the insertion-gradient
  direction (the gradient is the limit of exactly this finite difference as the needle
  thickness → 0). This satisfies AC-G6 (films grow by one `Layer`; a near-absent needle
  + re-refinement only decreases merit) and is the defensible reading of §G.9 chosen.
- **Finite-difference step is per-parameter RELATIVE (attempt-02).** `residualJacobian`
  uses `h_p = √eps · max(|x_p|, 1e-12)`, NOT an absolute floor. Do not reintroduce a
  `max 1.0 |x|`-style floor: at the canonical meter scale (|x| ~ 1e-7) a `1.0` floor
  perturbs by ~40 % and corrupts the covariance. The `1e-12` floor is only the
  zero-parameter guard.
- **Engine Ψ uses the reciprocal p/s convention.** The solver's
  `psiDelta` reads ρ off `EmFieldSystem.reflected.amplitudeP/amplitudeS`, which here
  equals the RECIPROCAL of the textbook Fresnel |r_p/r_s| (solver Ψ = π/2 − analytic
  Ψ). AC-F5 therefore asserts tan Ψ matches the analytic ratio OR its reciprocal, up
  to that axis labelling — not a fixed orientation.
- **ALGLIB minlm is poorly conditioned for meter-scale (~1e-7) variables.** The R/T
  thickness fit converges in canonical meters, but the more multimodal/sensitive
  ellipsometric Ψ fit gets a noise-dominated finite-difference gradient at the
  meter scale and stalls. AC-G9-Ψ conditions the thickness `DesignParameter` in
  NANOMETRES (O(100)); `getSys` still stores canonical meters via `Thickness.nm`, so
  §0 constraint 3 holds. A future slice may want `minlmsetscale` in the adapter.
- **`reflectanceOf` hardcodes normal-incidence vacuum light.** The AC-F8 TIR sanity
  check must solve with the actual 60°-glass `light`, not `reflectanceOf` (which
  builds `IncidentLightInfo.create` = vacuum/normal). The sweep itself uses the
  correct `light` via `FixedInfo`.
- **`Assert.Equal(double, double, digits)` caps digits at 15** (not 17/18).
- **`Softellect.Sys` is NOT transitive into Optimization.** Even though `Berreman`
  references it, the package did not flow to `OpticalConstructor.Optimization`; an
  explicit PackageReference was required for the `.binz` serializer.
- **`BinaryZippedFormat` lives in `Softellect.Sys.Primitives`**, `serialize`/
  `deserialize` in `Softellect.Sys.Core` — both opens are needed.
- **`interface` is a reserved F# keyword** — cannot be used as an identifier.

## Artifacts

`C:\GitHub\Berreman\specs\0022\.artifacts` — no persistent captures needed this
round (all gate output was transient console).
