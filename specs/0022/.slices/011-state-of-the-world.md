# 011 — Fit quality, synthesis/global optimization, Synthesis/Fit page & Wolfram validation (Part G §G.8–G.11) — state of the world

## Where we are

Slice 011 closes Part G on top of the slice-009 optimization infrastructure
(§G.1–G.3) and the slice-010 fitting core (§G.4–G.7). It adds [Standard]
fit-quality reporting with the `FitReport` `.binz` sidecar (§G.8), [Standard]
needle/tunnelling synthesis and global optimization (§G.9), the Avalonia-free
Synthesis/Fit operator page over the six-item §G.10 UX checklist, and the §G.11
validation of the ALGLIB `minlm` path plus the deferred AC-F3/F5/F7/F8 equivalence
cross-checks. Slice 012 (Part H — charts) and later slices build on this; Part G
is complete.

## What's working

- Add `FitQuality.fs` (§G.8): `FitReport` exposing χ²/MSE, reduced χ², covariance,
  standard errors, 95% confidence intervals, the parameter correlation matrix, and
  per-sample residuals; covariance Cov = reducedχ²·(JᵀJ)⁻¹ routed through the
  `MathNetNumericsMath` `RealMatrix` seam (NOT ALGLIB, §A.8); persistable as a
  `.binz` fit-history sidecar via `serialize … BinaryZippedFormat`.
- Add `Synthesis.fs` (§G.9): needle insertion (grow films by one `Layer` at the
  merit-minimizing depth, then re-refine via §G.5), tunnelling (perturb + re-refine),
  and simulated-annealing / genetic-algorithm global loops over the G.4 residual;
  every global loop observes a cooperative `shouldCancel` flag.
- Add `SynthesisFitPage.fs` (§G.10): Avalonia-free MVU model/`update` (P3) covering
  the named nav entry, the per-iteration progress payload, one-click non-destructive
  Start, confirm-gated Accept, the `plotComparison` fit-vs-measured overlay refresh,
  and distinct Cancel + Revert controls.
- Add `BerremanTests/OptimizationTests.fs`: AC-G9 (R/T and ellipsometric Ψ
  self-consistency over the real solver), AC-G3 (bounded), AC-G6 (needle), and the
  AC-F3/F5/F7/F8 cross-checks; add the enabling `OpticalConstructor.Optimization`
  ProjectReference (no UI dependency).
- Add `OpticalConstructor.Tests/SynthesisFitPageTests.fs`: AC-G5 (`FitReport` +
  `.binz` round-trip), AC-G7 (model-level Cancel/Revert + loop-level cooperative
  cancel), AC-G8 (overlay refresh), and the nav entry.
- Register the new files in their `.fsproj`s; all three gates green.

## Tests

- `build` gate: PASS — `dotnet build Berreman.slnx -c Release`, 0 errors.
- `unit-tests` gate (`BerremanTests`): PASS — 84 passed, 5 skipped, 0 failed
  (baseline 76 → +8 in `OptimizationTests.fs`).
- `constructor-unit-tests` gate (`OpticalConstructor.Tests`): PASS — 124 passed,
  0 failed (baseline 118 → +6 in `SynthesisFitPageTests.fs`; attempt-02 adds the
  mis-scaled-Jacobian regression test, verified RED against the old step).

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 124
```

## Architecture

- **§G.8 covariance through the engine math seam, not ALGLIB.** `FitQuality` builds
  the residual Jacobian numerically and forms Cov = reducedχ²·(JᵀJ)⁻¹ by
  constructing J and Jᵀ as `RealMatrix` and routing the multiply + `.inverse`
  through `MathNetNumericsMath.fs` (§A.8: ALGLIB is for optimization only; engine
  linear algebra stays on the math seam). `FitQuality` returns plain arrays and
  renders no charts.
- **§G.8 Jacobian step is per-parameter RELATIVE (attempt-02 fix).** The
  forward-difference step is `h_p = √eps · max(|x_p|, 1e-12)`, scaled to each
  parameter's own magnitude with NO absolute meter-scale floor. The earlier
  `1.0e-7 · max(1.0, |x|)` floored at 1e-7 m and perturbed a canonical thickness
  (~2.5e-7 m) by ~40 %, which made the covariance / standard errors / CIs /
  correlation numerically meaningless at the project's canonical scale (§0 #3). A
  regression test (nonlinear residual with a closed-form Jacobian and standard error)
  now pins both the Jacobian accuracy (rel < 1e-5) and the standard-error closed form
  (rel < 1e-3); it is RED against the old step and GREEN with the fix.
- **One shared `sumSq` for the least-squares definition (attempt-02, reuse-critic F1).**
  Σ residualᵢ² lives once as `let internal sumSq` in `OptimizationInterface`; the
  §G.2 adapter objective, the §G.9 synthesis merit, and the §G.8 `FitReport.chiSquared`
  all call it, so the load-bearing χ²/merit definition cannot drift between the three.
- **§G.9 needle "gradient" = post-insertion merit per candidate depth (attempt-02
  interpretation note).** §G.9 phrases needle optimization as evaluating the
  merit-function gradient w.r.t. inserting a thin layer at each candidate depth;
  `Synthesis.needleInsertion` evaluates the ACTUAL merit of the design grown by a
  thin needle at each candidate depth and keeps the lowest — an exact discrete
  evaluation of that insertion-gradient direction (the gradient is the limit of this
  finite difference as the needle thickness → 0). This satisfies AC-G6 and is the
  chosen reading of §G.9.
- **§G.8 report is `.binz`-derived, never JSON.** `toBinz`/`fromBinz` wrap
  `Softellect.Sys.Core.serialize`/`deserialize` with `BinaryZippedFormat`
  (`Core.fs:257`); the report is a sidecar referenced from the canonical JSON, not
  embedded in it (§0 constraint 4).
- **§G.9 reuses §G.5/§G.4, not a forked optimizer.** Needle insertion re-refines via
  `LocalRefinement.refine` (ALGLIB `minlm` through §G.2). SA/GA are explicit F#
  loops over the same G.1 `Residual` closure (the merit assembly of §G.4), as §G.9
  sanctions for the methods ALGLIB does not provide; no parallel/distributed/plugin
  infrastructure is added.
- **§G.10 page is Avalonia-free (P3).** The model/`update` and all UX-checklist
  helpers carry no Avalonia type, so AC-G7/AC-G8 are unit-tested headlessly. The
  fit-vs-measured overlay is exposed as the exact `plotComparison` payload
  (`FixedInfo list × OpticalFunction list`); the view binds it without a manual
  reload. Long-running execution defers to the Part J §J.10 job runner (slice 016) —
  this page supplies only the per-iteration progress payload and a single cancellable
  run (no job-queue).
- **Cooperative cancellation split.** The page model holds `cancelRequested`; the
  G.9 SA/GA loops observe a `shouldCancel : unit -> bool` and stop with
  `TerminationReason = Failed "cancelled"`. The closed slice-009 `TerminationReason`
  DU was reused (not extended) for the cancelled state.

## Deferred

- **§G.11 / AC-G9 path:** path (b) (self-consistency/regression) was taken — NO
  external Wolfram ground truth is available in-repo (§G.0 places the Wolfram
  workflow outside the repository, and no `.manual/` fixture supplies the recorded
  converged parameters / final χ²). The tests generate synthetic targets from the
  real `OpticalSystemSolver`, perturb, and assert LM recovery; no fabricated numbers
  are presented as Wolfram ground truth. If an operator later supplies a recorded
  Wolfram fixture under `.manual/`, path (a) can be added.
- **§G.10 long-running job runner** is the Part J §J.10 harness (slice 016); this
  slice supplies only the per-iteration progress payload and a single cancellable run.
- **`NelderMead` simplex backend** remains the carried-forward open decision from
  slice 009 (G.2 surfaces `Failed "simplex backend unresolved"`); the global methods
  here (SA/GA) are independent of it.
- **ALGLIB scale tuning** (`minlmsetscale`) for meter-scale variables is not added to
  the adapter this round (see Gotchas); the Ψ test conditions its variable in nm.
- **Reuse-critic F2 / F3 (attempt-02, left as Deferred per the code-judge).** F2: the
  `thicknessMeters` `Thickness`-unwrap is duplicated between `Synthesis.fs` and
  `OptimizationTests.fs` (differing only in the `Infinity` arm); a first-class
  `Thickness.inMeters` accessor in `Media.fs` is out of this slice's scope (§0 #6).
  F3: the `glass`/`mkSystem`/`reflectanceOf`/`diagonalLight`/`ssr` test fixtures are
  duplicated across `LocalRefinementTests`, `SynthesisFitPageTests`, and (cross-project)
  `OptimizationTests`; extracting a shared test-support module is a maintenance-hygiene
  cleanup for a later slice.
- **Singular-`JᵀJ` guard (attempt-02, Deferred).** `covarianceMatrix` calls
  `RealMatrix.inverse` without a rank/condition guard; a rank-deficient design
  (degenerate or duplicated parameters) would surface as a non-finite covariance
  rather than a typed error. Out of scope for the minimum implementation (§0 #6);
  a future slice can add a guarded pseudo-inverse if the UI exposes such fits.

## Gotchas

- **Engine Ψ uses the reciprocal p/s convention.** `psiDelta` reads ρ off
  `reflected.amplitudeP/amplitudeS`, which equals the reciprocal of the textbook
  Fresnel |r_p/r_s| (solver Ψ = π/2 − analytic Ψ for the bare interface). AC-F5
  asserts tan Ψ matches the analytic ratio OR its reciprocal.
- **ALGLIB `minlm` is poorly conditioned for meter-scale (~1e-7) design variables.**
  The R/T thickness fit converges in canonical meters, but the more sensitive
  ellipsometric Ψ fit gets a noise-dominated finite-difference gradient and stalls at
  the meter scale. AC-G9-Ψ conditions the thickness `DesignParameter` in nanometres
  (O(100)); `getSys` still stores canonical meters via `Thickness.nm` (§0 #3 intact).
  A future slice may add `minlmsetscale` to the adapter so all callers can work in
  canonical meters.
- **`Softellect.Sys` is not transitive into `OpticalConstructor.Optimization`.** Even
  though the core `Berreman` project references it, an explicit PackageReference was
  required for the `.binz` serializer; `BinaryZippedFormat` is in
  `Softellect.Sys.Primitives`, `serialize`/`deserialize` in `Softellect.Sys.Core`.
- **`reflectanceOf`-style helpers hardcode normal-incidence vacuum light.** Off-normal
  / dense-medium (TIR, ellipsometric) checks must solve with the intended
  `IncidentLightInfo`, not a convenience helper built on `IncidentLightInfo.create`.
- **`.gitignore` already ignores `*.binz`** (line 336, slice 012 / Part I) — the rule
  was NOT duplicated; the fit-history sidecar path is already covered.
- **`Assert.Equal(double, double, digits)` caps `digits` at 15**, and `interface` is a
  reserved F# keyword (cannot be an identifier).

## Changelog

- 2026-06-01 — Slice 011 attempt-02 (code-judge route-back): fixed the
  `FitQuality.residualJacobian` forward-difference step to a per-parameter relative
  `√eps · max(|x|, 1e-12)` (the old `1e-7 · max(1, |x|)` floor perturbed canonical
  meter-scale thicknesses by ~40 %, corrupting the covariance/CI/correlation);
  added a mis-scaled-Jacobian regression test (RED against the old step, GREEN with
  the fix); consolidated the triplicated `sumSq` into one `internal` helper in
  `OptimizationInterface` (reuse-critic F1); recorded the §G.9 needle
  post-insertion-merit gradient interpretation; left F2/F3 and the singular-`JᵀJ`
  guard as Deferred. Gates: build 0 errors; BerremanTests 84 passed; constructor 124
  passed.
- 2026-06-01 — Slice 011 (Part G §G.8–G.11): added `FitQuality.fs` and `Synthesis.fs`
  to `OpticalConstructor.Optimization`, `SynthesisFitPage.fs` to
  `OpticalConstructor.Ui`, `OptimizationTests.fs` to `BerremanTests`, and
  `SynthesisFitPageTests.fs` to `OpticalConstructor.Tests`; registered them in their
  `.fsproj`s, added `Softellect.Sys` to the Optimization project, and a
  `BerremanTests`→`OpticalConstructor.Optimization` ProjectReference. Implemented the
  [Standard] fit-quality `FitReport` (covariance via the engine math seam, `.binz`
  sidecar), [Standard] needle/tunnelling synthesis + simulated-annealing / genetic
  global optimization (cancellable), and the Avalonia-free Synthesis/Fit operator
  page over the six-item §G.10 checklist. Validated §G.11 via path (b)
  self-consistency LM recovery (R/T and Ψ) over the real solver, plus AC-G3, AC-G6,
  and the AC-F3/F5/F7/F8 equivalence cross-checks. All three gates green (build 0
  errors; unit-tests 84 passed; constructor-unit-tests 123 passed).
