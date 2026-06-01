# 010 — Fitting core — impl-log

## Progress

- [x] `Ellipsometry.fs` (§G.6) — `EllipsometricFunction` DU + evaluator sharing §F.5 `psiDelta`
- [x] `MeritFunction.fs` (§G.4) — `FitTarget`, `modelValue` via `Solution.func`, weighted-LSQ residual
- [x] `LocalRefinement.fs` (§G.5) — `refine`/`refineWith`, LM default, fold-back through G.3
- [x] `InverseFit.fs` (§G.7) — CSV/tab import via FSharp.Data → FitTargets → refine
- [x] `LocalRefinementTests.fs` — AC-G3, AC-G4 (+ Ψ/Δ sharing check)
- [x] fsproj registration + FSharp.Data package
- [x] gates: build, unit-tests, constructor-unit-tests — all green
- [x] **Cycle-1 route-back:** ellipsometric `FitTarget` through buildResidual/refine
      test; `MeasuredEllipsometric` CSV-path test; F2 fixture reuse
      (`IncidentLightInfo.create`/`createInclined`); F1/silent-0.0/RangedVariable
      divergence/ellipsometric-polarization documented in Gotchas
- [x] **Cycle-2 route-back:** dropped the isomorphic `InverseFit.MeasuredQuantity`
      DU; `parseMeasurementCsv`/`invertFromCsv` now take `MeritFunction.TargetQuantity`
      directly (kept as a `MeasuredQuantity` *type alias* for caller clarity);
      removed the 1:1 translation match; switched the four test constructors to
      `Photometric`/`Ellipsometric`

## Files modified

New:
- `OpticalConstructor/OpticalConstructor.Optimization/Ellipsometry.fs`
- `OpticalConstructor/OpticalConstructor.Optimization/MeritFunction.fs`
- `OpticalConstructor/OpticalConstructor.Optimization/LocalRefinement.fs`
- `OpticalConstructor/OpticalConstructor.Optimization/InverseFit.fs`
- `OpticalConstructor/OpticalConstructor.Tests/LocalRefinementTests.fs`

Edited:
- `OpticalConstructor/OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj`
  — registered the four files in compile order (Ellipsometry → MeritFunction →
  LocalRefinement → InverseFit) and added `FSharp.Data` 8.1.14.
- `OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
  — registered `LocalRefinementTests.fs`.

## Decisions

- **Compile order Ellipsometry before MeritFunction.** R-3 requires `FitTarget`
  to reach the Ψ/Δ cases, so `EllipsometricFunction` (Ellipsometry.fs) must
  compile before `FitTarget` (MeritFunction.fs). The spec allows either order
  for the two; this is the only one that compiles.
- **`refine` takes the initial design-variable vector explicitly.** The G.3
  `DesignParameter` (slice 009) carries `getSys` but no value getter, and
  `DesignParameters.fs` is out of this slice's scope to edit, while the
  optimizer requires a start point. `refine`/`refineWith` therefore accept
  `initial : float[]`. Recorded as the spec is silent on where the start vector
  comes from.
- **§G.6 shares §F.5, does not fork.** `EllipsometricFunction.evaluate` delegates
  to `Analytics.AnalysisFunctions.psiDelta` (slice 008), which reads ρ = r_p/r_s
  off `EmFieldSystem.reflected.amplitudeP/amplitudeS`. The engine's closed
  `OpticalFunction` is NOT edited; the Mueller path is NOT revived.
- **Inequality flow (R-1 ∧ R-2).** The per-scalar one-sided hinge is composed
  into the residual vector (R-1, matching slice 009's explicit guidance that a
  target on a specific model scalar belongs in the `Residual` closure). In
  addition, `refine` surfaces the FitTargets' inequality targets as a G.1
  `InequalityTarget list` and passes them to the request (R-2). The slice-009
  adapter treats those as a one-sided penalty on the residual L2 norm; at the
  optimum (hinge ≈ 0) that penalty row is inactive, so it reinforces rather than
  distorts. See Gotchas.
- **Inverse-fit unit seam.** Wavelength conversion at the CSV import boundary
  goes through the SOLE `OpticalConstructor.Domain.Units.toWaveLength` seam (§D.3)
  so the canonical-meter reduction is `WaveLength.value`. CSV parsing follows the
  `SpectralImport` precedent (`CsvFile.Parse` on text, no filesystem IO in the
  parser, errors as values).

## Cycle-1 route-back resolution

The code-judge cycle-1 verdict was `route-back-to-worker` on two triggers plus
secondary documentation gaps. All are addressed this round:

1. **Untested net-new ellipsometric surface (R-3/R-4).** Added two tests in
   `LocalRefinementTests.fs`:
   - `G6 an ellipsometric Psi FitTarget drives buildResidual and refine` — builds
     an `Ellipsometric Psi` `FitTarget`, asserts a non-trivial start residual
     through `buildResidual`, then `refine`s and asserts χ² drops. This is the
     first test driving the ellipsometric composition through the residual/refine
     path (the §G.6 sharing test only checked `Psi.evaluate` in isolation).
   - `G7 inverse fit drives the MeasuredEllipsometric CSV path` — parses a
     synthetic Ψ spectrum via `InverseFit.parseMeasurementCsv
     (MeasuredEllipsometric Psi)`, asserts each target maps to `Ellipsometric Psi`
     with sample points stored in meters, asserts the residual is well-posed
     (≈0 at the true thickness, strictly larger at the start), and drives
     `invertFromCsv` end-to-end to a non-negative refined thickness.
2. **Reuse-before-invention F1.** Routing call recorded: the `InverseFit`
   CSV-helper duplication of `SpectralImport` is **accepted** and documented (see
   Gotchas) with an in-code note at the helpers, rather than lifted — the proper
   lift requires editing the out-of-scope Storage `SpectralImport.fs` and pushing
   an FSharp.Data dependency into the conversion-only `Domain.Units` spine.
3. **F2 (test-only).** `LocalRefinementTests.fs` fixtures now call
   `IncidentLightInfo.create`/`createInclined` (`Fields.fs:375-391`) instead of a
   hand-rolled five-field record literal.
4. **Under-documented behaviour.** The silent-0.0 `modelValue` path and the
   `RangedVariable`→`IncidentLightInfo` sample-point divergence are now in
   Gotchas, alongside a new gotcha on the ellipsometric s+p-polarization
   requirement surfaced while writing the tests.

## Cycle-2 route-back resolution

The code-judge cycle-2 verdict was `route-back-to-worker` on a single concrete
change (reuse critic F1, new): `InverseFit.MeasuredQuantity` restated
`MeritFunction.TargetQuantity` case-for-case over the same two payload types and
paid for it with a pure 1:1 rename arm in `parseMeasurementCsv` — an untraced DU
against §0 #2 (reuse before invention). Addressed:

- **Dropped the second isomorphic DU.** `InverseFit.fs` no longer declares
  `MeasuredQuantity = MeasuredPhotometric of OpticalFunction | MeasuredEllipsometric
  of EllipsometricFunction`. `MeasuredQuantity` is now a **type alias** for
  `MeritFunction.TargetQuantity` (`InverseFit` already `open`s `MeritFunction`),
  keeping the `Measured…` naming at the call site without inventing a new type —
  exactly the alias the judge named as acceptable.
- **Removed the 1:1 translation match.** `parseMeasurementCsv` no longer maps
  `MeasuredPhotometric f -> Photometric f | MeasuredEllipsometric e -> Ellipsometric
  e`; the incoming `quantity` is bound straight into `FitTarget.quantity`.
- **Switched the four test constructors** in `LocalRefinementTests.fs` (the
  `parseMeasurementCsv`/`invertFromCsv` call sites) from
  `InverseFit.MeasuredPhotometric R`/`InverseFit.MeasuredEllipsometric Psi` onto the
  `MeritFunction` cases `Photometric R`/`Ellipsometric Psi` (already in scope via the
  test's `open MeritFunction`).
- **Left untouched** (explicitly out of scope for this re-spawn): the per-target
  `applyVector` system-rebuild efficiency note (architecture critic, non-gating) and
  the already-accepted `InverseFit`↔`SpectralImport` CSV-helper duplication (cycle-1
  F1).

### Discovery while testing: pure-s default polarization collapses Ψ

`Polarization.defaultValue` is `Polarization.s` (`Fields.fs:321-322`). With
pure-s incidence on isotropic media the reflected p-amplitude is 0, so
ρ = r_p/r_s ≡ 0 and Ψ ≡ 0 regardless of the stack — the first ellipsometric test
drafts failed with χ² = 0. The fix is to drive the ellipsometric tests with 45°
linear polarization (equal s/p) so ρ depends on the regressed thickness. Recorded
in Gotchas as a constraint on any ellipsometric caller, not just the tests.

## Gotchas

See the state-of-the-world `Gotchas` section for the full list. The
route-back-relevant entries added this round:

- **Accepted F1 duplication** — `InverseFit` CSV helpers mirror `SpectralImport`;
  consolidation would cross project boundaries this slice does not own. Accepted,
  with an in-code note and the lagging `UnitOfMeasure` ladder gap recorded.
- **Silent-0.0 `modelValue`** — `MeritFunction.fs:72` maps an unevaluable
  photometric `Solution.func` `None` to 0.0 to keep the residual finite; no
  diagnostic surface (fit-quality is slice 011 §G.8). Ellipsometric has no such
  silent path.
- **`RangedVariable`→`IncidentLightInfo` divergence** — the sample point is stored
  as the `IncidentLightInfo` the solver consumes directly, not a `RangedVariable`
  index, avoiding an indirection (§0 constraint 6) and carrying the polarization
  the ellipsometric path needs.
- **Ellipsometric targets need s+p polarization** — see the discovery note above.

## Testing state

All three gates pass in the local run (cycle-1 fixes applied):
- `build`: `dotnet build Berreman.slnx -c Release` → Build succeeded, 0 errors.
- `constructor-unit-tests`: `OpticalConstructor.Tests` → 118 passed, 0 failed,
  0 skipped (slice-009 baseline 113 → +5: AC-G3, AC-G4, §G.6 Ψ/Δ sharing, the
  ellipsometric `FitTarget` buildResidual/refine test, and the
  `MeasuredEllipsometric` CSV-path test).
- `unit-tests`: `BerremanTests` → 76 passed, 5 skipped, 0 failed (baseline 76;
  this slice does not touch `BerremanTests`).

AC-G3 proved: a bounded LM refinement (lower bound 0) of a glass-film thickness
against an `R ≤ 0.5%` inequality target returns a system with no negative
`Thickness` and drives the hinge to ~0. AC-G4 proved: parsing a measured-R CSV
stores each sample point in meters (`WaveLength.value`) and the inverse fit
reduces χ² (SSR of the merit residual) below its initial value.

## Artifacts

No persistent run artifacts beyond the gate stdout (build/test output) were
produced; the fitting core is exercised entirely by in-process unit tests.
