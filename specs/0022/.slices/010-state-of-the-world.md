# 010 — Fitting core (Part G §G.4–G.7) — state of the world

## Where we are

Slice 010 lands the fitting core of Part G on top of the slice-009 optimization
infrastructure (§G.1 interface, §G.2 ALGLIB adapter, §G.3 design-variable
mapping). It adds the [Core] merit/target editor model and local-refinement
entry point, plus the [Standard] Ψ/Δ ellipsometric targets and inverse fitting
from measured CSV/tab data, all in the gated, UI-less
`OpticalConstructor.Optimization` / `OpticalConstructor.Tests` projects. Slice
011 (§G.8–G.11) adds fit-quality reporting, needle/tunnelling synthesis and
global optimization, the Synthesis/Fit operator page, and the Wolfram-reference
validation suite in `BerremanTests/OptimizationTests.fs`.

## What's working

- Add `Ellipsometry.fs` (§G.6): net-new `EllipsometricFunction` DU (`Psi | Delta`)
  whose evaluator SHARES the §F.5 `psiDelta` derivation (ρ = r_p/r_s on
  `EmFieldSystem.reflected`) rather than forking it or editing the engine's
  closed `OpticalFunction`.
- Add `MeritFunction.fs` (§G.4): `FitTarget` (quantity, sample-point
  `IncidentLightInfo`, desired value, weight, tolerance, `Equality | Inequality`
  kind); `modelValue` reads the SAME `Solution.func` evaluator the charts use;
  `buildResidual` assembles the weighted-LSQ residual satisfying the G.1
  `Residual` signature directly.
- Add `LocalRefinement.fs` (§G.5): `refine` (Levenberg–Marquardt default) /
  `refineWith` (quasi-Newton and simplex selectable); passes G.3 bounds and
  inequality targets through to `optimize` and folds the solution back through
  the G.3 mapping. No thread spawned.
- Add `InverseFit.fs` (§G.7): measured R/T and Ψ,Δ CSV/tab import via FSharp.Data,
  converting wavelength to canonical meters through the sole `Units.toWaveLength`
  seam, then handing the `FitTarget` list to local refinement.
- Register the four files (Ellipsometry → MeritFunction → LocalRefinement →
  InverseFit) and `FSharp.Data` in the Optimization `.fsproj`, and add
  `LocalRefinementTests.fs` to the Tests `.fsproj`.
- Prove AC-G3 (bounded refinement: no negative thickness, inequality hinge to
  zero) and AC-G4 (inverse fit reduces χ² with sample points stored in meters).
- Cover the net-new ellipsometric surface (cycle-1 route-back): a Ψ `FitTarget`
  driven through `buildResidual` + `refine` (χ² drops), and the
  `MeasuredEllipsometric` CSV path in `InverseFit` parsed, mapped to
  `Ellipsometric` targets in meters, and driven through `invertFromCsv`.
- Reuse the engine's `IncidentLightInfo.create`/`createInclined` constructors in
  the test fixtures instead of a hand-rolled record literal (reuse critic F2).
- Drop the isomorphic `InverseFit.MeasuredQuantity` DU (cycle-2 route-back, reuse
  critic F1): consume `MeritFunction.TargetQuantity` directly through a type alias
  and bind it straight into `FitTarget.quantity`, removing the 1:1 translation match.

## Tests

- `build` gate: PASS — `dotnet build Berreman.slnx -c Release`, 0 errors. The
  four new Optimization files compile in dependency order; `FSharp.Data` 8.1.14
  (already used by `OpticalConstructor.Storage`) resolved; no new C# project.
- `constructor-unit-tests` gate (`OpticalConstructor.Tests`): PASS — 118 passed,
  0 failed, 0 skipped (slice-009 baseline 113 → +5 in `LocalRefinementTests`:
  AC-G3, AC-G4, a §G.6 Ψ/Δ sharing check, an ellipsometric Ψ `FitTarget` through
  buildResidual/refine, and the `MeasuredEllipsometric` CSV path).
- `unit-tests` gate (`BerremanTests`): PASS — 76 passed, 5 skipped, 0 failed
  (baseline 76; this slice does not touch `BerremanTests`).

```yaml
gates:
  berreman_unit_tests:    76
  constructor_unit_tests: 118
```

## Architecture

- **One evaluator for fit and chart (§G.4).** `MeritFunction.modelValue` runs
  `OpticalSystemSolver` on the G.3-mapped system and reads `Solution.func`
  (`FieldFunctions.fs:192`) — the same evaluator the charts use — so the fit and
  the chart can never diverge. A parallel evaluator was deliberately NOT written.
- **Ψ/Δ shared from §F.5, not forked (§G.6).** `EllipsometricFunction.evaluate`
  delegates to `Analytics.AnalysisFunctions.psiDelta` (slice 008). The engine's
  closed `OpticalFunction` DU is untouched and the Mueller path
  (`FieldFunctions.fs:93`, Part F §F.3) is not revived. Compile order puts
  `Ellipsometry.fs` before `MeritFunction.fs` because `FitTarget` references
  `EllipsometricFunction` (R-3).
- **Residual satisfies G.1 directly (§G.4/§G.5).** `buildResidual` returns a
  `float[] -> float[]` closure handed unchanged to `AlglibAdapter.optimize`;
  equality terms are `weight·(model−desired)/max(tol,ε)`, inequality terms are a
  one-sided hinge. Robust normal-equation handling stays inside ALGLIB's `minlm`
  (§G.2); none is re-implemented on the math seam.
- **Bounds and inequality flow (§G.5).** G.3 `ParameterBounds` (lower bound 0 for
  thickness) are passed to the request — that bound IS the negative-thickness
  prohibition, not a separate validator. The FitTargets' inequality targets are
  surfaced as G.1 `InequalityTarget`s on the request in addition to the in-residual
  hinge.
- **Inverse-fit boundary (§G.7).** CSV/tab parsed with `FSharp.Data.CsvFile`
  following the `SpectralImport` precedent (text in, errors as values);
  wavelength reduced to canonical meters only at the import boundary via the sole
  `Units.toWaveLength` seam (§D.3). No measurement-format registry/autodetection.
- **One quantity DU, reused not restated (§G.7, cycle-2).** `InverseFit` consumes
  `MeritFunction.TargetQuantity` (`Photometric | Ellipsometric`) directly as its
  `quantity` parameter — `MeasuredQuantity` is a `type` alias of it, not a second
  isomorphic union — so the measured datum binds straight into `FitTarget.quantity`
  with no translation arm (§0 #2, reuse before invention).

## Deferred

- Fit-quality reporting (χ²/MSE, CI, correlation, residual arrays, `FitReport`),
  needle/tunnelling synthesis and global optimization, the Synthesis/Fit operator
  UI page (progress channel, Cancel/Revert/refresh), and the Wolfram-reference
  validation in `BerremanTests/OptimizationTests.fs` → slice 011 (§G.8–G.11).
- Long-running execution and cancellation for refinement → §G.10 (slice 011) and
  the Part J §J.10 job runner (slice 016); §G.5 does not spawn a thread.
- The `NelderMead` simplex backend remains the carried-forward open deferred
  decision from slice 009 (G.2). `refineWith NelderMead …` is selectable but the
  adapter still surfaces `Failed "simplex backend unresolved"` until that backend
  is resolved; LM and quasi-Newton are the usable methods this round.
- Nonuniformity/roughness regression: expressible as additional `DesignParameter`
  closures (R-4) but no roughness physics is added here.

## Gotchas

- **`refine` needs an explicit initial vector.** The G.3 `DesignParameter`
  (slice 009) has no value getter and `DesignParameters.fs` is out of scope to
  edit, so `refine`/`refineWith` take `initial : float[]` as the optimizer start
  point. Callers that built the `DesignParameter` list know the current values.
- **Inequality is double-routed by design.** The per-scalar hinge lives in the
  residual vector (R-1, per slice 009's guidance that scalar targets belong in
  the `Residual` closure), and the same FitTargets also flow to the request as
  G.1 `InequalityTarget`s (R-2). The slice-009 adapter binds the latter to the
  residual L2 norm, not to a model scalar; at a satisfied optimum (hinge ≈ 0) the
  penalty row is inactive, so it reinforces convergence rather than distorting χ².
  Do not read the request's `InequalityTarget` bound as binding to one model
  quantity — that is the slice-009 trap.
- **Single dielectric film R has periodic minima.** AC-G3/AC-G4 fixtures use a
  vacuum/glass-film/vacuum stack whose normal-incidence R hits 0 at half-wave
  "absentee" thicknesses; the AC-G3 start point (150 nm) is chosen inside the
  basin of the ~197 nm minimum so LM converges. A start point a full period away
  could land in a different minimum — relevant when reusing the fixture.
- **`Thickness` has an `Infinity` case.** Pattern matches on `Layer.thickness`
  must cover `Thickness of float<meter>` AND `Infinity` (Media.fs:12–14), else
  `--warnaserror+:25` fails the build.
- **`modelValue` silently yields 0.0 for an unevaluable photometric quantity.**
  `MeritFunction.modelValue` (`MeritFunction.fs:72`) reads
  `solution.func f |> Option.defaultValue 0.0`: when the engine's `Solution.func`
  evaluator returns `None` for an `OpticalFunction` not defined for the solved
  configuration, the residual silently treats the model value as 0.0 rather than
  raising. This is deliberate — it keeps the residual total and finite so the
  optimizer never faults mid-run — but it means a mis-paired target (a quantity
  the configuration cannot produce) shows up as a *non-zero residual against 0*,
  not as an error. There is no diagnostic surface for this here; fit-quality
  reporting (which would flag it) is slice 011 §G.8. The ellipsometric branch has
  no such silent path — `EllipsometricFunction.evaluate` always returns a float.
- **Ellipsometric targets need s+p incident polarization.** Ψ = atan|ρ|,
  ρ = r_p/r_s is read off `EmFieldSystem.reflected.amplitudeP/amplitudeS`, which
  are the response to the *incident* polarization. The engine default
  (`IncidentLightInfo.create`/`createInclined` → `Polarization.defaultValue =
  Polarization.s`, `Fields.fs:321-322`) is pure s: on isotropic media the
  reflected p-amplitude is 0, so ρ ≡ 0 and Ψ ≡ 0 *independent of the stack*,
  which is useless as a fit target. The ellipsometric tests therefore set 45°
  linear polarization (equal s/p) so ρ = r_p/r_s actually depends on the
  regressed parameter. A UI/caller that wires ellipsometric targets must do the
  same; the default-light constructors are not suitable for Ψ/Δ fitting.
- **Sample point is an `IncidentLightInfo`, not a `RangedVariable` index
  (R-1 divergence).** R-1 lists the sample point as "a `RangedVariable` index or
  explicit `WaveLength`/`IncidenceAngle`". The implementation stores the sample
  point as an `IncidentLightInfo` (which carries wavelength, incidence angle,
  refraction index, polarization, ellipticity) because that is exactly what
  `OpticalSystemSolver` consumes — a `RangedVariable` index would have to be
  resolved back into a light configuration before solving, adding an indirection
  the spec's §0 constraint 6 ("minimum implementation") discourages. The
  `IncidentLightInfo` is the more direct, strictly-more-expressive representation
  (it also carries the polarization the ellipsometric path requires, see above).
  Recorded per the project prompt's "record the choice you made" rule.
- **Accepted duplication: `InverseFit` CSV helpers mirror `SpectralImport`
  (reuse critic F1).** `InverseFit.fs`'s `inv`/`tryFloat`/`unitFromHeader`
  (lines ~46-59) duplicate `SpectralImport.fs:25-38`. Consolidating them would
  require either an `Optimization → Storage` project reference (which does not
  exist and would invert the dependency direction) or lifting the FSharp.Data
  CSV-row scaffolding into the `Domain.Units` spine — a conversion-only module
  with no FSharp.Data dependency — *and* editing `SpectralImport.fs`, which this
  slice does not own (Storage is a different slice's scope, with its own tests).
  The duplication is two ~4-line helpers; it is **accepted** this round and
  flagged with an in-code note at the top of the helpers. The shared boundary-unit
  ladder lagging `Units.UnitOfMeasure` (no mm/eV/cm⁻¹ header recognition) is the
  SAME pre-existing gap in both copies — it is left to a future Domain-level
  consolidation, not widened here.

## Changelog

- 2026-06-01 — Slice 010 (Part G §G.4–G.7): added `Ellipsometry.fs`,
  `MeritFunction.fs`, `LocalRefinement.fs`, `InverseFit.fs` to
  `OpticalConstructor.Optimization` and `LocalRefinementTests.fs` to
  `OpticalConstructor.Tests`; registered them and `FSharp.Data` 8.1.14 in their
  `.fsproj`s. The [Core] merit/target model (weighted-LSQ residual through the
  chart's `Solution.func`), the [Core] LM-default local-refinement entry point
  (folding the solution back through the §G.3 mapping), the [Standard] Ψ/Δ
  ellipsometric targets (sharing §F.5 `psiDelta`), and [Standard] inverse fitting
  from measured CSV/tab data are implemented and unit-tested. All three gates
  green (build 0 errors; constructor-unit-tests 116 passed; unit-tests 76 passed).
- 2026-06-01 — Cycle-1 route-back fixes: added two tests covering the net-new
  ellipsometric surface (a Ψ `FitTarget` through buildResidual/refine, and the
  `MeasuredEllipsometric` CSV path through `invertFromCsv`); switched the test
  fixtures onto `IncidentLightInfo.create`/`createInclined` (reuse critic F2);
  recorded the accepted `InverseFit`↔`SpectralImport` CSV-helper duplication
  (reuse critic F1, with an in-code note), the silent-0.0 `modelValue` behaviour,
  the `RangedVariable`→`IncidentLightInfo` sample-point divergence, and the
  ellipsometric s+p-polarization requirement in Gotchas. constructor-unit-tests
  116 → 118; build and unit-tests still green.
- 2026-06-01 — Cycle-2 route-back fix (reuse critic F1): dropped the isomorphic
  `InverseFit.MeasuredQuantity` DU and consumed `MeritFunction.TargetQuantity`
  directly (kept as a `MeasuredQuantity` type alias for caller clarity); removed
  the 1:1 translation match in `parseMeasurementCsv`, binding the datum straight
  into `FitTarget.quantity`; switched the four `LocalRefinementTests` constructors
  onto `Photometric`/`Ellipsometric`. The per-target `applyVector` efficiency note
  and the accepted CSV-helper duplication were left untouched (out of scope).
  constructor-unit-tests 118 (unchanged); all three gates green.
