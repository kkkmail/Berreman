# 011 — impl-plan (Part G §G.8–G.11)

## Approach

Close Part G on top of the slice-009 (§G.1–G.3) infra and slice-010 (§G.4–G.7)
fitting core. Four new files, three `.fsproj` registrations, one enabling
`ProjectReference`.

### §G.11 / AC-G9 path decision — **path (b), self-consistency / regression**

No operator-supplied Wolfram fixture exists under `specs/0022/.manual/` (that
folder holds only spec-authoring discussion files). §G.0 places the Wolfram
workflow outside the repo. Therefore I take **path (b)**: generate a synthetic
target from the REAL `OpticalSystemSolver` forward model, perturb the
parameters, and assert `LocalRefinement` + `LevenbergMarquardt` recovers them
to a stated tolerance over the full `DesignParameters`→`MeritFunction`→
`optimize`→`AlglibAdapter`→`OpticalSystemSolver` path. No invented numbers are
presented as Wolfram ground truth. Documented in impl-log + SoW.

## Files

New:
- `OpticalConstructor.Optimization/FitQuality.fs` — `FitReport` (χ²/MSE, reduced
  χ², covariance, std errors, 95% CIs, correlation matrix, per-sample residuals);
  numerical residual Jacobian at the solution; covariance = reducedχ²·(JᵀJ)⁻¹ via
  the **`MathNetNumericsMath` `RealMatrix.inverse` seam** (NOT ALGLIB); `.binz`
  persistence via `Softellect.Sys.Core.serialize BinaryZippedFormat`.
- `OpticalConstructor.Optimization/Synthesis.fs` — `needleInsertion` (insert one
  thin `Layer` at the merit-minimizing position, then re-refine via §G.5),
  `tunnel` (perturb a converged design + re-refine), `simulatedAnnealing` and
  `geneticAlgorithm` as explicit F# loops over the G.1 `Residual` closure; all
  observe a `shouldCancel : unit -> bool` flag (AC-G7 for the G.9 loops).
- `OpticalConstructor.Ui/SynthesisFitPage.fs` — Avalonia-free MVU model/update
  (per P3, no Avalonia type) covering the six-item §G.10 checklist: nav entry,
  per-iteration progress payload, one-click non-destructive Start, confirm-gated
  Accept, `plotComparison` fit-vs-measured overlay refresh, distinct Cancel +
  Revert. Long-running execution / job runner is Part J §J.10 (slice 016); this
  page supplies only the progress payload + a single cancellable run.
- `BerremanTests/OptimizationTests.fs` — AC-G9 (self-consistency LM), AC-G3
  (bounded fit, no negative thickness), AC-G6 (needle grows films by one, merit
  non-increasing), and the AC-F3/F5/F7/F8 cross-checks (the full equivalence
  suite `AnalysisFunctionsTests.fs:20-22` defers here). No UI dependency.

Edited:
- `OpticalConstructor.Optimization.fsproj` — add `FitQuality.fs`, `Synthesis.fs`
  after the slice-010 files.
- `OpticalConstructor.Ui.fsproj` — add `SynthesisFitPage.fs`.
- `BerremanTests.fsproj` — add `<Compile Include="OptimizationTests.fs">` AND a
  `ProjectReference` to `OpticalConstructor.Optimization` (required so the
  AC-G9/G3/G6 tests can drive `LocalRefinement`/`Synthesis`; BerremanTests
  previously referenced only OpticalProperties + Analytics).
- `OpticalConstructor.Tests.fsproj` — add a new `SynthesisFitPageTests.fs`
  registration for AC-G5/G7/G8 (constructor-unit-tests gate; this project already
  references Ui + Optimization). AC-G7/G8 are model/`update`-level (no Avalonia).

NOT edited:
- `.gitignore` — already ignores `*.binz` at line 336 (owned by slice 012 /
  Part I). The fit-history sidecar path is covered; the rule is NOT duplicated.

## Retry round (attempt 02) — code-judge route-back

The cycle-1 code-judge routed back on ONE finding: `residualJacobian`
(`FitQuality.fs:72`) used `h = 1.0e-7 * (max 1.0 (abs x.[p]))`, whose `1.0`
floor forces a ~40 % perturbation of a canonical meter-scale thickness
(~2.5e-7 m), so the covariance / std-errors / CIs / correlation (the substance
of R-1 / AC-G5) are numerically meaningless in the project's canonical regime
(§0 #3). The defect was latent: AC-G5 only checks the CI brackets the estimate
and the correlation diagonal is 1, which any non-negative σ satisfies.

Fix this round:
1. **Per-parameter relative step.** `h_p = √eps · max(|x_p|, 1e-12)` — scaled to
   each parameter's own magnitude, no absolute `1.0` (meter-scale) floor. The
   `1e-12` floor only keeps the step nonzero for an exactly-zero parameter
   (sub-picometre, 5+ orders below any canonical thickness) — it never perturbs
   the step for a real fit.
2. **Regression test that catches a mis-scaled Jacobian** (`SynthesisFitPageTests.fs`):
   a NONLINEAR meter-scale residual with a closed-form Jacobian and standard
   error; asserts `residualJacobian` matches the analytic derivative (rel < 1e-5)
   AND `reportFrom`'s standard error matches its analytic closed form (rel < 1e-3)
   with a finite, strictly-positive covariance. Verified RED against the old step
   (6.1 % Jacobian error), GREEN with the fix.
3. **`sumSq` consolidated** (reuse-critic F1): one `internal sumSq` in
   `OptimizationInterface`; `AlglibAdapter` / `FitQuality` / `Synthesis` all call it.
4. **Needle gradient interpretation recorded** (see SoW Architecture / Gotchas).
5. F2/F3 (duplicated `thicknessMeters` / test fixtures) and the singular-`JᵀJ`
   guard left as **Deferred** notes per the judge's steering.

## Risks
- AC-G6 "post-insertion merit ≤ pre-insertion": guaranteed by inserting a
  vanishingly-thin needle (≈ optically absent → merit ≈ pre) then re-refining
  the grown design (LM only decreases). Test fixture uses a target a single film
  cannot hit but two can, so the drop is strict.
- Covariance seam: build J and Jᵀ as `RealMatrix`, route the multiply + inverse
  through the seam (§A.8 forbids ALGLIB here).
- SA/GA determinism: seeded `System.Random`; tests assert convergence trend, not
  exact values.
