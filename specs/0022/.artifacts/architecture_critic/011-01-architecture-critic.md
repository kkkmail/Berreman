# Architecture critique -- 011.slice-md cycle 2

## Summary

Cleaner than cycle 1, and the route-back was handled with discipline. The
cycle-1 code-judge sent this back on exactly one numerical-correctness finding —
the mis-scaled `FitQuality.residualJacobian` forward-difference step — and the
attempt-02 diff fixes precisely that, adds a real regression test that fails
against the old step, and folds in the reuse-critic's `sumSq` consolidation,
without touching anything outside the judge's steering. No new structural debt
was introduced. The remaining notes (singular `JᵀJ`, two reuse duplications, the
`Ellipsometric`-blind overlay, untested SA/GA convergence) are the same
non-blocking quality items cycle 1 already raised; most are now explicitly parked
in `Deferred`, which is the correct disposition for a minimum-implementation
slice (§0 #6).

## Consistency

The `sumSq` consolidation (reuse-critic F1) is the right move and is done
correctly. The single `let internal sumSq` now lives in
`OptimizationInterface.fs:25`, and the three byte-identical `private` copies in
`AlglibAdapter`, `FitQuality`, and `Synthesis` are gone — each of those modules
already `open`s `OptimizationInterface`, and all three compile into the one
`OpticalConstructor.Optimization` assembly, so `internal` is the right visibility
(narrower than `public`, still reachable by the siblings; the green build
confirms it). This genuinely removes a drift hazard: the χ² of the report, the
synthesis merit, and the adapter's scalar objective are now the *same* definition
of Σ residualᵢ² by construction, which matters because AC-G6's "post-insertion
merit ≤ pre-insertion merit" assertion is only meaningful if `systemMerit` and
the optimizer minimise an identical scalar. Naming, the `Result<_, string>`
convention, and the closed-`TerminationReason` reuse all remain consistent with
slices 009/010.

## Spec fit

The fix matches R-1 as the judge scoped it. `residualJacobian`
(`FitQuality.fs:85-98`) now uses `h_p = √eps · max(|x_p|, 1e-12)` — a
per-parameter *relative* step with the floor demoted to a sub-picometre
zero-guard, five-plus orders below any canonical thickness (~1e-7 m), so it never
perturbs a real fit. That is the textbook forward-difference optimum (≈√eps·scale)
and it restores a meaningful covariance at the project's canonical meter scale
(§0 #3). The cycle-1 interpretation asks were also closed: the §G.9 needle
"gradient ≈ post-insertion merit per candidate depth" reading is now recorded in
both the impl-log Gotchas and the SoW Architecture section, so it is presented as
a documented interpretation rather than as the literal gradient method.

## Evolvability

The regression test added this round (`SynthesisFitPageTests.fs:111-147`) is
better engineering than a placeholder: a *nonlinear* residual sin(ωx)−d at a
2.5e-7 m parameter with a closed-form Jacobian and a closed-form standard error,
asserting both rel < 1e-5 and rel < 1e-3. Crucially it is sensitive to the step
scaling where AC-G5 (CI brackets the estimate, unit diagonal) is not — so the
defect the judge caught can no longer regress silently. That is the right test to
have left behind.

The `thicknessMeters` duplication (reuse-critic F2) is still live and the two
copies have now visibly *diverged*: `Synthesis.fs:56-59` maps `Infinity` to a
caller-supplied `fallback`, while `OptimizationTests.fs:70-73` maps it to
`infinity`. Today both call sites only ever see finite films so the divergence is
inert, but this is exactly the drift a shared `Thickness.inMeters` on `Media.fs`
would prevent. It is correctly parked in `Deferred` as out-of-scope for §0 #6;
the divergence is worth a one-line note so a later slice extracting the accessor
knows the two arms differ.

## Risks

- **Singular `JᵀJ` still unguarded (carried).** `covarianceMatrix`
  (`FitQuality.fs:102-111`) still calls `(jtM * jM).inverse` with no rank or
  condition check. The single-parameter suite never exercises a rank-deficient
  normal matrix, so this stays latent — but a correlated multi-parameter fit (the
  precise scenario the correlation matrix exists to report) can produce a
  near-singular `JᵀJ` and cascade NaN/Inf into the report. The improvement over
  cycle 1 is that it is now explicitly recorded in `Deferred` with the same
  reasoning I gave, which is the disposition I asked for; flagging it again only
  to note it remains the most realistic robustness gap in the shipped covariance
  path.
- **SA/GA convergence still unasserted (carried).** `simulatedAnnealing` /
  `geneticAlgorithm` are tested only for the `shouldCancel` early-exit and
  iteration count (`SynthesisFitPageTests.fs:185-208`); no test asserts either
  actually descends the merit landscape. This is acceptable against the declared
  ACs (none demand global-optimum recovery), but the two global methods ship
  without a convergence regression, so a future break in the Metropolis accept
  rule or the GA tournament/elitism would pass the gate unnoticed. Unlike the
  singular-matrix note, this one is not captured in `Deferred`.
- **`comparisonOverlay` drops `Ellipsometric` channels (carried, undocumented).**
  `overlayFunctions` (`SynthesisFitPage.fs:212-215`) `List.choose`s only
  `Photometric` and pins both overlaid systems to the first target's
  `samplePoint` (`:225-232`). An ellipsometric-only fit therefore renders an
  empty overlay and a multi-angle set collapses to one angle. This is partly
  forced by `plotComparison` taking only `OpticalFunction` (no Ψ/Δ charting
  primitive exists yet), so it corners no future slice structurally — but it is
  still the one cycle-1 finding that was neither addressed nor written into
  `Deferred`, and whoever wires the Ψ/Δ chart will hit the silent gap.

## Bottom line

I would ship this. The route-back was the load-bearing question and it is
answered correctly — the FD step is now properly scaled for the canonical regime,
the fix is pinned by a test that genuinely fails without it, and the change set
stayed inside the judge's steering instead of sprawling. The `sumSq`
consolidation is a real consistency gain. Everything else is the carried-over
quality tail from cycle 1: the singular-`JᵀJ` guard and the F2/F3 duplications
are now honestly parked in `Deferred`, and the two items still *not* documented
(SA/GA convergence coverage and the `Ellipsometric`-blind overlay) are
robustness/coverage notes rather than correctness blockers. All three gates are
green and the §G.11 path-(b) decision remains documented with no fabricated
Wolfram numbers. No structural objection from me; the judge can weigh whether the
two undocumented carry-overs warrant a one-line `Deferred` entry before close.
