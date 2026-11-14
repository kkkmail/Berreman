# Step 0403 — impl plan

## Approach

Production (`StudyEngine.fs`, after `realizabilityWith`): one member,
`type StagedStudy with member this.resultsWith context prefits data solver solution watch : Result<FitRunResults, StudyFitError>`.

- `engineProblemsWith` (step 402) gives the engine's problems, run residuals and product checks.
- `realizabilityWith context solution.layout solution.fitted` gives the black boxes' realizability.
- `sigmas` is `sigmasFor solution.layout`; `problems` each problem's `quality`, in order.
- `profiles`: for each `suspectsWith this.study.settings.suspect solution.layout`, in order, the suspect's problem's
  `profileAt coordinate earlier watch` (step 397), `earlier` the sum of the iterations of the profiles before it,
  starting at 0. A `ProfileCancelledErr reason` answers `FitCancelledErr reason`. No suspect, no re-fit.

No cache, retry or option. Nothing is written.

## Tests (`StudyEngineTests`, five facts)

A test-local Malus intensity study: the HeNe item, LP1 (the ideal-LP preset, fixed, at R1 0°) as prep, a new fixed
ideal-LP analyzer item (draft 18), and the Phone draft's intensity detector with test-specific gain/dark states.

- One setup of the four, built through the file's `benchOf` (as `SetupTests` builds slots: `withItem`).
- One Science run, the default session, gain model free, scanning the analyzer's `VaryR1` over nine readings.
- Its points: an `IntensityData` series answered by the file's test-local `dataOver` proxy. Point i is
  2 · S₀ + 0.1 + 0.002 · (−1)ⁱ, S₀ from `ExperimentRun.read Intensity` at unit scale over the bound path's steps.
- Staged `JointStage` / `CalibrationHeldFixed`, fitted by `fitNonlinear` unwatched, results by `resultsWith`.

Facts:

1. Identifiable gain/dark offset over 0..160° by 20°: rank 2, both σ below a tenth of span, ρ = −√(2/3) ± 1e-6, no
   suspect, no profile.
2. Exactly confounded gain/source scale (dark known 0.1): rank 1, σ None, no correlation, both suspect with
   `UndeterminedSigmaReason`. The gain profile is 1, 1.5, …, 4, flat at the fit's χ² within 1e-9 relative. The scale
   profile is seven points across [0.25, 4], each converged.
3. Nearly confounded over 0..20° by 2.5°: ρ < −0.95, each suspect by `CorrelatedEntryReason` naming the other. Each
   profile is x̂ + jσ, its middle the fit's χ², each χ² equal to χ²_min · (1 + j²/7) within 1e-6 relative.
4. Fact 2's solution under an already-cancelled watch answers `FitCancelledErr`.
5. Step 390's series fixture under `tryCreate 1.0 1.0e9`: one σ per entry, one problem per object solve, one run
   residual per fitted run, QZ/LR realizability, the [QZ; LR] product check, no profile.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs`

## Risks

- StudyEngineTests is the constructor suite's critical path (≈134 s of 180 s). The new studies are linear and only
  nine points each, so they should cost a second or two each.
- The flatness (1e-9) and quadratic-profile (1e-6) tolerances rest on ALGLIB's epsX 1e-12 convergence.
- The scale profile holds scale at 0.25, which needs a gain of 8 beyond the [1, 4] bound. That re-fit ends on the bound
  and must still answer `ProfileConverged`.
