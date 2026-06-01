# Architecture critique -- 010.slice-md cycle 3

## Summary

Clean; nothing new of substance landed since cycle 2 and no regressions. The
fitting core delivers the four owned files in correct compile order on a correct
dependency direction (Optimization → Domain only), routing everything through the
existing `Solution.func` / `psiDelta` / `AlglibAdapter.optimize` seams without
forking any of them. The one finding I would add beyond the cycle-2 record is a
coverage risk the prior critiques did not surface: the net-new **Δ** fit surface
ships with zero residual/fit coverage and carries a branch-cut hazard.

## Separation of concerns

The cycle-1/-2 efficiency finding persists, restated for the judge not
re-litigated. `MeritFunction.modelValue` (`MeritFunction.fs:64-73`) calls
`DesignParameters.applyVector parameters vector baseSystem` on every invocation,
and `buildResidual` (`MeritFunction.fs:87-94`) calls `modelValue` once per target.
The mapped system is a pure function of `vector` — re-confirmed against
`DesignParameters.fs`: every `getSys` (incl. `dispersionCoefficient`, which bakes
its `waveLength` at construction, `:72-91`) ignores the target's `samplePoint`.
So for a k-point fit the identical system is rebuilt k times per residual call,
and LM evaluates the residual n+1 times per iteration. Hoisting
`let system = applyVector parameters vector baseSystem` to the top of the
`buildResidual` closure, with a leaner per-target step doing only the
solve+read, separates build-from-solve and removes the waste. This is not the
merit cache §G.4 forbids and not the math-seam re-implementation §G.5 forbids —
it is a clarity clean-up on a gated-green path. The cycle-1 judge already
declined to gate on it; defensible to defer again.

## Spec fit

Full coverage, unchanged from cycle 2. All four files present with tier labels in
their module doc-comments ([Core] MeritFunction/LocalRefinement, [Standard]
Ellipsometry/InverseFit); both ACs proven; LM default with quasi-Newton/simplex
selectable; bounds and inequality targets flowed to the request; solution folded
back through the §G.3 `applyVector` mapping. No scope creep — no `FitReport`, no
synthesis, no UI page, no spawned thread; `NelderMead` stays deferred (selectable
but the adapter surfaces `Failed`). The deliberate `IncidentLightInfo` sample
point (vs R-1's `RangedVariable` index) remains the right, documented call — it
is exactly what `OpticalSystemSolver` consumes and carries the polarization the
ellipsometric path needs.

## Evolvability

Inequality targets are intentionally double-routed: each `Inequality` FitTarget
contributes an in-residual hinge (`MeritFunction.fs:81-82`) **and** is surfaced
again as a request-level `InequalityTarget` (`LocalRefinement.fs:37-39, 58`).
Both are spec-mandated (R-1 hinge, R-2 flow-through), but the slice-009 adapter
binds the request-level target to the **L2 norm of the whole residual vector**,
not to a model scalar (`AlglibAdapter.fs:62-68`). The maintainer consequence:
adding a target or changing any `weight`/`tolerance` shifts that norm and silently
moves the activation point of the request-level penalty row — whose bound
(`0.005` in AC-G3) is compared against a norm, not against R. The Gotcha "do not
read the request's `InequalityTarget` bound as binding to one model quantity"
captures this, but architecturally the request-level penalty is near-vestigial
here: real enforcement is the box bound (lower 0) plus the in-residual hinge. Fine
to ship; slice 011's inequality editor should not conflate the load-bearing
mechanism with the cosmetic one.

## Risks

**The net-new Δ fit surface ships with zero residual coverage and a branch-cut
hazard** — a finding the cycle-1/-2 critiques did not raise.
`EllipsometricFunction.Delta.evaluate` returns `r.Phase`
(`Ellipsometry.fs:33-37` via `AnalysisFunctions.psiDelta`,
`AnalysisFunctions.fs:62-63`), i.e. `arg ρ`, which wraps in (−π, π]. Δ is only
value-checked against `psiDelta` (`LocalRefinementTests.fs:161`); it is never
driven through `buildResidual`/`refine` — only Ψ is fit-tested
(`:166-200`, `:204-257`). An equality Δ residual
`weight·(model−desired)/denom` straddling the cut can read ≈ 2π when the true
mismatch is ≈ 0, which would trap LM. The wrap is inherited from the shared §F.5
derivation (correctly not re-derived here) and Δ-fitting is outside this slice's
ACs, but Δ is a fully composable `FitTarget` quantity that ships untested in the
one path that matters. Slice 011 should add a wrapped Δ residual test before
exposing Δ in the editor.

Secondary, already documented and acceptable: `modelValue`'s photometric branch
maps an unevaluable quantity to `0.0` (`Option.defaultValue 0.0`,
`MeritFunction.fs:72`) while the ellipsometric branch always returns a float — a
mis-paired target surfaces as a nonzero residual-against-0, not an error.
Deliberate (keeps the residual finite for the optimizer), deferred to slice-011
§G.8, noted in Gotchas. No action.

## Consistency

`InverseFit`'s `inv`/`tryFloat`/`unitFromHeader` (`InverseFit.fs:56-70`) are a
third copy of the `SpectralImport.fs:25-38` CSV scaffolding (also in
`MaterialImport`) — squarely the reuse critic's call. It is consistently flagged
with an in-code ACCEPTED-DUPLICATION note and an impl-log Gotcha, and the stated
blocker is accurate (no `Optimization → Storage` edge; the shared seam would have
to lift into `Domain.Units`, which already hosts `toWaveLength`). The
`MeasuredQuantity = TargetQuantity` alias (`InverseFit.fs:39`) is the right
cycle-2 outcome. Module boundaries, error-as-value channels, and naming otherwise
match the surrounding code.

## Bottom line

I would ship this. It is a faithful, minimal §G.4–G.7 implementation that reuses
the chart evaluator, the §F.5 ellipsometric derivation, and the slice-009
optimizer without forking any of them, both ACs green and tier labels intact. The
findings are refinements: the `applyVector`/`modelValue` build-vs-solve seam
(clarity + cost, already on the cycle-2 radar) and — newly — the untested Δ
residual path, which is the one risk slice 011 should close before exposing Δ in
the editor. None bind the verdict; my recommended posture is **pass**, weighed by
the judge against the green gates and the reuse critic's read.
