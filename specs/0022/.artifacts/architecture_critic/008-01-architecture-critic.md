# Architecture critique -- 008.slice-md cycle 2

## Summary

This is a cleanup-only re-spawn, and it does exactly the one thing the cycle-1
route-back asked for. Both items the judge named are resolved: the
`FieldFunctions.fs:93` whitespace churn is fully reverted, and
`MuellerMatrixTests` now reuses the shared tolerance. No analysis code or test
logic changed, the three gates stay green per the SoW, and the diff collapses
to only the real edits. The remaining substantive notes from cycle 1 are
pre-existing, documented, and correctly left untouched this cycle.

## Consistency

The two route-back items are cleanly addressed:

- **Whitespace churn reverted.** `git diff HEAD` and
  `git diff HEAD --ignore-space-at-eol` now both report 61 lines across
  `FieldFunctions.fs` + the two `.fsproj` files (cycle 1 was 499 vs 9). The
  `FieldFunctions.fs` change is now the bare member completion at line 93 — a
  pure delegation to `MuellerMatrix.fromEmFields em em` with an explanatory
  docstring — and the two `.fsproj` edits are just the `<Compile>`/
  `<ProjectReference>`/`<PackageReference>` adds. The permanent history of the
  core engine file is no longer polluted; this was the judge's primary reason
  for the re-spawn and it is fully cleared.
- **Tolerance literal removed.** `MuellerMatrixTests.fs:10,24` now opens
  `BerremanTests.MatrixComparison` and asserts against `allowedDiff` instead of
  the fresh `1.0e-10` literal, matching the project prompt's pin on
  `MatrixComparison.fs` as the tolerance home. Since the delegation diff is
  identically zero, the looser shared tolerance still guards the member against
  drifting away from delegation.

## Risks

These are carried forward verbatim from the cycle-1 critique — none was
introduced by cycle 2, and the judge explicitly instructed the worker not to
touch working analysis code, so leaving them is the correct posture. I restate
them only so the judge does not have to re-derive them:

- **No small-denominator guard in `rho`** (`AnalysisFunctions.fs:53-55`).
  `degreeOfPolarization` guards `s0 ≈ 0` (`:46`) but `reflected.amplitudeP /
  reflected.amplitudeS` does not; a near-s-null reflected field would push
  Inf/NaN into Ψ/Δ/(N,C,S). Low likelihood for tested cases, deferrable to the
  slice-011 equivalence suite.
- **Infinity-thickness asymmetry between the two walks.** `fieldDepthProfile`
  rewrites `Infinity` thickness to a `0.0<meter>` sub-step
  (`FieldProfile.fs:37-42`) to dodge `propagate`'s `failwith`, but
  `layerAbsorptance` calls `field.propagate layer` on the raw layer (`:75`).
  An infinite-thickness film would throw there. Films are finite in practice,
  so low risk, but the inconsistency is a latent trap for Part H.
- **AC-F9 convergence control is threaded at the raw engine constructor, not
  through an analysis-layer parameter.** None of the `FieldProfile` entry
  points accepts a `SolverParameters`; `totalAbsorbedPower` hardcodes the
  default via `OpticalSystemSolver(info, system)` (`FieldProfile.fs:95`). The
  directive is satisfied at the seam and the default-is-3 invariant holds, but
  Part H will need a pass-through added later — a cross-call signature change
  rather than a near-free addition now. Evolvability note, not a blocker.

All three are documented in the SoW Gotchas/Architecture sections and are
defensible smoke-level non-blockers given the slice's remit and the slice-011
equivalence routing.

## Bottom line

I would ship this. The re-spawn was scoped to the cleanup the cycle-1 judge
ordered, and it delivers precisely that and nothing more: the core-engine
whitespace churn is gone (61-line diff, EOL-stable), the tolerance literal is
folded into the shared `MatrixComparison.allowedDiff`, and no working code or
test logic moved. The substantive items I and the reuse critic raised in cycle
1 remain open by design — the judge told the worker not to touch green code —
and none rises above a documented evolvability/edge-case note appropriate to
defer to slice 011. The judge decides, but I see no reason for a third cycle:
the one cheap, mechanical fix that held cycle 1 back is done.
