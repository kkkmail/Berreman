# Code judge -- 008.slice-md cycle 2

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0022\.slices\008.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0022\.slices\008-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0022\.slices\008-impl-log.md`
- Gate results: build = pass, unit-tests = pass, constructor-unit-tests = pass
- Critic critiques:
  - `C:\GitHub\Berreman\specs\0022\.artifacts\architecture_critic\008-01-architecture-critic.md`
  - `C:\GitHub\Berreman\specs\0022\.artifacts\reuse_critic\008-02-reuse-critic.md`

## Rationale

This is a cleanup-only re-spawn, and it resolved exactly the two items the
cycle-1 route-back ordered. I confirmed both directly against the tree rather
than taking the critics' word. (1) The whitespace churn is gone: `git diff HEAD`
on the three tracked engine files (`FieldFunctions.fs`, both `.fsproj`) is a
27-line real-edit diff, and it is identical to `git diff HEAD
--ignore-space-at-eol` — no EOL-normalization noise remains on the core engine
file. The `FieldFunctions.fs:93` change is now the bare member completion
`MuellerMatrix.fromEmFields em em` with a docstring, a pure delegation that
re-derives no 4×4 algebra (R-3 satisfied). (2) The fresh tolerance literal is
gone: `MuellerMatrixTests.fs:10,24` now `open BerremanTests.MatrixComparison`
and assert against the shared `allowedDiff`, matching the project prompt's pin
on `MatrixComparison.fs` as the tolerance home. Both cycle-1 route-back items
are fully cleared, and no working analysis code or test logic moved.

All three gates are green and the SoW/impl-log line up with the diff. I verified
the new public surface exists and is named where the spec requires:
`AnalysisFunctions.fs` carries `absorptance`/`stokesSystem`/`degreeOfPolarization`/
`psiDelta`/`ncs`, `FieldProfile.fs` carries `fieldDepthProfile`/`layerAbsorptance`/
`totalAbsorbedPower`, and `Colorimetry.fs` carries `spectrumToXyz`/`xyzToLab`/
`xyzToSrgb` — all registered in `Analytics.fsproj` after `Charting.fs` in
dependency order (R-10). The reuse critic's seam audit (engine seams called, not
re-derived) is consistent with what I read.

On the test-coverage criterion: the slice spec deliberately routes the FULL
AC-F3/F5/F7/F8 equivalence suite to slice 011's `OptimizationTests.fs` and makes
the AC bullets implemented behaviourally here — AC-F1/F2/F4/F6/F9 — carry
mandatory same-slice smokes. Every one is present in the diff:
`AnalysisFunctionsTests.fs` covers AC-F1 (A = 1−R−T via `Solution.func`), AC-F4
(`degreeOfPolarization` value + [0,1] bound), AC-F6 (`fieldDepthProfile` list
length + monotonic meter depths, shape-only as the spec mandates given the
carried-forward `propagate` limitation), AC-F9 (default `numberOfReflections` = 3
and a caller value threading into a thick-Plate solve), and AC-F2 (`.binz`
round-trip with gzip-magic JSON-exclusion). The `FieldFunctions.fs:93` engine
edit ships with its own guard in `MuellerMatrixTests.fs`. No function implemented
behaviourally here is left without an owned in-slice test, and the deferred
equivalence coverage is a binding parent-spec routing decision, not a worker
omission — the named covering tests are present per the done-green rubric.

The residual critic notes are all advisory non-blockers, documented in the SoW
Gotchas/Architecture: the missing small-denominator guard in `rho` (low
likelihood, deferrable to the 011 equivalence suite), the infinity-thickness
asymmetry between the two `propagate` walks (films are finite in practice), the
AC-F9 pass-through Part H will need as a later signature change, and the two
cross-assembly reuse near-misses (F2 spec-sanctioned embedded CIE/illuminant
tables per R-8; F3 `interp` with no reachable shared helper given the
`Analytics`-is-consumed-by-`OpticalConstructor` dependency direction). None
identifies an unmet slice-spec requirement, a layering violation, or a forbidden
duplication. Both critics independently say "ship it." With `cycles_remaining`
at 1, routing back to spend the final cycle on cosmetic comparator shape or
spec-deferred items would be ordering exactly the nice-to-haves the rubric says
to note and move past. The slice meets done-green ground.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "Cleanup-only cycle 2 resolved both cycle-1 route-back items: the FieldFunctions.fs core-engine whitespace churn is fully reverted (git diff HEAD identical to --ignore-space-at-eol, 27-line real-edit diff) and MuellerMatrixTests now reuses the shared MatrixComparison.allowedDiff instead of a fresh literal. All three gates pass (build, unit-tests 76, constructor-unit-tests 107). SoW and impl-log line up with the diff. The new public surface (absorptance/stokesSystem/degreeOfPolarization/psiDelta/ncs, fieldDepthProfile/layerAbsorptance/totalAbsorbedPower, spectrumToXyz/xyzToLab/xyzToSrgb) is registered in dependency order and the FieldFunctions.fs:93 member is a clean pure delegation to MuellerMatrix.fromEmFields. Every behaviourally-implemented AC bullet (F1/F2/F4/F6/F9) has its mandatory same-slice smoke present in the diff, and the engine edit is guarded by MuellerMatrixTests; the FULL AC-F3/F5/F7/F8 equivalence suite is routed to slice 011 by the binding parent spec, not omitted. Both critics recommend shipping; all residual findings are documented, advisory, and either spec-sanctioned or have no reachable reuse target. No substantive change remains to order.", "retry_hint": ""}
```
