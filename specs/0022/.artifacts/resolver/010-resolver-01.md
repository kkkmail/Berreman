# Resolver decision -- 010.slice-md

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0022\.slices\010.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0022\.slices\010-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0022\.slices\010-impl-log.md`
- Judge MD: `C:\GitHub\Berreman\specs\0022\.artifacts\code_judge\010-03-code-judge.md`
- Critic critiques:
  - `C:\GitHub\Berreman\specs\0022\.artifacts\architecture_critic\010-01-architecture-critic.md`
  - `C:\GitHub\Berreman\specs\0022\.artifacts\reuse_critic\010-02-reuse-critic.md`
- Escalation reason: `too many failures (2/2): code-judge route-back-to-worker on cycle 2` (category: `failures-cap`)

## Diagnosis

This is a `failures-cap` escalation, but the underlying pattern is **not** the
usual oscillation. The worker is converging, not thrashing: cycle 1 routed back
on a real coverage gap (the net-new ellipsometric surface untested) plus reuse
findings F1/F2, and the worker closed all of them cleanly — the architecture
critic this cycle explicitly recommends ship, and all three gates (build,
unit-tests, constructor-unit-tests = 118 passed) are green. Cycle 2 surfaced one
**new, different, low-severity** finding rather than a re-raise of an unfixed
one. The slice ran out of per-launch failure budget on what is essentially a
cosmetic reuse nit, which is exactly the situation the resolver's +1 route-back
bump exists for.

The cause is concrete and singular. The reuse critic's F1 and the code-judge's
verdict agree precisely: `InverseFit.MeasuredQuantity`
(`InverseFit.fs:37-39`) is an isomorphic net-new DU that restates
`MeritFunction.TargetQuantity` (`MeritFunction.fs:34-36`) case-for-case over the
same two payload types (`OpticalFunction`, `EllipsometricFunction`), and it pays
for that with a pure 1:1 rename `match` in `parseMeasurementCsv`
(`InverseFit.fs:87-90`). `InverseFit` already `open`s `MeritFunction`
(`InverseFit.fs:13`), so the existing union is in scope at the use site. Under
the slice's inherited binding constraint §0 #2 ("reuse before invention … a new
artefact with no such trace MUST be dropped"), this untraced abstraction should
be dropped — R-4 names `OpticalFunction`/`EllipsometricFunction` directly, not a
new measured-quantity union.

The fix the judge specified is mechanical and self-contained: drop the
`MeasuredQuantity` DU, make `parseMeasurementCsv`/`invertFromCsv` take
`MeritFunction.TargetQuantity` directly, delete the translation arm at
`InverseFit.fs:87-90`, and bind the value straight into `FitTarget.quantity`;
then switch the four test constructors in `LocalRefinementTests.fs` (~lines
124/135/224/251) from `InverseFit.MeasuredPhotometric`/`MeasuredEllipsometric`
onto `MeritFunction.Photometric`/`Ellipsometric`. That is **two files**, no new
design, no new dependency, no scope change — a deletion plus call-site renames
on an already-green tree. The reuse critic and judge even sanction a type alias
if the `Measured…` naming is wanted, so the worker has a fallback that cannot
fail to compile.

Confidence is high. The change is below the file cap (2), expressible well within
the sentence cap, and the deterministic gates already pass before the change,
so the regression surface is tiny. The two explicitly-out-of-scope items (the
per-target `applyVector` rebuild efficiency note and the accepted `SpectralImport`
CSV-helper duplication) were already dispositioned by prior judges and must be
left untouched — folding them in would be the only way this hint could overreach,
so the hint names them as off-limits. This meets every `issue-hint` criterion in
the rubric; a one-shot hint plausibly closes the slice.

## Verdict

issue-hint

## Hint

In `InverseFit.fs`, delete the `MeasuredQuantity` DU (~lines 37-39) and make
`parseMeasurementCsv`/`invertFromCsv` take `MeritFunction.TargetQuantity`
directly as the `quantity` parameter, removing the 1:1 translation `match`
(~lines 87-90) and binding the value straight into `FitTarget.quantity`; in
`LocalRefinementTests.fs`, switch the four constructors at ~lines 124/135/224/251
from `InverseFit.MeasuredPhotometric`/`MeasuredEllipsometric` to
`MeritFunction.Photometric`/`Ellipsometric`. A type alias for the `Measured…`
naming is acceptable for caller clarity, but a second isomorphic DU is not. Leave
the per-target `applyVector` efficiency note and the already-accepted
`SpectralImport` CSV-helper duplication untouched — neither is in scope this
round. Re-run all three gates (build, unit-tests, constructor-unit-tests) and
confirm green before emitting.

```json
{"verdict": "issue-hint", "rationale": "failures-cap escalation but the worker is converging, not thrashing: cycle-1 triggers all closed (gates green, architecture critic recommends ship), and cycle 2 routed back on a single NEW low-severity reuse nit. Cause is concrete and unanimous (reuse critic F1 == judge verdict): InverseFit.MeasuredQuantity (InverseFit.fs:37-39) is an isomorphic net-new DU restating MeritFunction.TargetQuantity (MeritFunction.fs:34-36) with a pure 1:1 rename match at InverseFit.fs:87-90, an untraced abstraction against inherited binding constraint §0 #2. Fix is mechanical, two files (InverseFit.fs + LocalRefinementTests.fs), no new design/dependency/scope, on an already-all-gates-green tree, with a type-alias fallback the critics sanctioned -- a textbook one-shot unblock.", "operator_reply_text": "In InverseFit.fs, delete the MeasuredQuantity DU (~lines 37-39) and make parseMeasurementCsv/invertFromCsv take MeritFunction.TargetQuantity directly as the quantity parameter, removing the 1:1 translation match (~lines 87-90) and binding the value straight into FitTarget.quantity; in LocalRefinementTests.fs, switch the four constructors at ~lines 124/135/224/251 from InverseFit.MeasuredPhotometric/MeasuredEllipsometric to MeritFunction.Photometric/Ellipsometric. A type alias for the Measured naming is acceptable for caller clarity, but a second isomorphic DU is not. Leave the per-target applyVector efficiency note and the already-accepted SpectralImport CSV-helper duplication untouched -- neither is in scope this round. Re-run all three gates (build, unit-tests, constructor-unit-tests) and confirm green before emitting.", "confidence": "high"}
```
