# Step 0250 — state of the world

## Where we are

Spec 0050, section S8.1 ("The run law walks every object; the source is real"), step 250 (IMPLEMENT).

- Steps 244–249 built the item-model evaluator:
  - `ParameterValues` with `checkedFor`;
  - its source arm;
  - its element arm `BoundOpticalElement.muellerAt`, for polarizer presets, the empty slot, the parameterised optical
    forms and the stack samples.
- This step adds the cut plates: a free-standing crystal plate, its parameters already chosen. It is predicted by the
  spec 0044 Berreman forward (`MuellerInverse.createBerremanForward`, oriented through `orientForCut`), reused and not
  copied, at the element's rotation as the plate's azimuth, its incidence, its branch as the observable and the
  wavelength.
- Step 251 binds every item-model choice through these constructors. S10.1 and S11.1 promote the crystal-parameter
  builders and the material form; step 375 uses `CutPlate.ofForward` for the study's observation rows.

## What's working

- Added `CutPlate` (cut, thickness and the forward's prediction) and `CutPlate.ofForward`, which holds a `ForwardModelProxy`'s `muellerOf` at chosen parameters, so the spec 0044 Berreman forward is called and not copied
- Added `ElementOrientation.sampleAzimuth` (the rotation as the plate's azimuth) and `ElementOrientation.observable` (the branch as `TransmittedMueller` / `ReflectedMueller`)
- Added `BoundOpticalElement.BoundCutPlate`; `muellerAt` predicts it on both branches at the incidence, azimuth and wavelength
- Added `ItemEvaluationError.CutPlateSolveErr`, which carries the forward's `ForwardSolveFailedErr` reason and renders `the cut plate's Berreman solve failed: <reason>`
- Added two `ItemEvaluationTests` facts and widened the render fact (constructor tests 1395 → 1397)

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster: build, elevation,
constructor-unit-tests over everything discovered, and the fourteen s25 invariants.

The worker's development runs over the final tree, with logs under `specs/0050/.artifacts/0250-*`:

- **build:** exit 0, `0 Warning(s)`, `0 Error(s)`.
- **constructor-unit-tests,** the gate's command: `Passed!`, 1397 succeeded, 0 failed, 0 skipped, in 67 s
  (estimate 180 s).
  - That is checkpoint 249's 1395 plus the two declared facts:
    - `a cut plate is predicted through the Berreman forward at the element's spin, incidence, branch and wavelength`
    - `a cut plate whose forward fails answers CutPlateSolveErr`
  - The widened `every evaluation error renders its condition and its payload` also passes.
  - All three are `Passed` in the TRX under their declared identities.
- **elevation** from the arc origin `f395597`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in 200 s
  (estimate 600 s). Known findings: 0. The run was repeated over the final tree after a doc-comment edit.
- **The fourteen s25 invariants,** by their gate commands: all hold. The greps with untracked files included find
  nothing.
- **Red, measured by mutation.** Pinning `sampleAzimuth` to 0 and dropping the forward's reason turned both new facts
  red (2 of 19). The mutation was reverted (`cmp` identical) and everything above was re-run.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1397
```

## Architecture

- **A cut plate is one more case of `BoundOpticalElement`, holding a `[<ReferenceEquality>] CutPlate`.**
  - Its `predict` is the forward's `muellerOf` with the parameters applied (`ofForward`), so the evaluator never sees
    the crystal-parameter type. `CutPlate` is not generic, and neither is `BoundOpticalElement`.
  - `[<ReferenceEquality>]` keeps `BoundOpticalElement`'s generated value equality (step 249's finding).
- **The element's orientation speaks the forward's vocabulary through two intrinsic members:**
  - `sampleAzimuth`, the rotation as the plate's azimuth about its normal;
  - `observable`, the branch as the recorded Mueller matrix.

  `muellerAt` builds the `MeasurementConfiguration` from them, the plate's cut and thickness and λ. Step 375 reads
  `o.configuration.elementOrientation` back into the same members.
- **The forward's typed failure becomes the evaluator's typed failure.** `ForwardSolveFailedErr (_, reason)` maps to
  `CutPlateSolveErr reason`. The forward already catches the solver's throw at its own boundary, so nothing here
  catches.
- **The `muellerAt` match still has no wildcard.** The cut plate's arm is an or-pattern over both branches.

## Deferred

Nothing. Every requirement of the step landed in this round. No crystal-parameter material, builder or option is
added, as the brief directs: S10.1 promotes the builders and S11.1 the material form. Those are scheduled steps, not
deferrals from this one.

## Gotchas

- **`MuellerInverse.SampleAzimuth.degree` does not compile fully qualified.** The union case shadows the type. The
  test opens `MuellerInverse`, as the inverse suites do.
- **Two `CutPlate`s are never `=`.** Compare cut plates by their predicted matrices.
- **The brief's plate (1 mm, n_o 1.544, n_e 1.553, 600 nm) is 15 whole waves at normal incidence.** Azimuth
  dependence shows only off normal. The fact uses the brief's 20° incidence.

## Changelog

- 2026-11-10 — Step 0250:
  - added `CutPlate` / `ofForward`, `ElementOrientation.sampleAzimuth` / `observable`, `BoundCutPlate` and
    `CutPlateSolveErr`;
  - the cut plate is predicted through the spec 0044 Berreman forward;
  - added two `ItemEvaluationTests` facts (constructor tests 1395 → 1397).
