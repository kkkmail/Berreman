# Step 0248 — state of the world

## Where we are

Spec 0050, section S8.1 ("The run law walks every object; the source is real"), step 248 (IMPLEMENT).

- Steps 244–247 built the evaluator:
  - its parameter names and `ParameterValues` with `checkedFor`;
  - its source arm (`emissionWith`);
  - its element arm, `BoundOpticalElement.muellerAt`, for a polarizer preset and the empty slot.
- This step adds the parameterised optical forms of step 208 to the element arm, evaluated through the Stokes–Mueller
  algebra: the real linear polarizer, the retarder plate, the black-box Mueller matrix and the compound.
- Steps 249 and 250 add the stack samples and the cut plates as further `BoundOpticalElement` cases. The run law keeps
  calling `Propagation.behaviorMueller` until S8.2 moves it onto the evaluator.

## What's working

- Added `BoundOpticalForm of form * values`, bound by `BoundOpticalElement.ofOpticalForm` after `checkedFor` the form's axes
- Evaluated a real LP (extinction ratio or diattenuation), a retarder plate (constant or λ_ref / λ retardance), a black-box Mueller matrix (16 elements, or a scale and 15) and a compound, at the element's rotation
- Reused `rotateMueller`, `retarderMueller` and `compoundMueller`; a compound's components evaluate through `muellerAt` itself, so compounds nest
- Added `ParameterValues.forComponent` (a component's `c<k>.` values under their own names) and `ParameterValue.retardanceAt`
- Added five `ItemEvaluationTests` facts (constructor tests 1387 → 1392)

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster: build, elevation,
constructor-unit-tests over everything discovered, and the fourteen s25 invariants.

The worker's development runs over the final tree, with logs under `specs/0050/.artifacts/0248-*`:

- **build:** exit 0, `0 Error(s)`.
  - The first run recompiled the vendored MathNet C#, which raised its two pre-existing `SYSLIB0051` warnings.
  - The final rebuild reports `0 Warning(s)`.
- **constructor-unit-tests:** `Passed!`, 1392 succeeded, 0 failed, 0 skipped, in 67 s (estimate 180 s).
  - That is checkpoint 247's 1387 plus the five declared facts:
    - `a real linear polarizer at its defaults is the ideal one, and its leakage passes crossed light`
    - `a retarder plate is the retarder matrix at its fast axis, its retardance dispersed as its model says`
    - `a black-box Mueller matrix is held at reference orientation and turned by the element's rotation`
    - `a compound applies its components in light order, each under its own prefixed parameters`
    - `a form is bound only with a value for each of its parameters, in range`
  - Each is `Passed` in the TRX under its declared identity.
- **elevation** from the arc origin `f395597`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in 198 s.
  Known findings: 0.
- **The fourteen s25 invariant greps,** untracked files included: none finds anything.
- **Red, measured by mutation.** Reversing the compound's light order and dropping the λ_ref / λ scaling turned the
  compound and retarder facts red (2 of 14 failed). The mutation was reverted and everything above re-run.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1392
```

## Architecture

- **The forms are one more case of `BoundOpticalElement`.**
  - `muellerAt` stays one match over (element, branch) with no wildcard.
  - It now has four transmitted arms for the forms and one reflected arm for all of them (`UnmodelledBranchErr`).
- **A compound evaluates each component by calling `muellerAt` on a `BoundOpticalForm` of the component.**
  - The component is evaluated at rotation 0, with values from `forComponent`.
  - Each result is kept as a `MuellerComponent` at the component's offset.
  - The arm answers `rotateMueller θ (compoundMueller …)`, the arm `behaviorMueller` takes for a `ConstantMueller`.
  - Nesting recurses through the member, so no new public symbol is added.
- **No new 4×4 algebra.**
  - The real LP and the black box write their element lists through `RealMatrix4x4.create`, as `analyzerMueller` does.
  - Every rotation is `Propagation.rotateMueller`, and every product is `compoundMueller`'s or `retarderMueller`'s.
- **`forComponent` takes a `ParameterNameIndex`,** step 244's elevated position, not the brief's `int`, which ELEV010
  refuses beside `ParameterName`.

## Deferred

Nothing. Every requirement of the step landed in this round. The stack samples and the cut plates are steps 249 and
250's own cases, scheduled steps rather than deferrals from this one.

## Gotchas

- **A caller of `forComponent` passes a `ParameterNameIndex` bound at origin,** e.g.
  `let position = ParameterNameIndex (k + 1)`.
- **A `BoundOpticalForm` built without `ofOpticalForm` is unchecked for range.** A missing number still answers
  `MissingParameterValueErr`, because every arm reads through `valueOf`.
- **The new facts compare within 1e-12.** The forms reach their matrices by routes other than the expectations'.
- **`CompoundComponent` and `MuellerComponent` share the record label `offset`.** Annotate the record you build.
- **The vendored MathNet `Exceptions.cs` emits two `SYSLIB0051` warnings** whenever it recompiles from scratch. They
  pre-date this step.

## Changelog

- 2026-11-10 — Step 0248:
  - added `BoundOpticalForm` / `ofOpticalForm`, `ParameterValues.forComponent` and `ParameterValue.retardanceAt`;
  - evaluated the real LP, the retarder plate, the black box and the compound in `muellerAt`;
  - added five `ItemEvaluationTests` facts (constructor tests 1387 → 1392).
