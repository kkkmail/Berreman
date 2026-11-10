# Step 0248 — impl plan (IMPLEMENT: the parameterised optical forms through the Stokes–Mueller algebra)

## Approach

`ItemEvaluation.fs` already has the element arm (`BoundOpticalElement.muellerAt`, step 247) for a polarizer preset and
the empty slot. This step adds the third case, `BoundOpticalForm of form : OpticalForm * values : ParameterValues`,
bound through `BoundOpticalElement.ofOpticalForm`, which checks the values against `form.parameterAxes` (step 245).

- `ParameterValues.forComponent` reads a compound component's axes under their `c<k>.` names (step 244's
  `inComponent`) and holds them under their own names, so a component evaluates exactly as a standalone form.
- `ParameterValue.retardanceAt` turns the retardance number into a `MuellerReconstruction.Retardance`, scaled by
  λ_ref / λ under `InverseWaveLengthRetardance`.
- `muellerAt` gains one transmitted arm per form and one reflected arm for all forms (`UnmodelledBranchErr`):
  - real LP: the linear diattenuator ½·[[1+r, 1−r, 0, 0], [1−r, 1+r, 0, 0], [0, 0, 2√r, 0], [0, 0, 0, 2√r]] written
    through `RealMatrix4x4.create`, turned by `Propagation.rotateMueller (θ + axisOffset)`;
  - retarder plate: `MuellerReconstruction.retarderMueller (θ + fastAxisOffset) (retardance.retardanceAt dispersion w)`;
  - black box: the sixteen elements (or the scale times the fifteen with m00 = 1) written through
    `RealMatrix4x4.create`, turned by `Propagation.rotateMueller θ`;
  - compound: each component evaluated by `muellerAt` itself at rotation 0 from `values.forComponent`, kept as a
    `MuellerComponent` at its offset, answered as `Propagation.rotateMueller θ (Propagation.compoundMueller …)`, so a
    nested compound recurses.

No new 4×4 algebra (the only arithmetic is writing the element lists), cache or option.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemEvaluation.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemEvaluationTests.fs` (five new facts, a 1e-12 helper)

## Risks

- **ELEV010:** the brief types `forComponent`'s position as `int`; step 244 measured that ELEV010 refuses an `int`
  beside `ParameterName` in a member, and `inComponent` takes a `ParameterNameIndex`. The position is therefore a
  `ParameterNameIndex` (recorded as a deviation).
- Local helpers keep elevated `Ok` types (`ParameterValue`, not `float`) so no private-signature rule fires.
- Exactness: the forms reach their matrices by other routes than the expectations, so the new facts compare within
  1e-12, as the brief says.
