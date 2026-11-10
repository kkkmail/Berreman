# Step 0248 — impl log (IMPLEMENT: the parameterised optical forms through the Stokes–Mueller algebra)

## Progress

- [x] `ItemEvaluation.fs`: `BoundOpticalForm`, `ofOpticalForm`, `forComponent`, `retardanceAt`, the form arms of `muellerAt`.
- [x] `ItemEvaluationTests.fs`: five new facts and their private helpers.
- [x] Red measured by a temporary mutation, then reverted (see Testing state).
- [x] Development runs over the final tree: build, constructor tests (all discovered), elevation, invariant greps.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemEvaluation.fs`
  - `open Berreman.MathNetNumericsMath`, for `RealMatrix4x4.create`;
  - `ParameterValues.forComponent (position : ParameterNameIndex) (axes : ItemParameterAxis list)`: an intrinsic
    member, read under `axis.name.inComponent position`, held under `axis.name`;
  - `ParameterValue.retardanceAt (dispersion : RetardanceDispersion) (w : WaveLength) : MuellerReconstruction.Retardance`;
  - `BoundOpticalElement.BoundOpticalForm of form : OpticalForm * values : ParameterValues`;
  - `static member BoundOpticalElement.ofOpticalForm (values) (form)`: `values.checkedFor form.parameterAxes`, then
    the case;
  - `muellerAt`: four transmitted arms, one per form, and one reflected arm for every form.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemEvaluationTests.fs`
  - private helpers: `assertElementsWithin` / `assertMuellerWithin` / `assertWithin` (1e-12), `matrixOf`,
    `formMuellerAt`, `formDefaults`, `assertBound`;
  - private fixtures: `at600`, `sampledRotations`, `lpByExtinction`, `quarterWaveAfterLp`;
  - the five facts the step declares, under their declared identities.

## Decisions

- **Deviation: `forComponent`'s position is a `ParameterNameIndex`, not the brief's `int`.**
  - Step 244 measured that ELEV010 refuses an `int` beside `ParameterName` in a member (its impl-log, progress 4).
  - Step 244 introduced `ParameterNameIndex` for exactly this, and `inComponent` takes one.
  - An `int` here would also force a call-site rewrap, `inComponent (ParameterNameIndex position)` (ELEV001).
  - The symbol, `ParameterValues.forComponent`, and its meaning are as declared.
- **`forComponent` is intrinsic** (declared in the `ParameterValues` body beside `checkedFor`), not a `type … with`
  extension. The contract symbol is the same, and an intrinsic member cannot collide under ELEV019.
- **Every form arm reads its numbers through `values.valueOf` and the `ParameterName` members.** A missing number is
  therefore `MissingParameterValueErr` even for a `BoundOpticalForm` built without `ofOpticalForm`.
- **Real LP.** The minor transmittance r is `extinctionRatio`, or `(1 − D) / (1 + D)` of `diattenuation`. The
  diattenuator ½·[[1+r, 1−r, 0, 0], [1−r, 1+r, 0, 0], [0, 0, 2√r, 0], [0, 0, 0, 2√r]] is written through
  `RealMatrix4x4.create` (as `analyzerMueller` writes its matrices) and turned by
  `Propagation.rotateMueller (θ + axisOffset)`.
- **Retarder.** It is `MuellerReconstruction.retarderMueller (θ + fastAxisOffset) (retardance.retardanceAt dispersion w)`.
  `retardanceAt` scales by `reference.value / w.value`, two lengths in metres.
- **Black box.**
  - Under `SixteenElementMueller` every element is read from `ParameterName.muellerElement row column`.
  - Under `FifteenElementsAndScale`, m00 is 1, the other fifteen are read, and `blackBoxScale` multiplies every
    element as the element list is written.
  - The matrix is turned by `Propagation.rotateMueller θ`.
- **Compound.**
  - Each component k (1-based, light order) gets `values.forComponent k part.form.parameterAxes` and is evaluated by
    `muellerAt` itself on `BoundOpticalForm (part.form, componentValues)` at `transmittingAt Angle.zero`.
  - The result is kept as a `MuellerComponent` at `part.offset`, and the arm answers
    `Propagation.rotateMueller θ (Propagation.compoundMueller parts)`.
  - Recursion goes through the member, so no new public symbol is added and a nested compound recurses.
- **One local `sequenced` (Result list → list Result) inside `muellerAt`,** shared by the black-box and compound arms.
  - FSharp.Core has no Result traversal.
  - `CategoryEditor.fs`'s private `traverse` is fixed to `CategoryError`, and that file lies outside this step's
    authorized edits.
- **The reflected branch of every form** is `UnmodelledBranchErr "the reflected branch of an optical form, which only
  transmits"`.
  - The reason is one literal for all four forms. Naming each form's kind would copy the kind strings
    `emissionWith` already holds.
  - The incidence is never read.
- **Assertions beyond the brief's minimum, inside the five declared facts** (no new identity):
  - the `DiattenuationLeakage` defaults are also the ideal LP;
  - `retardanceAt` is checked directly at 316.4 nm and 632.8 nm, and under `ConstantRetardance`;
  - `forComponent` of the nested compound's second component equals the inner compound's own values;
  - the leaky inner compound's m00 is 0.505, which proves the prefixed value reached it;
  - a compound refuses a missing `c2.retardance` and an out-of-range `c1.extinctionRatio`;
  - an unchecked `BoundOpticalForm` with no values answers `MissingParameterValueErr "retardance"`;
  - `ofOpticalForm` at the defaults is `Ok (BoundOpticalForm (form, defaults))`.

## Testing state

These are development runs over the final tree. Invariant 6: the arc-runner's gate engine gives the verdict, not
these runs.

- **build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exit 0, `0 Error(s)`.
  - The first run recompiled the vendored MathNet C#, which raised its two pre-existing `SYSLIB0051` warnings
    (`.artifacts/0248-build.log`).
  - The final rebuild after the mutation revert is exit 0, `0 Warning(s)`, `0 Error(s)` (`.artifacts/0248-build-final.log`).
- **constructor-unit-tests (all discovered):** `Passed!`, total 1392, succeeded 1392, failed 0, skipped 0, in 1 m 07 s
  (`.artifacts/0248-constructor-unit-tests.log`; TRX at `.artifacts/0248-constructor-unit-tests.trx`).
  - 1392 is checkpoint 247's 1387 plus the five new facts.
  - All 14 `ItemEvaluationTests` facts are `Passed` in the TRX under their declared identities.
- **elevation** with `ASG_ARC_BASE_COMMIT=f395597c…`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 198s`.
  Known findings 0, suppressed 1350 (the excluded ELEV004) (`.artifacts/0248-elevation.log`).
- **The fourteen s25 invariant greps,** with `--untracked`: all 14 find nothing (`.artifacts/0248-invariants.log`).
- **Red, measured by mutation.** The new symbols did not exist before this step, so the facts could not compile
  against the prior tree. To show they test behaviour, `ItemEvaluation.fs` was mutated temporarily:
  - `compoundMueller (List.rev parts)`, which reverses the light order;
  - `retardanceAt` ignoring the λ_ref / λ scaling.

  `--filter-class OpticalConstructor.Tests.ItemEvaluationTests` was then `Failed!`, 2 of 14
  (`.artifacts/0248-red-mutation.log`): the retarder fact and the compound fact (element (0,1): expected 0.5, got
  3.06e-17). The file was restored byte for byte from a copy, and the build and the full suite above were re-run on it.

No test was removed or renamed, and no assertion was weakened.

## Artifacts

- `specs/0050/.artifacts/0248-build.log`, `0248-build-final.log`
- `specs/0050/.artifacts/0248-constructor-unit-tests.log`, `0248-constructor-unit-tests.trx`
- `specs/0050/.artifacts/0248-elevation.log`
- `specs/0050/.artifacts/0248-invariants.log`
- `specs/0050/.artifacts/0248-red-mutation.log`

## Gotchas

- **ELEV010 overrides the brief's `int`.** A later step calling `forComponent` passes a `ParameterNameIndex` bound at
  origin, as `Sample.parameterAxes` and the compound arm bind theirs (`let position = ParameterNameIndex (k + 1)`).
- **The new facts compare within 1e-12, not exactly.**
  - The forms reach their matrices by other routes than the expectations (R(−θ)·M·R(θ) against `analyzerMueller`'s
    closed form; the compound's product against a hand-built product).
  - The step-247 facts keep `assertMuellerExact`.
- **`CompoundComponent` and `MuellerComponent` share the label `offset`.** Annotate the record you build:
  `let lp : CompoundComponent = { form = …; offset = … }`.
- **`ElementOrientation.transmittingAt` cannot be the source of a copy-and-update expression directly.** The tests
  bind it first (`let at30 = …` then `{ at30 with branch = … }`).
- **`specs/0050/.manifest.state.json` was already modified when the round began.** It is the supervisor's file, and
  this step did not touch it.
