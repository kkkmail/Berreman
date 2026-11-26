# Step 0247 — impl log (IMPLEMENT: the bound element arm of the evaluator)

## Progress

- [x] `ItemEvaluation.fs`: `UnmodelledBranchErr`, `ElementOrientation`, `BoundOpticalElement.muellerAt`.
- [x] `ItemEvaluationTests.fs`: two new facts, widened error-rendering fact.
- [x] Development runs: build, constructor tests (all discovered), elevation, invariant greps.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemEvaluation.fs`
  - `open Berreman.Fields` (for `IncidenceAngle`, `WaveLength`, `MuellerMatrix`), placed before the constructor
    modules' opens so their record labels keep precedence;
  - `ItemEvaluationError.UnmodelledBranchErr of reason : string`, rendered `the element has no such branch: <reason>`;
    the error type's doc names the new condition;
  - `ElementOrientation = { rotation : Angle; incidence : IncidenceAngle; branch : Propagation.Branch }` with
    `static member transmittingAt (rotation : Angle) : ElementOrientation` (`IncidenceAngle.normal`, `BranchTransmitted`);
  - `BoundOpticalElement = BoundPolarizerPreset of PolarizerBehavior | BoundEmptySlot` with
    `member this.muellerAt (orientation : ElementOrientation) (w : WaveLength) : Result<MuellerMatrix, ItemEvaluationError>`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemEvaluationTests.fs`
  - private helpers `assertMuellerExact` (exact, element by element through `MuellerMatrix.element`) and
    `assertUnmodelledBranch`;
  - new: `a polarizer preset is its behaviour's Mueller matrix at the element's rotation`;
  - new: `an empty slot is the identity, and an element that only transmits has no reflected branch`;
  - widened: `every evaluation error renders its condition and its payload` covers `UnmodelledBranchErr`
    (distinct renderings 3 → 4).

## Decisions

- **`muellerAt` is one match over (element, branch) with no wildcard.** A later `BoundOpticalElement` case (steps
  248–250) fails to compile until both its branches are answered.
  - `BoundPolarizerPreset b` on `BranchTransmitted` is `Propagation.behaviorMueller b orientation.rotation`, the call
    the run law makes today.
  - `BoundEmptySlot` on `BranchTransmitted` is `Propagation.identityMueller`.
  - Either on `BranchReflected` is `UnmodelledBranchErr`.
- **The `UnmodelledBranchErr` reason names the branch and the element's kind:** `the reflected branch of a polarizer
  preset, which only transmits` / `the reflected branch of an empty slot, which only transmits`.
  - The kind is a literal per match arm, because the slice forbids any other member (no `kind` member is added).
  - `BoundPolarizerPreset` carries only the `PolarizerBehavior`, not the preset's name, so the reason cannot name the
    preset itself.
- **`w` is unread** by both arms (a polarizer preset and an empty slot are achromatic). The parameter is part of the
  declared contract; FS1182 (unused parameter) is not enabled in this repo, so it raises no warning.
- **Fixtures:**
  - the compound is two reference ideal LPs (`Propagation.analyzerMueller IdealLinear Angle.zero`) at offsets 20° and
    65°, the same compound `PolarizerBehaviorTests` builds;
  - the `MuellerComponent` records are annotated, since later opens could capture the `matrix` / `offset` labels.
- **Assertions added inside the authorized facts:**
  - the polarizer fact also evaluates every rotation at an oblique 40° incidence and expects the same matrix (the slice
    says both arms ignore the incidence);
  - the empty-slot fact pins `transmittingAt 30°` as `{ rotation = 30°; incidence = normal; branch = BranchTransmitted }`.

  No new test identity is added.

## Testing state

These are development runs over the final tree. Invariant 6: the arc-runner's gate engine gives the verdict, not these runs.

- **build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exit 0, `0 Error(s)` (`.artifacts/0247-build.log`).
  - That run recompiled the vendored MathNet C#, which raised its two pre-existing `SYSLIB0051` warnings
    (`MathNetNumerics/Numerics/Exceptions.cs`).
  - The incremental rebuild over the same tree is exit 0, `0 Warning(s)`, `0 Error(s)`
    (`.artifacts/0247-build-incremental.log`).
  - The first build compiled both edited files with no F# warning or error.
- **constructor-unit-tests (all discovered):** `Passed!`, total 1387, succeeded 1387, failed 0, skipped 0, 1 m 09 s
  (`.artifacts/0247-constructor-unit-tests.log`, TRX at `.artifacts/0247-constructor-unit-tests.trx`).
  - 1387 is checkpoint 246's 1385 plus the two new facts.
  - All nine `ItemEvaluationTests` facts are `Passed` in the TRX under their declared identities, including
    `a polarizer preset is its behaviour's Mueller matrix at the element's rotation`,
    `an empty slot is the identity, and an element that only transmits has no reflected branch` and the widened
    `every evaluation error renders its condition and its payload`.
- **elevation** with `ASG_ARC_BASE_COMMIT=f395597c…`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 191s`.
  Known findings 0, suppressed 1350 (the excluded ELEV004, unchanged from step 246) (`.artifacts/0247-elevation.log`).
- **The fourteen s25 invariant greps,** with `--untracked`: all 14 find nothing (`.artifacts/0247-invariants.log`).
- **Red before green: not measured.** The production members and the facts were written in one pass.
  - The new facts name symbols that did not exist before this step (`ElementOrientation`, `BoundOpticalElement`,
    `muellerAt`, `UnmodelledBranchErr`).
  - Against the prior tree they cannot compile (FS0039), but that red build was not captured.

No test was removed or renamed, and no assertion was weakened.

## Artifacts

- `specs/0050/.artifacts/0247-build.log`, `0247-build-incremental.log`
- `specs/0050/.artifacts/0247-constructor-unit-tests.log`, `0247-constructor-unit-tests.trx`
- `specs/0050/.artifacts/0247-elevation.log`
- `specs/0050/.artifacts/0247-invariants.log`

## Gotchas

- **`ItemEvaluation.fs` now opens `Berreman.Fields`.**
  - The open sits before the constructor modules' opens, so their record labels keep precedence.
  - `SourceEmission.ofValues` already annotates its record, so `IncidentLightInfo.ellipticity` cannot capture it.
- **The new record labels `rotation` / `incidence` / `branch`** win inference in any file that opens `ItemEvaluation`.
  - Today that is only `ItemEvaluationTests`.
  - Annotate the record you mean when a file also opens a module with a `rotation` label.
- **The tests bind each element to a typed `let`** (`let emptySlot : BoundOpticalElement = BoundEmptySlot`) before
  calling `muellerAt`, rather than writing `BoundEmptySlot.muellerAt`, which F# could read as a qualified name. That
  form was avoided, not tried.
- **Exact equality holds** because `muellerAt` makes the very `Propagation.behaviorMueller` call the expectation makes.
  - A later arm that reaches the same matrix by another route (e.g. via `compoundMueller` per component) would need
    `MatrixComparison`-style tolerance, not exact equality.
