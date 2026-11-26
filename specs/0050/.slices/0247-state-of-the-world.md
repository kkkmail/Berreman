# Step 0247 — state of the world

## Where we are

Spec 0050, section S8.1 ("The run law walks every object; the source is real"), step 247 (IMPLEMENT).

- Steps 244–246 gave the evaluator its `ParameterValues` with `checkedFor`, and its source arm,
  `ItemModelChoice.emissionWith`.
- This step adds the element arm: an optical element with its parameters bound, evaluated at an orientation and a
  wavelength (`BoundOpticalElement.muellerAt`). For now it covers a polarizer preset and the empty slot.
- Steps 248, 249 and 250 add the optical forms, the stack samples and the cut plates as further `BoundOpticalElement`
  cases. The run law keeps calling `Propagation.behaviorMueller` directly until S8.2's step 265 reads the setup's
  slots through `ItemModelChoice.boundWith` / `BoundOpticalElement.muellerAt`.

## What's working

- Added `ItemEvaluation.ElementOrientation` (rotation, incidence, branch) and `transmittingAt`, a rotation at normal incidence on the transmitted branch
- Added `BoundOpticalElement` (`BoundPolarizerPreset of PolarizerBehavior | BoundEmptySlot`) with `muellerAt`: a preset is `Propagation.behaviorMueller` at its rotation, an empty slot the identity
- Added `ItemEvaluationError.UnmodelledBranchErr`, rendered `the element has no such branch: <reason>`, answered on the reflected branch of a T-only element
- Added two `ItemEvaluationTests` facts and widened the error-rendering fact; constructor tests rise 1385 → 1387

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster: build, elevation,
constructor-unit-tests over everything discovered, and the fourteen s25 invariants.

The worker's development runs over the final tree, with logs under `specs/0050/.artifacts/0247-*`:

- **build:** exit 0, `0 Error(s)`.
  - The first run recompiled the vendored MathNet C#, which raised its two pre-existing `SYSLIB0051` warnings.
  - The incremental rebuild of the same tree reports `0 Warning(s)`.
- **constructor-unit-tests:** `Passed!`, 1387 succeeded, 0 failed, 0 skipped, in 69 s (estimate 180 s).
  - That is checkpoint 246's 1385 plus the two new facts:
    - `a polarizer preset is its behaviour's Mueller matrix at the element's rotation`
    - `an empty slot is the identity, and an element that only transmits has no reflected branch`
  - The widened `every evaluation error renders its condition and its payload` also passes.
  - Each is `Passed` in the TRX under its declared identity.
- **elevation** from the arc origin `f395597`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in 191 s.
  Known findings: 0.
- **The fourteen s25 invariant greps,** untracked files included: none finds anything.
- **Red before green** was not captured separately: the facts and members were written in one pass. See the impl-log.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1387
```

## Architecture

- **`ElementOrientation` and `BoundOpticalElement` live in `ItemEvaluation`,** beside the source arm and the
  `ItemEvaluationError` they answer with.
  - `ElementOrientation.branch` is `Propagation.Branch`, reused, not redeclared.
- **`muellerAt` is one match over (element, branch) with no wildcard.** Each later `BoundOpticalElement` case fails
  to compile until both its branches are answered.
- **The polarizer arm makes the very call the run law makes** (`Propagation.behaviorMueller b rotation`), so moving
  the run law onto the evaluator cannot change a single matrix element.
- **Both arms are achromatic and T-only.** They read neither the wavelength nor the incidence; the reflected branch is
  `UnmodelledBranchErr`, naming the branch and the element's kind.

## Deferred

Nothing. Every requirement of the step landed in this round. The optical forms, stack samples and cut plates are
steps 248–250's own cases, scheduled steps rather than deferrals from this one.

## Gotchas

- **`ItemEvaluation.fs` now opens `Berreman.Fields`,** ahead of the constructor modules' opens so their record labels
  keep precedence.
- **The labels `rotation` / `incidence` / `branch`** now win record inference in any file that opens `ItemEvaluation`
  (today only `ItemEvaluationTests`). Annotate the record you mean.
- **The `UnmodelledBranchErr` reason is a literal per match arm.** The slice forbids any other member, so there is no
  `kind` member, and a preset carries only its behaviour, not its name.
- **The facts compare exactly** because the evaluator and the expectation make the same `behaviorMueller` call. An
  arm that reaches a matrix by another route needs a tolerance.
- **The vendored MathNet `Exceptions.cs` emits two `SYSLIB0051` warnings** whenever it recompiles from scratch. They
  pre-date this step and do not affect the build gate's pass rule.

## Changelog

- 2026-11-10 — Step 0247:
  - added `ElementOrientation` with `transmittingAt`, `BoundOpticalElement` with `muellerAt`, and `UnmodelledBranchErr`;
  - added two `ItemEvaluationTests` facts and widened the error-rendering fact (constructor tests 1385 → 1387).
