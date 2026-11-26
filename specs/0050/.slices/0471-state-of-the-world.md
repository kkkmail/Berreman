# Step 0471 — state of the world

## Where we are

This is spec 0050 S14.1, the Measurement Planner. Steps 465–469 built layer 1 (symmetry, looks and the structural
recoverability report) over a crystal and a set of looks. Step 470 read the kit's lab profile. This step completes the
planner's four inputs from a kit and one of its studies (001 §M):

- the lab profile;
- the sample inventory: the kit's finite, free-standing plates of the material, each with the axis its cut puts on the
  normal;
- the prior: the study's opened working copy and its crystal at the current values;
- the targets: the study's unknown components of that material.

Each input is read through the study's own readers; nothing is typed in or stored.

## What's working

- Added `Study.plannerInputsWith`, which answers the kit's lab profile, its plates of the material, the opened copy
  as the prior and the study's unknown components as targets
- Added `PlannerPlate`: a finite substrate layer of the material, with its thickness, substrate kind, emission and the
  axis its orientation puts on the normal
- An unopened version refuses with `UnopenedPlannerMaterialErr` and a tabulated copy with
  `NonCrystalPlannerMaterialErr`; the lab profile's own error passes through
- Added three `PlannerInputsTests` facts (constructor suite 1858 → 1861)

## Tests

These are development runs. The arc-runner's gate engine is the authority; these are the worker's own checks.

- **build**: succeeded with 0 errors in 84 s (deadline 600 s). The warnings are the exempt `NU1902` and `SYSLIB0051`
  from the vendored `MathNetNumerics/Numerics/Exceptions.cs`, which already existed and is outside this step (see
  Gotchas).
- **elevation**: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` in 231 s (deadline 600 s), over the final source.
- **constructor-unit-tests**: 1861/1861 passed in 108 s (deadline 180 s). That is +3 over checkpoint 1858, exactly the
  three new facts. `PlannerInputsTests` and `PlannerReportTests` filtered together passed 18/18.
- **The twenty-one s45 invariants**: each decoded `git grep` finds nothing (`.artifacts/0471-invariants.log`).

No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1861
```

## Architecture

- `plannerInputsWith` adds no reader, cache or store of its own. It reuses:
  - the study's opened copy (`this.materials`) and `OpenedMaterial.currentPhysics.crystalOpt` (step 460);
  - `LabProfile.ofItems` (step 470);
  - `Study.unknownsWith (context.itemsUsedBy this)` (steps 362, 366);
  - `CrystalOrientation.normalAxisOpt` (step 466);
  - `Thickness.parameterValueOpt` (step 207).
- Refusals come in a fixed order: an unopened version, then a non-crystal copy, then the lab profile's error. So a
  study-side mistake is never hidden behind an unresolved item.
- A plate is an item whose `choiceOf` is a `StackSampleChoice` and whose sample's substrate layer is the material at a
  finite thickness. Its cut is that layer's orientation, the kit's one statement of how the crystal sits in it. A film
  layer of the material, or an infinite substrate, is no plate. Plates keep `context.items` order.
- Targets are the material's `MaterialParameterRef` unknowns, in `unknownsWith` order:
  - a link follower is left out, as `unknownsWith` leaves it out;
  - known and calibrated components stay in the prior;
  - item unknowns (a retardance, a gain) are not crystal components, and S14.2 weighs them.
- `PlannerPrior` holds the whole opened copy. Its parameter states already carry the known and calibrated values and
  the unknowns' priors, a tabulated source's sigma included. `crystal` is that copy's crystal at its current values,
  which carries the class and point groups the report states.

## Deferred

Nothing. Every requirement of the brief landed in this step.

## Gotchas

- **One edit outside the listed effects.** The two mandated `PlannerError` cases made step 469's exhaustive match in
  `PlannerReportTests.fs` (the fact `a target the crystal does not have refuses the report, naming it`) an FS0025
  compile error under the test project's `--warnaserror+:25`. The fix widens that one failing arm to an or-pattern
  over the three non-target cases. No assertion changed and no test identity was touched; step 470 made the same kind
  of edit. The match in `PlannerInputsTests.unresolvedReasonOf` got the same treatment, and that file is within this
  step's effects.
- **The tabulated material reuses `TabulatedStudyFixtures`** (`tableVersion`, `uncertainTableEntry`). That module is
  step 460's own public fixture, compiled before this file. Reusing it is how "built as step 460's facts build theirs"
  holds without copying it ("Reuse, never copy"). The crystal entry is new and test-local, with GUID prefix `4710…`,
  because `StudyMaterialTests`' `crystalEntry` is private.
- **What the error reasons say.** `UnopenedPlannerMaterialErr` names `material <GUID> v<n>` and how many versions the
  study opens. `NonCrystalPlannerMaterialErr` names the copy as `'<name>'` and its `material <GUID> v<n>`.
- **The second fact asserts more than the brief asks.** It also checks the refusal order: an unopened version wins
  over an unresolved item, and the lab's `UnresolvedPlannerItemErr` passes through unchanged for an opened crystal.
- **±5 % bounds.** For n_e and g33 they are `min` / `max` of 0.95·v and 1.05·v, because g33 is negative (−1e-5).
- **The pre-existing `SYSLIB0051` warning is in vendored MathNet** (`MathNetNumerics/Numerics/Exceptions.cs`), not in
  our F# code. This full build recompiled that project. The step neither touches nor is authorized to touch it.
- **Running the invariant greps.** From Python, `subprocess` with `bash` reaches the WSL stub on this machine, not Git
  Bash. Run the decoded commands in Git Bash directly.

## Changelog

- 2026-11-17 — Step 0471: added `PlannerError.UnopenedPlannerMaterialErr` and `NonCrystalPlannerMaterialErr`,
  `PlannerPlate`, `PlannerPrior`, `PlannerInputs` and `Study.plannerInputsWith` to `MeasurementPlanner.fs`, and three
  `PlannerInputsTests` facts; constructor suite 1861.
