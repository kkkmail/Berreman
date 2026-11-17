# Step 0472 — state of the world

## Where we are

This is spec 0050 S14.1, the Measurement Planner. Steps 465–469 built layer 1: symmetry, looks, and the structural
recoverability report over a crystal and a set of looks. Steps 470–471 read the planner's four inputs from a kit and
one of its studies. This step closes S14.1's kit report (001 §M: "this kit can never recover X", "you need an off-axis
cut"):

- `PlannerInputs.looks` turns the kit's plates and detector reaches into layer 1's looks;
- `PlannerInputs.structuralReport` answers the report over them for the study's targets.

The numerical layer, the plan and its cache are S14.2's.

## What's working

- Added `PlannerInputs.looks`: each plate in item order under each reach of the lab profile, at normal incidence and at
  `IncidenceAngle.maxValue` (89°), on each branch its emission allows
- Added `PlannerInputs.structuralReport`, the crystal's structural report over the study's targets and the kit's looks
- A kit of z-cut plates calls for an x-cut, and one whose only detector is a phone behind a single polarizer can never
  recover g11 or g33
- Added three `PlannerInputsTests` facts (constructor suite 1861 → 1864)

## Tests

These are development runs. The arc-runner's gate engine is the authority; these are the worker's own checks.

- **build**: succeeded with 0 errors in 82 s (deadline 600 s). The warnings are only the exempt `NU1902` and the
  pre-existing `SYSLIB0051` in vendored `MathNetNumerics/Numerics/Exceptions.cs`, outside this step.
- **elevation**: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` in 231 s (deadline 600 s), run over every
  project with no comparison base, which is wider than the gate's touched-projects scope.
- **constructor-unit-tests**: 1864/1864 passed in 110 s (deadline 180 s). That is +3 over checkpoint 470 (1861),
  exactly the three new facts. `PlannerInputsTests` filtered on its own passed 9/9.
- **The twenty-one s45 invariants**: each decoded `git grep` finds nothing (`.artifacts/0472-invariants.log`). A
  positive control through the same runner does find the new member.

No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1864
```

## Architecture

- Both members are intrinsic members of the `PlannerInputs` record, and nothing new is stored or cached.
  - `looks` reuses `LabProfile.reaches` (step 470), `PlannerPlate.normalAxisOpt` and `emission` (step 471), and
    `IncidenceAngle.normal` / `maxValue` (`Fields.fs`).
  - `structuralReport` is exactly `prior.crystal.structuralReport targets looks` (step 469).
- The kit holds no angle limit, because no item model publishes one (step 208). So the table's range applies: normal
  incidence and the steepest incidence the engine takes. For layer 1 those two bound every tilt, since any oblique look
  sees what a 89° look sees, and the off-axis finding already carries the Snell cap.
- Looks nest as plate → reach → incidence → observable. With reaches fullest first, a plate's full-Mueller looks come
  before its ellipsometric ones. The `Emission` → `Observable` mapping is matched by case, transmitted before
  reflected.
- A kit with no plate of the material, or with no detector, has no look. Every target then falls to step 469's
  `NoLookReason`, whose remedies are the plates that would see it, then a full Mueller reach.

## Deferred

Nothing. Every requirement of the brief landed in this step.

## Gotchas

- **Record label clash.** `PlannerPlate`, declared after `PlannerLook`, shares the field label `normalAxisOpt`. So the
  comprehension in `looks` binds `let look : PlannerLook = …` explicitly. Without it, F# label inference can pick the
  later record.
- **Targets.** "Freeing all four components" is step 471's `crystalStudy` with n_o and g11 freed too
  (`everyTargetStudy`). `unknownsWith` lists the material's unknowns in its parameter order, so the targets are
  `[ n_o; n_e; g11; g33 ]`.
- **The facts assert a little more than the brief names**, all inside the three authorized identities:
  - the `EmitReflectedOnly` branch;
  - a kit without a detector has no look;
  - the off-axis sentence;
  - the phone-and-LP1 kit's only reach is `IntensityOnlyReach`;
  - the no-plate sentence for n_o.
- **The 89° incidence is written out in the test** and asserted equal to `IncidenceAngle.maxValue`. The fact pins the
  brief's number rather than echoing the constant.
- **Running the invariant greps.** Run the decoded commands in Git Bash (`/usr/bin/bash`). Step 471 found that Python's
  `subprocess` reaches the WSL stub.

## Changelog

- 2026-11-17 — Step 0472: added `PlannerInputs.looks` and `PlannerInputs.structuralReport` to `MeasurementPlanner.fs`,
  and three `PlannerInputsTests` facts; constructor suite 1864.
