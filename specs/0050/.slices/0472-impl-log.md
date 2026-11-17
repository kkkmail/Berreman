# Step 0472 — impl-log

## Progress

- Read the task, the system prompts, the project prompt, operator rulings R0001–R0005 and the slice. The rulings bind
  steps 0427, 0428 and 0436 and ask later steps to preserve that behavior. This step touches none of their files.
- Read `MeasurementPlanner.fs` (steps 465–471), `PlannerReportTests.fs` (step 469) and `PlannerInputsTests.fs`
  (steps 470–471), plus `Placement.Emission`, `MuellerInverse.Observable`, `Fields.IncidenceAngle` and
  `Study.unknownsWith` (material parameter order: n_o, n_e, g11, g33).
- Traced the three scenarios by hand through step 469's `sightOf` / `targetSightOf` / `structuralReport` before
  writing the tests (see the impl-plan).

- `MeasurementPlanner.fs`: two intrinsic members on the `PlannerInputs` record. Each has a two-line doc citing spec 0050
  S14.1.
  - `looks`: for each plate in item order, each reach of `lab.reaches`, each of `[ IncidenceAngle.normal;
    IncidenceAngle.maxValue ]` and each observable the plate's `Emission` allows. The `Emission` → `Observable` mapping
    is matched by case.
  - `structuralReport`: `this.prior.crystal.structuralReport this.targets this.looks`.
  - No other member was added.
- `PlannerInputsTests.fs`:
  - Opened `Berreman.Fields` (`IncidenceAngle`) and `OpticalConstructor.Domain.MuellerInverse` (`Observable`,
    `XCut`).
  - The module doc now also names the looks and the report.
  - New fixtures:
    - `nO`, `g11`, `probeCrystal`, `probeComponentOf`, `everyProbeComponent`;
    - `everyTargetStudy` (step 471's `crystalStudy` with n_o and g11 freed too, so the targets are all four);
    - `everyTargetInputsOver`, `kitReportOf`;
    - `steepestIncidence` (89°, written out);
    - `lookAt`.
  - The three facts the step names.
- Build: green in 82 s.
- `PlannerInputsTests` filtered: 9/9.
- Whole constructor suite (the gate selects `all_discovered`): 1864/1864 in 110 s.
- Elevation over all 13 projects: OK, 0 new findings, in 231 s.
- The 21 invariants: nothing found, with a positive control.
- Line endings: LF only.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs` (+33 / −0)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerInputsTests.fs` (+182 / −2)

Both are within the step's authorized effects. No other source file was touched.

## Testing state

Development runs only; the arc-runner's gate engine is the authority.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | 0 errors. Warnings are only `NU1902` (exempt) and vendored-MathNet `SYSLIB0051` (pre-existing, `MathNetNumerics/Numerics/Exceptions.cs`) | 82 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` (no comparison base, so all 13; ELEV004 excluded; known findings 0) | 231 s | 600 s |
| constructor-unit-tests | 1864/1864 passed (checkpoint 470 = 1861, so +3) | 110 s | 180 s |
| 21 × invariant-s45-* | each decoded `git grep` finds nothing | < 5 s | 300 s |

No run exceeded its deadline.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0472-build.log`
- `0472-planner-inputs-tests.log`
- `0472-constructor-unit-tests.log`
- `0472-elevation.log`
- `0472-invariants.log`

## Gotchas

- **Record label clash.** `PlannerPlate`, declared after `PlannerLook`, also has a `normalAxisOpt` field. So the look
  inside `looks`' comprehension is bound as `let look : PlannerLook = …` rather than left to label inference.
- **More asserted than the brief names.** The first fact also checks three things:
  - a plate that only reflects (`EmitReflectedOnly`, the third emission case) gives reflected looks alone;
  - a kit with no detector, so no reach, gives no look;
  - the targets are exactly n_o, n_e, g11 and g33.

  The second fact also asserts the off-axis sentence for the kit, and that the phone-plus-LP1 kit's only reach is
  `IntensityOnlyReach`. The third also asserts that the n_o finding reads "This kit can never recover n_o: no look of
  the kit measures a plate of it."
- **The 89° incidence is written out** (`IncidenceAngle.create (Angle.degree 89.0<deg>)`) and asserted equal to
  `IncidenceAngle.maxValue`, so the fact pins the brief's "89 degrees" without trusting the constant it checks.
- **Rulings.** R0001–R0005 bind earlier steps (0427, 0428, 0436). This step touches none of their files or behavior.
- **Shell heredoc.** A Bash heredoc holding the test names' apostrophes was refused by the tool's shell wrapper before
  it ran, and nothing was written. The tests went in through the Edit tool instead.
