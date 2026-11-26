# Step 0475 — impl-plan

## Approach

Add `BerremanTests.MeasurementPlannerTests`, a class in `namespace BerremanTests`. Its four facts run the Planner's
layer-1 structural report over spec 0044's own designs. Step 473 moved the quartz design to
`BerremanTestSupport.QuartzSample` and step 474 moved the biaxial one to `BiaxialDesign`, so the facts reuse them. They
check that the report reproduces the ablation facts of `MuellerInverseTests` and `BiaxialInverseTests`. No forward solve
and no fit runs.

- `reportOver (crystal) (design)` reads each configuration as `plannerLookWith FullMuellerReach` (step 467). It asks
  `crystal.structuralReport` (step 469) about the names of `crystal.parameterAxes` (step 361).
- Each fact compares names, reasons, remedies and the off-axis need with `Assert.Equal`. Where it looks a finding up, the
  failure message lists the report's finding texts.
- The designs are named by module (`QuartzSample.c1`, `BiaxialDesign.normalIncidenceOn`, `BiaxialSample.triclinic`), not
  opened, because their names collide.

## Files

- New `Berreman/BerremanTests/MeasurementPlannerTests.fs`.
- `Berreman/BerremanTests/BerremanTests.fsproj`: one `<Compile>` appended at the end of the compile `ItemGroup`, with no
  comment.

## Expected values (derived from `MeasurementPlanner.fs` before writing)

- **Quartz, C1** (z-cut, normal incidence):
  - n_o: NoRetardance (one retarding index across the wave);
  - n_e and g33: AlongEveryPropagation;
  - g11: reachable;
  - remedies: `[ AddPlate XCut; Oblique cap ]`, because XCut is the only missing candidate of a uniaxial crystal;
  - off-axis: targets n_e and g33, cuts `[XCut]`, cap `internalAngle n_o 89°`.
- **Quartz, C1 + C3, and C1–C4:** every parameter is reachable. Two normals mean no off-axis finding.
- **Triclinic, z-cut normal only:**
  - n3 and g33: AlongEvery, remedies `[ AddPlate XCut; AddPlate YCut; Oblique cap ]`;
  - g23, g13 and g12: NormalIncidenceOnly, remedies `[ Oblique cap ]`;
  - off-axis: n3 and g33, cuts `[XCut; YCut]`;
  - cap: `internalAngle index1 89°`, index1 being the smallest index.
- **Triclinic, z + x normal, and B1:** exactly g23, g13 and g12 are unrecoverable, and there is no off-axis finding.
- **Triclinic, B1 + B2 + B3:** all nine are reachable, with no finding.

## Risks

- `InverseFitHarness.expectOk` is typed `Result<'Value, MuellerFitError>`, but `structuralReport` answers
  `Result<_, PlannerError>`. Step 335 fixed that signature, and its file is outside this step's edits. `reportOver`
  therefore fails the fact with the planner error's own `describe`, as `expectOk` does with its error. This is recorded
  as a deviation.
- The count must rise by exactly 4: the class declares exactly the four effect identities.
