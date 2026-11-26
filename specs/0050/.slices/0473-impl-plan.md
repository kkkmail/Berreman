# Step 0473 — impl-plan

## Approach

A pure MOVE (spec 0050 .spec-md §4, §7; S14.1). The quartz ground truth and the C1-C4 measurement design leave the
`MuellerInverseTests` class and become `BerremanTestSupport.QuartzSample`, so step 475's Planner acceptance facts read
the design instead of transcribing it.

1. New `Berreman/BerremanTestSupport/QuartzSample.fs`, declared as `BiaxialSample.fs` is: `namespace
   BerremanTestSupport`, then `module QuartzSample =`. Its module doc is one to three lines: spec 0044's quartz ground
   truth and its C1-C4 design, shared by `MuellerInverseTests` and the Planner's facts (spec 0050 S14.1).
   - It opens only what the moved bodies read:
     - `Berreman.Constants`, `Berreman.Fields`, `Berreman.MaterialProperties`, `Berreman.Media`;
     - `OpticalConstructor.Domain.CrystalPhysics`, `OpticalConstructor.Domain.MuellerInverse`;
     - `OpticalConstructor.Optimization.MuellerFitting`.
   - It receives, in this order, with bodies and docs unchanged: `waveLength`, `quartzOrdinaryIndex`,
     `quartzExtraordinaryIndex`, `quartzG11`, `quartzG33`, `quartz`, `thickPlate`, `thinPlate`, `configuration`, `c1`,
     `c2`, `c3`, `c4` and `fullConfigurations`.
   - The two banners move above them, with only the edits the step names:
     - "Spec R3 puts the material in the TEST" becomes "in test code";
     - "the ablation fact below" (twice) becomes "`MuellerInverseTests`' ablation fact".
2. `BerremanTestSupport.fsproj`: append `<Compile Include="QuartzSample.fs" />` at the end of the compile list, with no
   comment. No reference is added, because Domain and Optimization are already referenced.
3. `MuellerInverseTests.fs`:
   - delete the moved declarations and both banners;
   - add `open BerremanTestSupport.QuartzSample` after `open BerremanTests.InverseFitHarness`;
   - keep `degree`, `solverParameters`, `forward`, `observe` and everything after them, and the class doc, unchanged.

## Risks

- **Shadowing.** The class-level `let`s shadowed every earlier `open` for the class body. Opening `QuartzSample` LAST
  gives the same resolution, so every fact reads the same values.
- **Bit-identity.** Every expression is copied unchanged. A module value is computed once rather than once per
  instance, which yields the same value.
- **ELEV.** The assembly `BerremanTestSupport` does not match the analyzers' `inScopePatterns` (`^Softellect\.Berreman\.`),
  and `BerremanTests` is a test project. The `double` parameters of `configuration` therefore stay exactly as they were.
- **LF line endings** on the new file.

## Verification (development only; the gate engine is the authority)

- Build the solution.
- Run `BerremanTests.MuellerInverseTests` filtered, because it is the selected and touched class.
- Run elevation.
- Run the 21 invariants as `git grep`.
- Kit-acceptance (`KitAcceptanceTests`) only compiles against the changed support project and reads none of the moved
  names. Its about 30-minute run is left to the gate engine.
