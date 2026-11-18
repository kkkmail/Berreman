# Step 0475 — state of the world

## Where we are

Spec 0050 section S14.1 (the Measurement Planner: kit inputs and the structural layer). The earlier steps built the
pieces this one checks:

- steps 465–472 built the Planner's inputs, its looks (`plannerLookWith`, step 467) and its structural report
  (`structuralReport`, step 469);
- steps 473 and 474 moved spec 0044's quartz and biaxial measurement designs into `BerremanTestSupport` (`QuartzSample`,
  `BiaxialDesign`).

This step lands the section's acceptance facts (S141-PLANNER-ACCEPTANCE). The Planner's layer-1 report, read over those
very designs, reproduces their ablation facts (001 §M "the planner reproduces existing facts"). No forward solve and no
fit runs.

## What's working

- Added `BerremanTests.MeasurementPlannerTests`. It runs the Planner's structural report over spec 0044's own quartz and
  biaxial designs, read as full Mueller looks.
- Quartz: C1 alone is found blind to g33 and unable to recover n_o, n_e and g33. The report proposes the x-cut, or a
  tilt capped at 40.4°. C1 + C3 and the full C1–C4 set recover every constant.
- Biaxial: a z-cut at normal incidence alone loses n3, g33 and the three off-diagonal gyration components. n3 and g33
  are offered further cuts or a tilt; g23, g13 and g12 a tilt alone. More cuts at normal incidence (z + x, B1) still
  leave exactly g23, g13 and g12. The full B1–B3 set recovers all nine.
- Registered the class last in `BerremanTests.fsproj`'s compile list.

## Tests

Development runs only; the arc-runner's gate engine is the authority.

- **build:** exit 0 in 85 s (deadline 600 s).
  - Warnings are only `NU1902` (exempt) and the vendored-MathNet `SYSLIB0051` (pre-existing).
  - No FS or MSB warning.
- **unit-tests, selected `BerremanTests.MeasurementPlannerTests`:** `Passed!`, 4 of 4 (failed 0, skipped 0) in 3 s
  (deadline 7200 s).
  - The count rises by 4, from 0 for the new class: exactly the four effect identities, by name in the TRX.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` in 235 s (deadline 600 s).
  - ELEV004 is excluded and the known findings are 0.
  - The suppressed count is 1767, the same as step 0474.
  - `BerremanTests` is outside the analyzers' scope, so the run re-checks the arc's product projects unchanged.
- **21 × invariant-s45-\*:** every decoded `git grep` finds nothing, also with `--untracked` so the new file is
  searched. The positive control finds 2.
- Line endings: LF only (0 CR bytes in both files). No BOM.

```yaml
gates:
  berreman_unit_tests: 4
```

## Architecture

- The class follows its neighbours' form:
  - a class `type MeasurementPlannerTests()` in `namespace BerremanTests`, with facts as `member _.` and `: unit`;
  - one-line-documented private `let` helpers, ahead of the members (FS0960).
- `reportOver crystal design` is the one way a fact reaches the Planner:
  - the design becomes looks through `plannerLookWith FullMuellerReach`;
  - the targets are the names of `crystal.parameterAxes`;
  - the report comes from `crystal.structuralReport`.
- The designs and ground truths are read from `BerremanTestSupport` by module name (`QuartzSample.c1`,
  `BiaxialDesign.normalIncidenceOn`, `BiaxialSample.triclinic`), never transcribed and never opened.
- The Snell caps are computed exactly as the step names them, so the comparisons with the report's caps are exact:
  - quartz: `internalAngle QuartzSample.quartzOrdinaryIndex IncidenceAngle.maxValue`;
  - biaxial: `internalAngle BiaxialSample.index1 IncidenceAngle.maxValue`.

## Deferred

Nothing. The new class with its four facts and the project entry both landed.

## Gotchas

- **`expectOk` is not used** (recorded deviation from the step's wording).
  - Step 335 fixed `InverseFitHarness.expectOk` to `Result<'Value, MuellerFitError>`. `structuralReport` answers
    `Result<_, PlannerError>`.
  - Widening `expectOk` would edit a file outside this step's authorized edits, and mapping the error would misstate it.
  - `reportOver` fails the fact with the planner error's own `describe` instead, as `expectOk` does with its error.
- **Same-named designs.** `QuartzSample`, `BiaxialDesign` and `AbsorbingBiaxialSample` share `configuration`,
  `fullConfigurations` and plate names. Qualify them; opening two lets the later `open` win silently.
- **`Assert.Equal` takes no message in xUnit v3.** The facts name the report's finding texts in three places:
  - comparing `findingTextsOf report` with `[]`;
  - the `failwith` of `targetFindingOf`;
  - the `Assert.Fail` of a missing off-axis need.
- **These facts pass on first run.** They are acceptance facts over production code that steps 467 and 469 already
  landed. Every expectation was derived from the planner's rules before the run.

## Changelog

- 2026-11-17 — Step 0475: added `BerremanTests.MeasurementPlannerTests`, whose four facts run the Planner's structural
  report over spec 0044's quartz (C1, C1 + C3, C1–C4) and biaxial (z-cut, z + x, B1, B1–B3) designs and reproduce their
  ablation facts (S141-PLANNER-ACCEPTANCE).
