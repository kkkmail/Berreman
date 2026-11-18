# Step 0475 — impl-log

## Progress

- Read the protocol, the project prompt, the five binding rulings (R0001–R0005) and the slice.
  - The rulings bind earlier steps (0427, 0428, 0436). None touches this step's two files. Each asks to preserve its
    rule through 0494, and a test-only class cannot breach any of them.
- Read `MeasurementPlanner.fs`, `PlannerSymmetry.fs` (`components`, `normalAxisOpt`), `Studies.fs` (`parameterAxes`),
  `QuartzSample.fs`, `BiaxialDesign.fs`, `BiaxialSample.fs` and `PlannerReportTests.fs`. Derived every expected value
  by hand from `sightOf` / `targetSightOf` / `structuralReport` before writing it (see the impl-plan).
- Wrote `MeasurementPlannerTests.fs`:
  - `type MeasurementPlannerTests()` in `namespace BerremanTests`, with the class doc the step names;
  - the four effect facts;
  - `reportOver` and three one-line-documented helpers: `namesOf`, `findingTextsOf`, `targetFindingOf`.
- Appended `<Compile Include="MeasurementPlannerTests.fs" />` at the end of the compile `ItemGroup`, with no comment.
- Development runs (below): build, the selected class, elevation and the 21 invariants.

## Files modified

- `Berreman/BerremanTests/MeasurementPlannerTests.fs` — new. Four facts:
  - **C1:** unrecoverable [n_o; n_e; g33] and reachable [g11]. g33 has `AlongEveryPropagationReason` and n_o
    `NoRetardanceReason`. Each of the three has `[ AddPlateRemedy XCut; ObliqueIncidenceRemedy cap ]`. The off-axis
    need is targets [n_e; g33], `cap`, cuts [XCut], with `cap = internalAngle QuartzSample.quartzOrdinaryIndex
    IncidenceAngle.maxValue`.
  - **C1 + C3 and C1–C4:** no finding, and every quartz constant reachable.
  - **Triclinic z-cut normal:** unrecoverable [n3; g33; g23; g13; g12].
    - g23, g13 and g12 have `NormalIncidenceOnlyReason` and `[ ObliqueIncidenceRemedy cap ]`.
    - n3 and g33 have `AlongEveryPropagationReason` and `[ AddPlate XCut; AddPlate YCut; Oblique cap ]`.
    - The off-axis need is [n3; g33], `cap`, [XCut; YCut], with `cap = internalAngle BiaxialSample.index1
      IncidenceAngle.maxValue`.
    - Over z + x normal and over B1: exactly [g23; g13; g12], each with the oblique remedy alone, and no off-axis need.
  - **B1 + B2 + B3:** no finding, and all nine reachable.
- `Berreman/BerremanTests/BerremanTests.fsproj` — one `<Compile>` line, last in the compile `ItemGroup`.

## Testing state

Development runs only; the arc-runner's gate engine is the authority (Invariant 6).

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0 in 85 s (deadline 600 s).
  - Warnings are only `NU1902` × 12 (exempt) and the vendored-MathNet `SYSLIB0051` × 4 (pre-existing).
  - No FS or MSB warning.
- **unit-tests, selected `BerremanTests.MeasurementPlannerTests`** (the gate's own command and filter): `Passed!` in 3 s
  (deadline 7200 s): total 4, succeeded 4, failed 0, skipped 0.
  - The TRX names exactly the four effect identities.
  - The class is new, so the selected count rises from 0 to 4.
- **21 × invariant-s45-\***: every decoded `git grep`, run with `--untracked` so the new file is searched, finds
  nothing. The positive control (`MeasurementPlannerTests`) finds 2.
- **elevation** (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`, no base, so all 13 projects):
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` in 235 s (deadline 600 s). The suppressed count is 1767, the same
  as step 0474, and the known findings are 0.
- Line endings: LF (0 CR bytes in both files). No BOM, as in the neighbouring test files.

## Artifacts

- `specs/0050/.artifacts/0475-dev-build.log`
- `specs/0050/.artifacts/0475-dev-unit-tests.log`
- `specs/0050/.artifacts/0475-dev-elevation.log`

## Gotchas

- **`expectOk` does not type-check here, so `reportOver` does not use it** (recorded deviation from the step's "through
  `expectOk`").
  - Step 335 fixed `InverseFitHarness.expectOk`'s parameter as `Result<'Value, MuellerFitError>`. `structuralReport`
    answers `Result<RecoverabilityReport, PlannerError>`.
  - Mapping a `PlannerError` into a `MuellerFitError` would misstate it: no fit or forward model runs here.
  - Widening `expectOk` would edit `InverseFitHarness.fs`, which is outside this step's authorized edits.
  - So `reportOver` matches the result itself. It fails the fact with the planner error's own `describe`, exactly as
    `expectOk` fails with its typed error.
  - With `expectOk` unused, `BerremanTests.InverseFitHarness` is not opened.
- **The designs are named by their module, not opened.** `configuration`, `fullConfigurations` and the plates share
  names across `QuartzSample`, `BiaxialDesign` and `AbsorbingBiaxialSample`, so a file opening two of them would have
  the later `open` win silently. Only the `BerremanTestSupport` namespace is opened.
- **Failure messages.** xUnit v3's `Assert.Equal` takes no message. The report's finding texts are named in three
  places:
  - the "no finding" comparisons compare `findingTextsOf report` with `[]`, so xUnit's diff prints the sentences;
  - a missing named finding (`targetFindingOf`) fails with every finding's text;
  - a missing off-axis need fails with `Assert.Fail` and every finding's text.
- **The facts can pass at once.** They reproduce facts over production code that steps 467 and 469 already landed. They
  are acceptance facts, not a red-then-green regression. Every expected value was derived from the planner's rules by
  hand before the run, and no expectation was adjusted after it.
