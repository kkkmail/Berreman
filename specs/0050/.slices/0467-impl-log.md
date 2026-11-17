# Step 0467 — impl log

## Progress

- Read the task, the base and IMPLEMENT worker prompts, the project prompt, rulings R0001–R0005 (all
  confined to steps 0427, 0428 and 0436; nothing here touches the code they govern, so they are preserved
  by not touching it) and the slice.
- Read `PlannerSymmetry.fs` (steps 465/466: `CrystalAxis`, `TensorComponent`, `CrystalParameters.components`,
  `SampleCut.normalAxisOpt`), `MuellerInverse.fs` (`MeasurementConfiguration`, `Observable`, `orientForCut`,
  `internalAngle`, `gyrationProjection`) and `Fields.fs` (`IncidenceAngle.normal`).
- Wrote the impl-plan.
- Added `OpticalConstructor.Domain/MeasurementPlanner.fs` (`module MeasurementPlanner`): `DetectorReach`,
  `LookTilt`, `AxisAspect`, `LookSight`, `PlannerLook` (`tilt`, `aspectOf`, `sightOf`) and the
  `MeasurementConfiguration.plannerLookWith` extension — the six authorized contracts and nothing else.
  `LookSight` is declared before `PlannerLook` because `sightOf` returns it (the brief lists it sixth).
- `sightOf` follows rules (a)–(e) of the brief verbatim:
  - (a) index: `AlongPropagationInLook` with no transverse axis; `SeenInLook` oblique; at normal incidence
    `NoRetardanceInLook` under `IntensityOnlyReach`, else `SeenInLook` when the look's transverse axes are
    occupied by two or more distinct `PrincipalIndexComponent`s of `crystal.components`, else
    `NoRetardanceInLook`;
  - (b) gyration diagonal: along / `SeenInLook` under `FullMuellerReach` / `ReachMissesInLook`;
  - (c) gyration off-diagonal: `NormalIncidenceInLook` for a normal look with a principal axis on the normal,
    else `SeenInLook` under `FullMuellerReach`, else `ReachMissesInLook`;
  - (d) absorption diagonal: seen when its axis is transverse; (e) off-diagonal: seen when both are.
  `gyrationProjection` and `internalAngle` are not called (the brief: the former is the crystallographic
  reading; the latter is step 468's Snell cap).
- One `<Compile>` line each in the Domain project (after `PlannerSymmetry.fs`) and the test project (after
  `PlannerSymmetryTests.fs`), no comment.
- Added `OpticalConstructor.Tests/PlannerReportTests.fs` (`module PlannerReportTests`, ADD_TEST
  `OpticalConstructor.Tests.PlannerReportTests`) with the five facts of the brief, word for word.
- Build green; the new class 5/5 filtered.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (+1 line)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerReportTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (+1 line)

## Testing state

- Build (`dotnet build Berreman.slnx -c Release` from `Berreman/`): succeeded, 0 errors, 81 s. Warnings: only
  the exempt `NU1902` and the vendored MathNet's `SYSLIB0051` (predates the arc). Log:
  `.artifacts/0467-build-01.log`.
- `OpticalConstructor.Tests.PlannerReportTests` filtered (the class this step adds): 5/5 passed, 2 s. Log:
  `.artifacts/0467-planner-report-tests.log`.
- Elevation (`Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004` from the repo root, no base: all 13
  projects): `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 237s`, known findings 0; deadline 600 s.
  Log: `.artifacts/0467-elevation.log`.
- `constructor-unit-tests` (the gate selects `all_discovered`, so the whole suite, run after elevation finished):
  1848/1848 passed, 109 s (deadline 180 s). Previous checkpoint 1843; +5, exactly this step's five new facts.
  Log: `.artifacts/0467-constructor-unit-tests.log`.
- The twenty-one s45 invariants grep for strings none of the new files contain (checked with the decoded
  patterns over both new files: no match).
- Line endings: 0 CR bytes in all four touched files.

## Artifacts

- `.artifacts/0467-build-01.log`
- `.artifacts/0467-planner-report-tests.log`
- `.artifacts/0467-elevation.log`
- `.artifacts/0467-constructor-unit-tests.log`

## Gotchas

- `component` is F#-reserved: the `sightOf` parameter is `tensorComponent`, and the record field read is
  `held.``component``` as in `PlannerSymmetry.fs`.
- "Test-local records written as literals": as step 465 recorded, the test project already holds exactly those
  literal records (`CrystalMaterialFixtures`, `MaterialProxyTests.fs`), so the facts open that module rather
  than re-transcribing them (AGENTS.md: reuse, never copy). Each parameter is looked up by its `ParameterName`
  in `crystal.components`, so the facts also exercise step 466's name-to-component mapping.
- The transverse axes are gathered with `List.choose` over `AxisAspect` and the "two or more" test is a list
  pattern (`_ :: _ :: _`): no two-case union is projected to a `bool` (ELEV012) and no bare count appears.
- The brief's normal-incidence test compares `incidence.value` with `IncidenceAngle.normal.value`;
  `IncidenceAngle.create` normalizes 0° to exactly 0.0, so a design's `IncidenceAngle.create (Angle.degree 0.0)`
  is a normal look (the fifth fact asserts it).
