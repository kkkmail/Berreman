# Step 0468 — impl log

## Progress

- Read the task, the arc-runner and IMPLEMENT system prompts, the project prompt, the five operator rulings
  (R0001–R0005) and the slice. The rulings bind 0427, 0428 and 0436 and ask later steps to preserve the dispatch,
  circular-analyzer, element-identity, study-fit and solver-allocation repairs; this step touches none of that code
  (`MeasurementPlanner.fs` and `PlannerReportTests.fs` only), so all five are preserved unchanged.
- `MeasurementPlanner.fs`, after step 467's `MeasurementConfiguration.plannerLookWith`:
  - `UnseenReason`: `NoLookReason | AlongEveryPropagationReason | NormalIncidenceOnlyReason | NoRetardanceReason |
    FullMuellerNeededReason`, in the brief's order, with `member this.text : string` giving the brief's five
    sentences word for word. No `value` tag: it is derived, never stored.
  - `TargetSight`: `ReachableTarget | UnreachableTarget of UnseenReason`.
  - `type CrystalParameters with`
    - `targetSightOf (looks : PlannerLook list) (tensorComponent : TensorComponent) : TargetSight`: no look →
      `UnreachableTarget NoLookReason`; otherwise every look's `sightOf this tensorComponent` is taken and the
      first entry of a closest-first table that some look answers decides: `SeenInLook` → `ReachableTarget`,
      `ReachMissesInLook` → `FullMuellerNeededReason`, `NoRetardanceInLook` → `NoRetardanceReason`,
      `NormalIncidenceInLook` → `NormalIncidenceOnlyReason`; none of them (every look `AlongPropagationInLook`) →
      `AlongEveryPropagationReason`.
    - `snellCap : Angle` = `internalAngle (List.min principalIndices) IncidenceAngle.maxValue`, the principal
      indices being the record's own `RefractionIndex` fields (n_o and n_e; index1 to index3).
  - Opened `Berreman.Geometry` (for `Angle`) and `Berreman.MaterialProperties` (for `RefractionIndex`).
- `PlannerReportTests.fs`: added `normalYCut` (the third principal cut for the triclinic g12 case), a private
  `componentNamed` lookup that `sightsOf` now calls (its five existing facts unchanged), a private
  `targetSightOf` helper over a parameter name, and the three facts named exactly as in the step's effects.
  Opened `Berreman.MaterialProperties` for the literal `RefractionIndex 1.544`.
- Build 1 failed on that missing `open` in the test file (FS0039 `RefractionIndex`); fixed, build 2 green.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs` (+61)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerReportTests.fs` (+56 / −5)

No project file changed: both files were already compiled by step 467.

## Testing state

- Build (`dotnet build Berreman.slnx -c Release -nologo -v:m` from `Berreman/`): attempt 1 failed (FS0039 in the
  test file, see above); attempt 2 succeeded, 0 errors, 25 s; warnings only the exempt `NU1902` and the vendored
  MathNet `SYSLIB0051` that predates the arc. Logs: `.artifacts/0468-build-01.log`, `.artifacts/0468-build-02.log`.
- `OpticalConstructor.Tests.PlannerReportTests` filtered: 8/8 passed (step 467's five and this step's three).
  Log: `.artifacts/0468-planner-report-tests.log`.
- Elevation (`Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004` from the repo root, no base: all 13
  projects): `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 243s`, known findings 0; deadline 600 s.
  Log: `.artifacts/0468-elevation.log`.
- `constructor-unit-tests` (the gate selects `all_discovered`, so the whole suite, run after elevation finished):
  1851/1851 passed, 112 s (deadline 180 s). Step 467's checkpoint was 1848; +3, exactly this step's three new
  facts. Log: `.artifacts/0468-constructor-unit-tests.log`.
- Line endings: 0 CR bytes in every touched file; `git diff --numstat` equals `--ignore-cr-at-eol`.
- The s45 invariants: no added line contains any of their patterns.
- No run exceeded its deadline.

## Artifacts

- `.artifacts/0468-build-01.log`, `.artifacts/0468-build-02.log`
- `.artifacts/0468-planner-report-tests.log`
- `.artifacts/0468-elevation.log`
- `.artifacts/0468-constructor-unit-tests.log`

## Gotchas

- `component` is F#-reserved, so the brief's `(component : TensorComponent)` parameter is `tensorComponent`, as
  at step 467.
- `RefractionIndex` is a plain single-case union over `double`, so `List.min` orders it structurally; no
  `.value` is read for the minimum.
- The closest-first table is a local `(LookSight * TargetSight) list` (two distinct types, ELEV011-clean) read by
  `List.tryPick`, so no two-case union is projected to a `bool` (ELEV012).
- The Snell-cap fact writes the uniaxial index as the brief does (`RefractionIndex 1.544`) and the triclinic
  probe's smallest index as `triclinicParameters.index1` (1.52 < 1.56 < 1.61), read from the shared fixture rather
  than transcribed again.
