# Step 0469 — impl log

## Progress

- Read the task, the arc-runner and IMPLEMENT system prompts, the project prompt, the five operator rulings
  (R0001–R0005) and the slice. The rulings bind 0427, 0428 and 0436 and ask later steps to preserve their repairs
  (series dispatch, circular analyzer, element identity, study-fit runtime, solver allocation); this step touches
  none of that code (`MeasurementPlanner.fs` and `PlannerReportTests.fs` only), so all five are preserved unchanged.
- `MeasurementPlanner.fs`, after step 468's `CrystalParameters.snellCap`, each doc citing spec 0050 S14.1:
  1. `type Angle with member private this.plannerDegreesText`: degrees, `"F1"`, `CultureInfo.InvariantCulture`. The
     one renderer of every angle in a planner sentence (the tilted-cut plate, the tilt remedy and the off-axis
     finding), so the one-decimal invariant format is written once. Private: no new public surface beyond the
     step's contracts.
  2. `type SampleCut with member this.plateText`: `z-cut plate`, `x-cut plate`, `y-cut plate`,
     `<tilt>° tilted-cut plate`.
  3. `PlannerRemedy = AddPlateRemedy of SampleCut | ObliqueIncidenceRemedy of cap : Angle | FullMuellerRemedy`, with
     `text` giving the brief's three sentences.
  4. `TargetFinding = { target; reason; remedies }`, `OffAxisCutNeed = { targets; capAngle; cuts }`.
  5. `PlannerFinding = UnrecoverableTargetFinding of TargetFinding | OffAxisCutFinding of OffAxisCutNeed`, with
     `text`: `This kit can never recover <name>: <reason>.` and `You need an off-axis cut: <names, ", "> lie along
     the normal of every plate, and a tilt carries the wave at most <cap>° off it; add <"one " + plate, " or ">.`
  6. `RecoverabilityReport = { symmetry; reachable; findings }` with `unrecoverable` (targets of its target findings,
     in order) and `offAxisCutOpt` (its off-axis finding, by `List.tryPick`).
  7. `PlannerError = UnknownPlannerTargetErr of reason : string`, `describe` = `the crystal has no such parameter:
     <reason>`.
  8. `type CrystalParameters with member this.structuralReport targets looks`:
     - (a) each target name is resolved through `this.components`; a miss is `UnknownPlannerTargetErr "<name> (it
       has <the crystal's names, ", ">)"`; the list goes through `ItemEvaluation.sequenced` (the shared ordered
       Result collector), so the FIRST unknown target answers.
     - (b) each resolved target, in target order, goes by `this.targetSightOf looks` (step 468) into `reachable` or
       into an `UnrecoverableTargetFinding` of its reason and remedies.
     - (c) candidates: `[ZCut; XCut]` for `Uniaxial`, `[ZCut; XCut; YCut]` for `Isotropic` / `Biaxial`; a candidate
       sees a component when the normal-incidence, transmitted, full-Mueller look at its `normalAxisOpt` answers
       `SeenInLook`; it is missing when no look has its `normalAxisOpt`. Remedies by reason exactly as the brief's
       table (no look → every seeing candidate + full Mueller; along / no retardance → every seeing MISSING candidate
       + the tilt; normal incidence only → the tilt alone; full Mueller needed → full Mueller).
     - (d) when the looks' distinct `normalAxisOpt` list is exactly `[ Some a ]` (so the looks are non-empty and all
       share it), the targets whose component's `axes = [ a ]`, if any, give one `OffAxisCutFinding` after the target
       findings, `capAngle = this.snellCap`, `cuts` = the candidates whose `normalAxisOpt` is not `Some a`.
     - (e) `symmetry = this.symmetry`. No caching; nothing read beyond the arguments.
  - Opened `OpticalConstructor.Domain.MaterialComplexityEditor` (for `Anisotropy`'s cases),
    `OpticalConstructor.Domain.ItemModels` (for `ParameterName`) and `System.Globalization`.
- `PlannerReportTests.fs`: a private `crystalComponentNamed` (the `CrystalComponent` by name) that the existing
  `componentNamed` now calls, a private `reportOf` / `reportOver` (a refusal fails the fact through `describe`) and
  `targetFindingsOf`; the four facts named exactly as in the step's effects. Step 467's and 468's eight facts are
  unchanged.
- Build 1 failed: `ParameterName` and the `Anisotropy` cases were not in scope (FS0039, FS0049/FS0018); added the two
  opens; build 2 green.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs` (+210)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerReportTests.fs` (+145 / −1)

No project file changed: both files were already compiled.

## Testing state

- Build (`dotnet build Berreman.slnx -c Release -nologo -v:m` from `Berreman/`): attempt 1 failed (missing opens,
  see above); attempt 2 succeeded, 0 errors, 63 s; the only warning is the exempt `NU1902`.
  Logs: `.artifacts/0469-build-01.log`, `.artifacts/0469-build-02.log`.
- `OpticalConstructor.Tests.PlannerReportTests` filtered: 12/12 passed (467's five, 468's three, this step's four).
  Log: `.artifacts/0469-planner-report-tests.log`.
- Elevation (`Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004` from the repo root, no base: all 13
  projects): `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 236s`, known findings 0; the Domain project
  `findings=0`. Deadline 600 s. Log: `.artifacts/0469-elevation.log`.
- `constructor-unit-tests` (the gate selects `all_discovered`, so the whole suite, run after elevation finished):
  1855/1855 passed, 109 s (deadline 180 s). Step 468's checkpoint was 1851; +4, exactly this step's four new facts.
  Log: `.artifacts/0469-constructor-unit-tests.log`.
- Line endings: 0 CR bytes in every touched file; `git diff --numstat` equals `--ignore-cr-at-eol`.
- The s45 invariants: decoded all twenty-one commands; no added line matches any of their patterns.
- No run exceeded its deadline.

## Artifacts

- `.artifacts/0469-build-01.log`, `.artifacts/0469-build-02.log`
- `.artifacts/0469-planner-report-tests.log`
- `.artifacts/0469-elevation.log`
- `.artifacts/0469-constructor-unit-tests.log`

## Gotchas

- The uniaxial probe's Snell cap is 40.36° (`40.4°`), the triclinic probe's 41.13° (`41.1°`, its smallest index
  1.52); the sentence fact asserts both renderings.
- `[ (Some sharedAxis as sharedNormal) ]` over `List.distinct` of the looks' normals is the brief's "the looks are not
  empty and every look's normalAxisOpt is the same Some a" in one pattern: an empty look list gives `[]`, a mixed or
  tilted-cut set anything else.
- The NoRetardance and AlongEveryPropagation remedies add only MISSING plates: over a normal z-cut look the uniaxial
  n_o gets `AddPlateRemedy XCut` (the z-cut is present and does not see it anyway), while over no look g11 gets every
  candidate that sees it (`ZCut`, `XCut`).
