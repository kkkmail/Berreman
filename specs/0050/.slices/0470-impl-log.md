# Step 0470 — impl log

## Progress

- Read the task, the arc-runner and IMPLEMENT system prompts, the project prompt, the five operator rulings
  (R0001–R0005) and the slice. The rulings bind 0427, 0428 and 0436 and ask later steps to preserve their repairs
  (series dispatch, circular analyzer, element identity, study-fit runtime, solver allocation); this step touches
  none of that code, so all five are preserved unchanged.
- Checked every new type and case name (`PlannerSource`, `OpticRole`, `PlannerOptic`, `DetectorRecording`,
  `PlannerDetector`, `LabProfile`, `PolarizerOptic`, `CompensatorOptic`, `CompoundOptic`, `RecordsIntensity`,
  `RecordsMuellerMatrix`, `RecordsPsiDelta`, `UnresolvedPlannerItemErr`) against Berreman and STL: none was taken.
- `MeasurementPlanner.fs`:
  1. Opens `OpticalConstructor.Domain.Kit` (`Item`, `ItemId`, `ItemLabel`, `ItemModelChoice`) and
     `OpticalConstructor.Domain.ItemEvaluation` (`ParameterValues`, `sequenced`), as the brief requires; nothing
     from Optimization. The library's `PolarizerBehavior` cases are written `Library.ComputedIdeal` /
     `Library.ConstantMueller` rather than opening `Library`.
  2. `PlannerError` gains `UnresolvedPlannerItemErr of reason : string`, `describe` = `the item's model is not in
     the library: <reason>`; its doc now says "a report or a lab profile".
  3. After step 469's `structuralReport`, each doc citing spec 0050 S14.1: `PlannerSource` (item, label, the
     preset's `waveLength`, its `tunableLimitsOpt`), `OpticRole`, `PlannerOptic`, `DetectorRecording`,
     `PlannerDetector` (item, label, recording, `noise : ParameterValues`) and `LabProfile` (sources, optics,
     detectors). The two unions are derived, never stored: no `value` tag. No `bool` anywhere.
  4. `LabProfile.ofItems items choiceOf`: each item, in order, becomes a one-item profile (or the empty profile);
     the list goes through `sequenced` (the shared ordered Result collector) and is concatenated, so the first
     refusal answers and nothing is answered in part.
     - `choiceOf` `None` → `UnresolvedPlannerItemErr <label>`.
     - `SourcePresetChoice (_, preset)` → a source at `preset.waveLength` with `preset.tunableLimitsOpt`.
     - `PolarizerPresetChoice (_, p)` → `PolarizerOptic` for `ComputedIdeal`, `CompoundOptic` for `ConstantMueller`.
     - `StackSampleChoice` → nothing (the plates are step 471's).
     - `LibraryModelChoice v` by `v.entry.model`: real polarizer → `PolarizerOptic`; retarder → `CompensatorOptic`;
       compound → `CompoundOptic` when a leaf at any depth (`leafForms`, recursive over nested compounds) is a
       `RealPolarizerModel` (`List.tryPick`, so no `bool`), else `CompensatorOptic`; black box and empty slot →
       nothing; intensity / polarimeter / ellipsometer → a detector recording `RecordsIntensity` /
       `RecordsMuellerMatrix` / `RecordsPsiDelta`, its noise `ParameterValues.ofParameters item.parameters`
       `checkedFor` the model's noise axes `n`; a missing or out-of-range value answers
       `UnresolvedPlannerItemErr "<label>: <the evaluation error's describe>"`, which names the axis.
  5. `reachOf`: polarimeter → `FullMuellerReach`; ellipsometer → `EllipsometricReach`; intensity → `FullMuellerReach`
     when two or more optics are `PolarizerOptic` / `CompoundOptic` (`List.choose`, then `_ :: _ :: _`), else
     `IntensityOnlyReach`. `reaches`: `[FullMuellerReach; EllipsometricReach; IntensityOnlyReach]` filtered to those
     some detector's `reachOf` answers. No other member.
- `PlannerInputsTests.fs` (new, `module PlannerInputsTests` in `namespace OpticalConstructor.Tests`, appended last to
  the compile list, no comment): nine `ItemDraft`s with `FixedMounting` over the Built-in catalogue
  (`BuiltinCatalogue.itemModel` for every model version, step 210's shipped entries; `BuiltinCatalogue.source`,
  `.preset`, `.sample`; GL at `SampleVersionId.firstOf glassPlate1mmId`), made items by `toItem`, and a test-local
  `choiceOf` answering each item's draft model. The three facts named exactly as the step's effects.
- Build 1 failed: `PlannerReportTests.fs(327)` FS0025, the existing fact `a target the crystal does not have refuses
  the report, naming it` matched `PlannerError` exhaustively and missed the new case. Added one explicit arm,
  `| Error (UnresolvedPlannerItemErr _ as error) -> Assert.Fail …`; no assertion removed, no identity changed (see
  Gotchas). Build 2 green.

- Wrapped the over-long lines the first draft added (the `ofItems` and `detectorOf` signatures, the binding-form
  refusal, a doc line, `reaches`); no behaviour change. While this ran, an elevation run had already started over the
  earlier source, so it was stopped (its stray `fsharp-analyzers.exe` killed), the solution rebuilt (build 3) and
  elevation re-run over the final source.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs` (+182 / −1)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerInputsTests.fs` (new, 236 lines)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (+1, the compile entry)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerReportTests.fs` (+1, the compile-forced arm)

## Testing state

- Build 1 (`dotnet build Berreman.slnx -c Release -nologo -v:m` from `Berreman/`): failed, FS0025 in
  `PlannerReportTests.fs` (above), 82 s. Build 2: succeeded, 0 errors, 26 s; the only warning is the exempt
  `NU1902`. (Build 1 also printed the vendored MathNet `SYSLIB0051` pair, as every full MathNet rebuild in this arc
  does, e.g. `0469-build-01.log`; not from our code, and absent from build 2.) Build 3, after the formatting-only
  edit: succeeded, 0 errors, 19 s, only `NU1902`; the Domain DLL was rebuilt and its dependents skipped (the
  reference assembly is unchanged).
- `OpticalConstructor.Tests.PlannerInputsTests` filtered: 3/3 passed.
- Elevation (`Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004` from the repo root, no base: all 13 projects),
  over the final source after build 3: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 226s`, known findings 0;
  the Domain project `findings=0`. Deadline 600 s.
- `constructor-unit-tests` (the gate selects `all_discovered`, so the whole suite; the gate's own command, run after
  elevation finished): 1858/1858 passed, 109 s (deadline 180 s). Checkpoint 469 was 1855; +3, exactly this step's
  three new facts.
- The s45 invariants: decoded all twenty-one commands and ran them: each finds nothing. `git grep` does not see the
  untracked new test file, so it was scanned with the same patterns directly: nothing.
- Line endings: 0 CR bytes in every touched file; `git diff --numstat` equals `--ignore-cr-at-eol`.

## Artifacts

- `.artifacts/0470-build-01.log`, `.artifacts/0470-build-02.log`, `.artifacts/0470-build-03.log`
- `.artifacts/0470-planner-inputs-tests.log`
- `.artifacts/0470-elevation.log`
- `.artifacts/0470-constructor-unit-tests.log`

## Gotchas

- **Edit outside the listed effects.** Adding the mandated `PlannerError.UnresolvedPlannerItemErr` makes step 469's
  exhaustive match in `PlannerReportTests.fs` (`a target the crystal does not have refuses the report, naming it`) a
  compile error (FS0025). The only conformant repair is one explicit failing arm for the new case: the fact's
  assertions and identity are unchanged, and the alternative (not adding the contract case) breaks the step.
- A bare binding form chosen as `LibraryModelChoice` (source / ideal polarizer / stack sample with no preset or sample)
  is not in the brief's mapping. It refuses as `UnresolvedPlannerItemErr "<label>: '<model>' takes a binding, and the
  choice binds none"`, as `ItemModelChoice.boundWith` refuses the same choice, since its physics does not resolve.
- `noise` holds every value of the item (`ofParameters`), checked against the noise axes: for an intensity detector it
  also carries `gain` and `darkOffset`. The brief says "ParameterValues.ofParameters of that detector item, checked
  against n", and the first fact compares with exactly that; step 486 reads the noise axes by name.
- The third fact also covers the noise refusals the brief requires (an out-of-range `elementUncertainty` of 1.5 on the
  Ellipsometer, a Phone without `throughputUncertainty`): both answer `UnresolvedPlannerItemErr` naming the item and
  the axis. These are added assertions inside an authorized new fact, not a new identity.
