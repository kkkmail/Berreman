# Step 0356 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice spec, the `.gates` snapshot and
  the standing rulings (016's ordered Result collector among them).
- Mapped every reference to the retired gain symbols: only `StagedMuellerPath.fs` and
  `MuellerReconstructionTests.fs` used them.
- Created `OpticalConstructor.Domain/Studies.fs` (`module Studies`) with `StudyError`, `AirRepeatPair`,
  `SessionGainModel`, `AirAnchorGain`, `GainInterpolation`, `SessionGainRule` (+ `gainAt`) and the
  `SessionGainModel.ruleWith` extension; one `<Compile Include="Studies.fs" />` directly after `ImportProfiles.fs`.
- Deleted `RepeatPairGain`, `InterpolatedGain`, `CplCplGain` (`Day1Interp` / `Day2Single`), `SplitGain`,
  `GainCalibration` and `familyGain` with their docs from `MuellerReconstruction.fs`; the Stage-3 section
  comment and `correctSignal`'s doc now name `SessionGainRule.gainAt` (spec 0050 S11.1).
- `StagedMuellerPath.fs`: the nine AIR runs are private `ExperimentName`s; `airBlock` keeps the rows whose
  `experiment.nameText.toExperimentName` answers `Ok` of the run; `airAnchors` builds the nine `AirAnchorGain`s
  in the reference order; six private gain models (`lpLpGainModel` … `cplCplDay2GainModel`) resolve through
  `ruleWith` in `sessionRules` (refusal → new `StagedMuellerError.SessionGainErr of error : StudyError`);
  `prepareRow` divides by `(ruleOf row).gainAt row.capturedAt`, a CPL-LP row strictly before the split taking the
  pre-check rule; `StagedMuellerData.airGains : AirAnchorGain list` replaces `gains` / `cplCplDay1Gain`.
- `MuellerReconstructionTests.fs`: renamed the Stage-3 fact to
  `Stage-3 signal reduction pins darkSubtract, scalarGain, airIdentityPred and correctSignal (always-run)` and
  removed its `familyGain` block (its rules moved to `StudySessionTests`); the §7.2 fact reads each gain from
  `staged.airGains` by run name against the same `MuellerReference` value, `failwith` on a missing run.
- New `OpticalConstructor.Tests/StudySessionTests.fs` (`module StudySessionTests`), appended last in the
  test fsproj, with the four facts the step names.
- Renamed the six private gain-model values away from the substring `SplitGain` and reworded two comments,
  so a plain grep for any retired symbol finds nothing.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StagedMuellerPath.fs`
- `Berreman/BerremanTests/MuellerReconstructionTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudySessionTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Decisions

- **Compile position.** The step says "directly after `ImportProfiles.fs` (step 300), the last Domain file";
  since step 300, `ExperimentDataProxy.fs`, `ExperimentDataLoad.fs`, `DataSplits.fs` and `ExperimentMatrices.fs`
  follow it. I followed the explicit placement (directly after `ImportProfiles.fs`). No later step that edits
  `Studies.fs` (357–471) names anything from those four files, so the position does not block them.
- **Ordered first-error lookup in `ruleWith`.** `ItemEvaluation.sequenced` (the shared ordered Result collector
  of ruling 016) is typed to `ItemEvaluationError`, and `ItemEvaluation.fs` is outside this step's authorized
  edits, so `ruleWith` does not reuse it. It looks each run up once (`anchorOf`, shared with the interpolated
  case), takes the first `Error` in model order with `List.tryPick`, and otherwise the gains with
  `List.choose Result.toOption`. That is a lookup, not a second copy of the collector's fold.
- **Arithmetic kept bit-identical.** One run → its gain; two → `AirGain (0.5 * (a.value + b.value))` (moved
  `meanPair`, model order = old `gainA`, `gainB`); more → sum over count; interpolation → the `Day1Interp`
  arm moved verbatim into `SessionGainRule.gainAt`.
- **Gain models born as values.** The six models are module-level private values, so no DU is wrapped around
  a local at a call site (ELEV001).
- `airBlock`'s empty-block message still names the run (`the AIR block <run> holds no capture row …`), which
  the existing `StagedMuellerPathTests` fact asserts.

## Testing state

Development runs (the gate engine's verdict is separate):

- `dotnet build Berreman.slnx -c Release`: succeeded, 0 errors (77 s). Warnings: NU1902 (exempt) and the vendored
  MathNet C# `SYSLIB0051`, which appears whenever that project recompiles (also in 0001–0003's logs); no F#
  or MSBuild warning from our code.
- constructor-unit-tests (whole suite): Passed, 1572 succeeded / 0 failed / 0 skipped, 70 s. The last checkpoint
  (354) is 1568, so +4. The four `StudySessionTests` facts are in the TRX.
- unit-tests `BerremanTests.MuellerReconstructionTests`: Passed, 25 succeeded / 0 failed / 0 skipped, 3 s. Class
  size unchanged (26 fact attributes in the file before and after); the renamed identity is in the TRX.
- The twenty s36 invariants: each command reports `INVARIANT HOLDS`.
- elevation (no base, all 13 projects): `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known 0,
  suppressed 1510 (ELEV004 excluded), 220 s.
- Final pass after the private renames: rebuilt (28 s, NU1902 only), constructor 1572/1572 (71 s; all four
  `StudySessionTests` Passed in the TRX), `MuellerReconstructionTests` 25/25 (the renamed Stage-3 fact and the §7.2
  end-to-end fact Passed), elevation OK as above, the 20 invariants hold, and a word-boundary grep for every retired
  symbol finds nothing under `Berreman/`. No run exceeded its deadline.

## Artifacts

- `specs/0050/.artifacts/0356-build.log`
- `specs/0050/.artifacts/0356-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0356-dev-unit-tests.log`
- `specs/0050/.artifacts/0356-dev-elevation.log`

## Gotchas

- `InterpolatedGain` is a prefix of the mandated `InterpolatedGainRule`; a "must find nothing" grep for the
  retired record needs a word boundary (`-w`).
