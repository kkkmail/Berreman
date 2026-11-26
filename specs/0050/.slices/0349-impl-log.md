# Step 0349 — impl log

## Progress

- [x] Read the protocol, project prompt, slice, AGENTS.md and the files the brief names (`MuellerReconstructionTests.fs`,
      `MuellerReconstruction.fs`, `MuellerReference.fs`, `FitUncertainty.fs`, `MeasuredDataTests.fs`, the reference
      scripts `matrix_step1_air_fit.py` `main` and `matrix_glue.py` `load_step1_models`).
- [x] `StagedMuellerPath.fs` and its compile line, directly after `FitUncertainty.fs`, no comment.
- [x] `MuellerReconstructionTests` re-pointed; `reduceAirTraces`, `lpFrameCoeffs` and `unwrapFit` deleted.
- [x] `StagedMuellerPathTests` added (ADD_TEST `OpticalConstructor.Tests.StagedMuellerPathTests`) and appended to the
      project's compile list, no comment.
- [x] Development runs of build, elevation, constructor-unit-tests, unit-tests (selected class) and the 20 s35
      invariants.
- [x] Bit-identity check of the old hand-composed stage 1 against `calibrateStageOne`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StagedMuellerPath.fs` (new): `module StagedMuellerPath`
  with, in order:
  - `StagedMuellerCaptureData` (the seven `MuellerRawRow list` fields);
  - `StagedMuellerError` (`EmptyCaptureSetErr of reason : string`,
    `AirCalibrationFitErr of family : Family * error : SolverError`);
  - `StageOneCalibrationData` with the `lpAnalyzer`, `cplAnalyzer` and `cplSource` members;
  - private `reduceAirTraces (darkMean : DarkMean)` and `lpFrameSourceModel`;
  - `calibrateStageOne`.
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj`: one
  `<Compile Include="StagedMuellerPath.fs" />` after `FitUncertainty.fs`.
- `Berreman/BerremanTests/MuellerReconstructionTests.fs`:
  - opens `OpticalConstructor.Optimization.StagedMuellerPath`;
  - deletes `reduceAirTraces`, `lpFrameCoeffs` and `unwrapFit`;
  - binds `opticsMuellerCaptures ()` after the seven kit file values;
  - the Stage-1 fact calls `calibrateStageOne (createMathNetSvd ()) (opticsMuellerCaptures ())`;
  - the section 7.2 fact binds one `captures`, takes `darkMean`, `lpAnalyzer`, `cplAnalyzer`, `cplSource` (with
    `LpSource`) from `calibrateStageOne solver captures`, and reads the bullshit checks from `captures`. Its Stage 3
    and assertions are unchanged.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StagedMuellerPathTests.fs` (new): `module StagedMuellerPathTests`,
  two facts over `createMathNetSvd ()` and fixed-literal rows.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: the new file appended at the
  end of the compile list.

## Decisions

- **The LP-frame helper is `lpFrameSourceModel`.** The brief names its shape (a private helper answering
  `Result<SourceCplModel, SolverError>`), not its name. It runs the same phased 2-column solve. `qLp` and `uLp` are
  bound as `CosineCoefficient` where they are read from the solution (ELEV001), then go to `sourceCplFromLpFrame`.
- **`calibrateStageOne` chains with `Result.bind`.** There is no `result { }` builder in the tree. `BenchNoise.fs` and
  `MuellerFitting.fs` use the same `Result.bind` / `Result.map` style. `Result.mapError` tags each solve's
  `SolverError` with its family (`LpLp`, `LpCpl`, `CplLp`), and nothing after a failed solve runs.
- **The empty-darkness reason** is "the darkness checks hold no capture row, so stage one has no dark mean". The fact
  asserts it contains "darkness checks".
- **The Stage-2 comment moved with the models.** The fact's note "analyzer retardance negative, only the relative sign
  is constrained (matrix_glue.py:211)" is now on `cplAnalyzer`'s doc, so it is documented once.
- **The section 7.2 fact keeps its five row names** (`lpLpRows` … `cplCplDay2Rows`), bound from `captures`, so its
  Stage 3 text is byte-for-byte unchanged.
- **`loadDataSet` and `degree` stay.** `loadDataSet` is the test's IO read of the kit. `degree` is still used by the
  always-run `freeCosineFit` fact.

## Testing state

Development runs of each gate's own command from `0349.gates`, over the final tree:

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, no `error` in stdout; only NU1902 (exempt) and the vendored MathNet SYSLIB0051 warnings | 76 s full, 11 s final incremental | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known 0; suppressed 1506 (+4, all ELEV004 in `StagedMuellerPath.fs`) | 226 s | 600 s |
| constructor-unit-tests | Passed!, 1555 of 1555 (checkpoint 342: 1553, so +2) | 71 s | 180 s |
| unit-tests (`BerremanTests.MuellerReconstructionTests`) | Passed!, 25 of 25, count unchanged | 3 s | 7200 s |
| invariant-s35-\* (20) | each grep found nothing, both as written and with `--untracked` | under 1 s each | 300 s |

- After the elevation run I reordered the section 7.2 fact so the four model bindings sit under the "Stages 1 and 2"
  header. That change touched only the test. I rebuilt (`.artifacts/0349-build-final.log`, BerremanTests recompiled)
  and re-ran the class: Passed!, 25 of 25. The product code did not change after the elevation run.
- Bit-identity: `.artifacts/0349-bit-identity-check.fsx` replays the deleted helpers verbatim against
  `calibrateStageOne` over the same rows. All eight values are bit-identical by `DoubleToInt64Bits`: dark mean, `z_LP`,
  `z_CPL`, `δ_an`, `θ_src`, `δ_src`, the CPL analyzer's negated retardance and the LP analyzer's zero dial.

## Artifacts

- `.artifacts/0349-build.log`, `.artifacts/0349-build-final.log`
- `.artifacts/0349-elevation.log`
- `.artifacts/0349-constructor-unit-tests.log`
- `.artifacts/0349-unit-tests.log`
- `.artifacts/0349-invariants.log`
- `.artifacts/0349-bit-identity-check.fsx` and `.artifacts/0349-bit-identity-check.log`

## Gotchas

- `reduceAirTraces` over an empty row list does not throw. Its only `List.averageBy` calls run per group, and an empty
  list has no group. So an empty LP-LP set reaches the solver as an empty design and comes back as
  `SingularDesignErr`, which is what the second new fact pins.
- Elevation's suppressed count rose by 4 (1502 to 1506). The SARIF shows all four are ELEV004 in
  `StagedMuellerPath.fs`: the `experiment`, `avgTotal` (twice) and `darkMean` `.value` reads of the moved body. The
  brief prescribes them, and the gate excludes ELEV004 by design (R7).
- `specs/0050/.manifest.state.json` was already modified when the round started. It is the supervisor's, not this
  step's.
