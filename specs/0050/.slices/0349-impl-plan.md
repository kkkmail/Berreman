# Step 0349 — impl plan

## Approach

Stage 1 of the optics-mueller reference pipeline, today composed by hand in `MuellerReconstructionTests`, MOVES into
product code as the first half of the study's staged Mueller path (001 §Q AT-1, §S10; S10.3 brief).

1. New `OpticalConstructor.Optimization/StagedMuellerPath.fs` (`module StagedMuellerPath`), compiled directly after
   `FitUncertainty.fs`:
   - `StagedMuellerCaptureData` — the seven lab files' capture rows (`MuellerRawRow list` each).
   - `StagedMuellerError` — `EmptyCaptureSetErr of reason : string`, `AirCalibrationFitErr of family : Family * error : SolverError`.
   - `StageOneCalibrationData` — dark mean, LP zero, CPL zero, CPL analyzer retardance, CPL source model; members
     `lpAnalyzer`, `cplAnalyzer`, `cplSource` (moved from the section 7.2 fact's `analyzerLp` / `analyzerCpl` / `sourceCpl`).
   - private `reduceAirTraces (darkMean : DarkMean)` (moved; `.value` where it subtracts) and the LP-frame fit
     answering `Result<SourceCplModel, SolverError>` (the coefficients bound as `CosineCoefficient` where read).
   - `calibrateStageOne solver captures` — darkness check guard, dark mean, LP-LP / LP-CPL free cosine fits, the CPL-LP
     LP-frame fit, chained by `Result.bind` so a failed solve stops the path with `AirCalibrationFitErr`.
2. `BerremanTests/MuellerReconstructionTests.fs` — open `StagedMuellerPath`, delete `reduceAirTraces`, `lpFrameCoeffs`,
   `unwrapFit`; add `opticsMuellerCaptures ()`; the Stage-1 fact and the section 7.2 fact call `calibrateStageOne`.
   Stage 3 and the assertions are unchanged; every number is computed by the same float operations in the same order.
3. New `OpticalConstructor.Tests/StagedMuellerPathTests.fs` with the two typed-error facts, appended to the project.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StagedMuellerPath.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj`
- `Berreman/BerremanTests/MuellerReconstructionTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StagedMuellerPathTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- Bit-identity: `DarkMean.value` / `Retardance.degrees` / `Angle.radiansPerDegree` must return the very doubles the
  test used (`degree` was `Angle.radiansPerDegree`; the analyzer retardance is the same `.degrees` read).
- ELEV: new public names must be unique (checked by grep: none exist), and no bare primitive in a signature.
- `reduceAirTraces` over an empty row list must not throw (no group → no `averageBy` on an empty list).
