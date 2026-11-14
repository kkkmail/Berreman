# Step 0398 — impl log

## Progress

- [x] Read the inputs:
  - the protocol (`arc-runner.system-md`, `implement_worker.system-md`);
  - the project prompt and the slice;
  - step 0397's SoW;
  - `StagedMuellerPath.fs`, `MuellerReconstruction.fs` (stage-one fits) and `StagedMuellerPathTests.fs`.
- [x] Wrote the impl-plan.
- [x] Production: `reduceAirTraces` is public, and `AirTrace.fringeResidualAt` and `SourceCplModel.lpFrameResidualOn`
  are added.
- [x] Tests: three new `StagedMuellerPathTests` facts with the exact authorized IDs.
- [x] First local runs: build, constructor-unit-tests, unit-tests (selected) and elevation.
- [x] Doc and format touch-ups, mutation checks, then the final build and runs (see Testing state).
- [x] Wrote the state-of-the-world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StagedMuellerPath.fs`:
  - opens `OpticalConstructor.Domain.MuellerInverse`, for `ResidualVector`;
  - `reduceAirTraces` loses `private`, with its doc rewritten to cite S11.4 and its body byte-identical;
  - after it, `type AirTrace with member this.fringeResidualAt`;
  - then `type SourceCplModel with member this.lpFrameResidualOn`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StagedMuellerPathTests.fs`:
  - opens `Berreman.MathNetNumericsMath`, for `RealVector`;
  - one sentence is added to the module doc for S11.4;
  - two private helpers: `traceAnglesDeg` (0° to 160° by 20°) and `airTraceOf`;
  - the three new facts.

## Decisions

- **Where the 2(α − z) phase is formed.** It is formed in degrees, as `2.0 * (a - zero.degrees) * Angle.radiansPerDegree`,
  in both members. That is how `lpFrameSourceModel` phases the LP-frame design and how `freeCosineFit` reads α. The
  members and the fits they restate therefore read each dial identically.
- **The LP-frame coefficients are recomputed per call.** The member forms (1 − cos δ), q and u once and then maps over
  the trace. There is no new helper, because the slice says no other declaration changes. The two members share only
  `Array.map2 … − (n − 1)`. That is not a duplicated body: they are two different models.
- **`open MuellerInverse` rather than qualifying.** This follows `FitQuality.fs`. The ELEV014/015/016 uniqueness rules
  guarantee that opening it shadows no type, function or case. Record labels are not covered by those rules, but the
  build compiled every existing record expression unchanged, so inference was not re-targeted.
- **The physical branch is named in the doc, not enforced.** The member is total, like `sourceCplFromLpFrame`. Off the
  branch it is still the model's residual. Only the read-back is not one-to-one there.
- **Test strengthening, inside the three authorized facts.** These assertions are additions only:
  - fact 1 asserts the fit's RMSE is non-trivial (> 0.005, measured ≈ 0.0099), so "equals the fit's residual" cannot
    pass on a zero residual;
  - fact 2 asserts the read-back θ lies in (0°, 90°);
  - fact 3 reads every dial twice and out of order, asserts the single trace's angles are 0° to 160° sorted, and
    asserts the two experiments' traces differ, so a swapped order or a pooled normalisation would fail.

## Testing state

Development runs on 2026-11-14. The supervisor's gate engine is authoritative, and every run was within its deadline.

- `build`: exit 0, 0 errors. The first build took 81 s, and the final incremental one 9 s.
  - The Optimization and test binaries were checked to be newer than the restored source.
  - The warnings are the exempt NU1902 advisories and the vendored MathNet `SYSLIB0051` pair only.
- `constructor-unit-tests`: 1708/1708, 117 s then 116 s alone (deadline 180 s).
  - The count is 1705 → 1708 (+3).
  - The three new IDs are `Passed` in the TRX.
- `unit-tests` (`BerremanTests.MuellerReconstructionTests`): 25/25, 3 s then 2.5 s (deadline 7200 s).
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 227 s then 225 s on the final tree (deadline 600 s).
- The twenty `invariant-s39-*` greps all find nothing.
- Mutation checks, one test-project build with three mutations at once. Each fact exercises only its own member, so
  each red is attributable.
  - `a - zeroDeg` → `a + zeroDeg` in `fringeResidualAt` failed the fringe fact.
  - `+ uLp * sin` → `- uLp * sin` failed the LP-frame fact.
  - `List.groupBy (fun r -> r.experiment)` → `List.groupBy (fun _ -> ())` failed the two-experiments fact.
  - The ten other `StagedMuellerPathTests` facts passed.
  - The file was restored from a backup, verified identical with `cmp`, and the backup deleted. The final build
    recompiled from it.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0398-build-dev.log`, `0398-build-final.log`
- `0398-constructor-unit-tests-dev.log`, `0398-constructor-unit-tests-final.log`
- `0398-unit-tests-dev.log`, `0398-unit-tests-final.log`
- `0398-elevation-dev.log`, `0398-elevation-final.log`
- `0398-mutation-build.log`, `0398-mutation-tests.log`

## Gotchas

- The slice says "its AIR-label filter gone since step 388". That refers to `reduceAirTraces`. `calibrateStageOne`
  still selects the `(AIR#0)` rows before calling it, and that filter is out of this step's scope, so it is untouched.
- `MuellerReconstructionTests` takes about 3 s alone. Step 0397's 11-minute run was the `MuellerInverseTests` class.
