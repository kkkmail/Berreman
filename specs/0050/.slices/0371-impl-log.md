# Step 0371 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice, its gates snapshot, `.manual/013` (elevation from
  the arc origin) and `.manual/016` (the shared ordered Result collector). No operator note or ruling is attached.
- Read the seams the step builds on: `ImportProfiles.fs` (`MappedPoints`, `ScanQuantity`, `ExperimentScan.quantities`,
  `validatedFor`, `ScanCell.displayValue`, `MappedReading`, `KeyedObservation`), `Experiments.fs` (`ScanAxis`,
  `ScanSetting`, `ExperimentScan`), `MeasuredData.fs` (series, `MeasuredAbscissa`, `validateAgainstExperiment`),
  `MuellerReconstruction.fs` (`MuellerRawRow`), `MuellerInverse.fs` (`MuellerObservation`), `AnalysisFunctions.fs`
  (`PsiDelta`), `ItemEvaluation.fs` (`sequenced`). Confirmed none of the new type, case or member names exists
  (ELEV014/016/019).
- Read STL's ELEV001 / ELEV005 analyzers (read-only): `SweptValue point.x.value` is ELEV001 (a cross-type re-wrap), so
  the conversion is a named member on the source type.
- New `OpticalConstructor.Domain/StudyForward.fs`, `module StudyForward`, compiled after `PrefitChecks.fs` (one
  `Compile` line, no comment), in order: `StudyForwardError` (`UnpairedPointsErr`, `describe`), `RecordedReading`
  (`RecordedSignal` / `RecordedPsiDelta` / `RecordedMueller`), `MeasuredRow` (`SettingsRow` / `ObservationRow`),
  `MeasuredAbscissa.toSweptValue`, and `MappedPoints.measuredRowsFor`. Each point maps to a `Result`, folded by
  `ItemEvaluation.sequenced` (points in order, the first refusal wins). No cache or option added.
- New `OpticalConstructor.Tests/StudyForwardTests.fs`, `module StudyForwardTests`, appended last in the test project's
  compile list, with the three census facts over test-local `ScanAxis` scans (`polarizer`, `sample`, `dial-plate`).
  The observations reuse `ImportProfileTests.observationOf`.
- Built, ran the whole constructor suite, the twenty s37 invariants and elevation from the arc base (below).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs` (new, 105 lines).
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (+1 `Compile`).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyForwardTests.fs` (new, three facts).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (+1 `Compile`, last).

All LF (no CR in the new files; `git diff --numstat` equals `--ignore-cr-at-eol`). `specs/0050/.manifest.state.json`
was already modified at round start (supervisor state) and is untouched by this round.

## Testing state

All run locally after the final edit (Release x64):

| Gate | Result | Duration | Deadline |
|---|---|---|---|
| build | exit 0, `0 Error(s)`, no "error" in the output; warnings only the pre-existing NU1902 advisories and vendored MathNet SYSLIB0051 | 81 s | 600 s |
| constructor-unit-tests | `Test run summary: Passed!`, 1623 succeeded, 0 failed, 0 skipped (checkpoint 370: 1620; +3); the three new facts `Passed` in the TRX | 76 s | 180 s |
| elevation (arc base `f395597c`) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` (all 13: the slnx changed since the arc base), exit 0 | 231 s | 600 s |
| twenty s37 invariants | each `section_invariants --find-nothing` exit 0; their patterns also find nothing in the two new (untracked) files | < 5 s each | 300 s |

Every requirement of the slice landed this round; nothing is deferred.

## Artifacts

- `specs/0050/.artifacts/0371-build.log`
- `specs/0050/.artifacts/0371-constructor-unit-tests.log`
- `specs/0050/.artifacts/0371-elevation.log`

## Gotchas

- Deviation (recorded when made): `MeasuredAbscissa.toSweptValue` is one member beyond the four contract symbols. An X,Y
  abscissa is already in the varied quantity's display unit (`validateAgainstExperiment` admits `p.x.value` as is), so
  the conversion is identity, but writing `SweptValue point.x.value` is ELEV001; AGENTS.md makes a cross-type reuse a
  named member on the source type. It lives in `StudyForward.fs`, the only Domain file this step may edit (precedent:
  `MuellerExperimentLabel.rowKey` in `ImportProfiles.fs`).
- "A series point under more than one quantity is refused" is read as an INTENSITY series point: the how-to and its
  test place an ellipsometric point under the two-axis scan, setting only the scan's `VaryR2` quantity. An ellipsometric
  point is never refused; with no angle of incidence, or no `VaryR2` quantity, it has no setting. With two `VaryR2`
  quantities the first is set (`List.tryFind`).
- Angles are read in degrees through `Angle.degrees`, as `validatedFor` reads them, with no rounding: 60° comes back as
  59.999999999999993, so the test's expected incidence setting is `incidence.degrees` (and separately asserted within
  1e-12 of 60). 20° round-trips exactly, so the capture's expected setting is a literal 20.
- Refusal reasons: `point <n> holds one scan value, and the scan varies <count>` (series point, capture row) and
  `point <n> holds <cells> scan value(s), and the scan varies <count>` (column row), `<n>` the 1-based place among the
  points given, as `MappedPointNumber` counts them.
- `measuredRowsFor` does not re-check ranges or units; that stays `validatedFor`'s (the pre-fit check runs it first).
