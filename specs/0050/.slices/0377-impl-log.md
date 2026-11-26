# Step 0377 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice, and the step 369 / 370 / 374 / 376 code
- [x] Domain: `ExperimentPrefit`, `Study.prefitWith`, the two new `StudyForwardError` cases
- [x] Tests: two new facts, and the widened render fact
- [x] Build, constructor-unit-tests, elevation, invariants

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs`
  - opens `PrefitChecks`;
  - `StudyForwardError` gains `MissingStudyRunErr` and `StudyRunKitErr`, with their renders;
  - after `residualAgainst`: `ExperimentPrefit` and `Study.prefitWith`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyForwardTests.fs`
  - opens `PrefitChecks`;
  - widens `every forward error renders its condition and its payload` with the two cases;
  - adds the pre-fit fixtures and the two new facts at the end of the module.

## Decisions

- **A run is found before it is filtered.** Its role is only known once the run is found by name in
  `context.kit.experiments`. So an enabled study run the kit lacks answers `MissingStudyRunErr`, whatever its role
  would have been. A disabled run is never looked up.
- **The status comes before the check.** The data's status (`attachmentStatus`) is read before the run's
  `prefitCheckWith`, so a `KitError` refuses without reading the file. Both are read only for the runs a fit reads,
  so a Diagnostic, Ignored or Planned run's unreadable status refuses nothing. A fact asserts this for X1.
- **Collection.** Results are collected by the shared ordered `ItemEvaluation.sequenced`, so the first refusal in
  study order is the one reported. `List.choose id` then drops the unread runs.
- **The refusal reason.** It reads `<run>, which the study '<study>' names`, and the render adds
  `the kit has no such experiment: `. The study name is quoted, as other reasons in the repo quote names.
- **Test fixtures.** They extend step 374's bench and add no new item:
  - the `Malus` setup holds the HeNe, LP1, LP2 and the camera;
  - every run turns LP2 over 0…90° in 4 points;
  - the Dark K1 runs over the dark line.

## Testing state

| Gate | Result | Time (deadline) |
|---|---|---|
| build (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`) | exit 0, `0 Error(s)`, no lowercase `error` in the log. 8 warnings: NU1902 advisories, which are exempt, and the vendored MathNet `SYSLIB0051` that appears whenever MathNet recompiles. Neither is from this change | 77 s (600 s) |
| constructor-unit-tests (the gate's own command, whole suite) | `Test run summary: Passed!`, 1642 succeeded (1640 at checkpoint 376, +2), 0 failed, 0 skipped. In the TRX, the two new facts and the widened render fact are `Passed` under the exact authorized ids | 73 s (180 s) |
| elevation (`ASG_ARC_BASE_COMMIT` = `f395597c`, `-ExcludeRules ELEV004`) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0 | 224 s (600 s) |
| the twenty s37 invariants | each `section_invariants --find-nothing` exits 0 with `INVARIANT HOLDS` | seconds (300 s) |

## Artifacts

- `specs/0050/.artifacts/0377-build.log`
- `specs/0050/.artifacts/0377-constructor-unit-tests.log` (the TRX is at
  `OpticalConstructor.Tests/TestResults/constructor-unit-tests.trx`)
- `specs/0050/.artifacts/0377-elevation.log`
- `specs/0050/.artifacts/0377-invariants.log`

## Gotchas

- **Equality.** `StudyForwardError` now carries a `KitError`, and the facts compare it structurally.
  `KitError`'s `KitLockHolder` payload is a plain record, so equality still derives.
- **Names.** `ExperimentPrefit`, `MissingStudyRunErr`, `StudyRunKitErr` and `prefitWith` were checked unused across
  `Berreman/` before they were added (ELEV014/016/019).
- **`ContentHash`.** The test qualifies it as `LibraryEntryFiles.ContentHash` instead of opening `LibraryEntryFiles`
  into the long test module.
