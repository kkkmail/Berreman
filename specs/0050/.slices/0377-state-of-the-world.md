# Step 0377 — state of the world

## Where we are

This is spec 0050 section S11.2, the forward model of an experiment and the study residual.

- **Steps 369 and 370** moved the per-experiment pre-fit checks into Domain (`Experiment.prefitCheckWith`) and added
  the data-change flag (`MappedAttachment.changeWith`).
- **Steps 372–376** built the forward model and the row residuals.
- **This step** applies those checks to a whole study: every enabled run a fit reads, in study order, with its data's
  change flag. A failed check is reported, so the study view (S11.5) can list each one.
- **The later S11.2 steps** assemble the study residual over these runs.

## What's working

- Add `StudyForward.ExperimentPrefit`: a run, its checked points or typed `PrefitError`, and its data-change flag
- Add `Study.prefitWith`:
  - it checks every enabled Science, Calibration, Drift-check and Dark run in study order;
  - it leaves out Diagnostic, Ignored, Planned and disabled runs.
- Add `MissingStudyRunErr` and `StudyRunKitErr` to `StudyForwardError`, with their renders
- Add two pre-fit facts and widen the forward-error render fact (constructor suite 1640 → 1642)

## Tests

```yaml
gates:
  constructor_unit_tests: 1642
```

| Gate | Result | Time (deadline) |
|---|---|---|
| build (`dotnet build Berreman.slnx -c Release -nologo -v:m`) | exit 0, `0 Error(s)`, no lowercase `error` in the log. The 8 warnings are NU1902 advisories, which are exempt, and the vendored MathNet `SYSLIB0051`, which appears whenever MathNet recompiles. None comes from this step | 77 s (600 s) |
| constructor-unit-tests (the gate's own command, whole suite) | `Test run summary: Passed!`, 1642 succeeded (1640 at checkpoint 376, +2), 0 failed, 0 skipped. In the TRX, `a study's pre-fit checks every run a fit reads, in study order, with its data-change flag`, `a study naming an experiment the kit lacks, or a data status the kit cannot read, is refused` and the widened `every forward error renders its condition and its payload` are `Passed` | 73 s (180 s) |
| elevation (from arc base `f395597c`, ELEV004 excluded) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0 | 224 s (600 s) |
| the twenty s37 invariants | each `section_invariants --find-nothing` exits 0 (`INVARIANT HOLDS`) | seconds each (300 s) |

Nothing is deferred. Every duration is within its deadline.

## Architecture

`Study.prefitWith context fitted : Result<ExperimentPrefit list, StudyForwardError>` works as follows:

1. It walks `enabledExperiments` in study order.
2. It finds each run by name in `context.kit.experiments`. A run the kit lacks answers
   `MissingStudyRunErr "<run>, which the study '<study>' names"`.
3. It keeps a run whose `role.studyUse` is `ResidualRunUse` or `DarkLevelRunUse`. An `UnreadRunUse` run reads nothing.
4. For an attached run, it reads `context.experimentData.attachmentStatus data.attachment` and sets
   `changeOpt = Some (data.changeWith status fitted)`. A `KitError` answers `StudyRunKitErr`. A run without data
   has `changeOpt = None`, because its check already answers `MissingDataErr`.
5. The run's `check` is `prefitCheckWith context.library context.experimentData context.kit.setups`, kept as a
   value. A failed check is reported in the list, not raised.
6. It collects through the shared ordered `ItemEvaluation.sequenced`, so the first refusal in study order wins.
   `List.choose id` then drops the unread runs.

`fitted` is the attachments the study's last fit run read, empty before its first. Within one run, the status is
read before the file, so a status refusal reads no data.

## Deferred

Nothing.

## Gotchas

- **A missing run refuses whatever its role.** Its role is unknown until the run is found, so an enabled study run
  the kit lacks refuses. A disabled one is never looked up.
- **Only read runs are asked for a status.** A Diagnostic run with an unreadable status refuses nothing. The refusal
  fact pins this for X1.
- **Fixtures.** The facts reuse step 374's bench through a `Malus` setup (HeNe, LP1, LP2, camera). The kit's run
  order is deliberately not the study's, so "study order" is observable.
- **Error equality.** `StudyForwardError` now carries a `KitError`. Its structural equality still derives, because
  `KitLockHolder` is a plain record.

## Changelog

- 2026-11-13 — Step 0377: added `StudyForward.ExperimentPrefit` and `Study.prefitWith`, the study's pre-fit:
  - every enabled run a fit reads is checked and flagged in study order;
  - Diagnostic, Ignored, Planned and disabled runs are left out;
  - `MissingStudyRunErr` and `StudyRunKitErr` were added.

  Two new facts and the widened error-render fact (constructor suite 1640 → 1642).
