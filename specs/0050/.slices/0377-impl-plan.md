# Step 0377 — impl plan

## Approach

This is spec 0050 S11.2: the study's pre-fit check. It reuses the per-experiment checks moved into Domain at step 369
(`Experiment.prefitCheckWith`) and the data-change flag of step 370 (`MappedAttachment.changeWith`). It applies them
to every run a fit reads, in study order.

In `StudyForward.fs`, after `residualAgainst`:

1. `ExperimentPrefit = { experiment; check; changeOpt }`. It holds one run's checked points or typed failure, and its
   data's change flag. The flag is `None` without data, because the check already answers `MissingDataErr` then.
2. `Study.prefitWith (context) (fitted)`:
   - walk `enabledExperiments` in study order;
   - find each run by name in `context.kit.experiments`, else `MissingStudyRunErr` naming it;
   - keep the `ResidualRunUse` and `DarkLevelRunUse` roles;
   - for an attached run, read `attachmentStatus` (a `KitError` answers `StudyRunKitErr`) and set `changeWith status fitted`;
   - the check is `prefitCheckWith context.library context.experimentData context.kit.setups`;
   - collect with the shared ordered `ItemEvaluation.sequenced`.
3. `StudyForwardError` gains two cases:
   - `MissingStudyRunErr of reason : string`, rendered `the kit has no such experiment: <reason>`;
   - `StudyRunKitErr of error : KitError`, rendered `the kit cannot report the data's status: <describe>`.
4. `open OpticalConstructor.Domain.PrefitChecks` in `StudyForward.fs`. It already compiles before the file.

## Tests (`StudyForwardTests`)

- Step 374's Malus setup: HeNe, LP1 prep, LP2 analyzer and the intensity camera.
- A test-local `ExperimentDataProxy` that answers `readMapped` and `attachmentStatus` by attachment path.
- The runs:

  | Run | Role | Data | Study |
  |---|---|---|---|
  | E1 | Science | attached, unchanged | |
  | K1 | Dark (dark-line scan) | attached, changed since attach | |
  | X1 | Diagnostic | attached | |
  | E2 | Calibration | none | |
  | E3 | Science | attached | disabled |
  | P1 | Planned | | |

  The kit order differs from the study order, so "study order" is observable.
- New fact 1: with `fitted` = E1's path at another hash, the result is E1, K1, E2 in that order, with the exact
  checks and flags.
- New fact 2: a study naming E9, which the context's kit lacks, answers `MissingStudyRunErr`. A `KitIoErr` status
  answers `StudyRunKitErr`.
- Widen `every forward error renders its condition and its payload` with the two cases.

## Risks

- ELEV: unique names (checked: `ExperimentPrefit`, `MissingStudyRunErr`, `StudyRunKitErr` and `prefitWith` are
  unused), and no primitives in the new record.
- F# duplicate top-level `let` names in the test module: the new fixtures get fresh names (`k1`, `x1`, `p1`, `e9`,
  `prefit…`).
