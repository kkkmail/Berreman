# Step 0396 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice, its `.gates` snapshot and the 0395 SoW.
  - The task carries no operator rulings, and the project prompt's Operator note is empty.
  - None of the standing per-step rulings names step 0396.
- Checked every referenced type, and none needed a change:
  - `StudyParameterRef`, `UnknownOwner`, `FitCorrelation` and `SigmaFraction` (Studies.fs);
  - `MuellerRowIndex`, `MuellerColumnIndex`, `MatrixDiff`, `MatrixRank` and `ResidualRmse` (MuellerReconstruction.fs);
  - `ConditionNumber`, `ResidualVector`, `CoherencyEigenvalue` and `cloudeEigenvalues` (MuellerInverse.fs);
  - `ParameterSigma` and `ItemId` (Kit.fs), and `ExperimentName` (Experiments.fs).
- None of the 29 new type and case names has a namesake in Berreman or STL (`git grep -w` over `*.fs`).
- `FitRuns.fs`: added the twelve how-to items after `ObjectMatrixFit` and before `FitRun`, in order:
  - `FitEntryRef`, `FitCoordinate`, `FitProblemSubject`;
  - `JacobianColumnNorms` and `CorrelationMatrix`;
  - `FitProblemQuality`, `FitEntrySigma`, `RunResidual`;
  - `MuellerRealizability` and `ObjectRealizability` with its `realizability` member;
  - `SeriesProductCheck`, `SuspectReason`;
  - `ProfileOutcome`, `ProfilePoint`, `ParameterProfile`;
  - `FitRunResults`.
- `FitResultsTests.fs`: added the single fact the effects name, and the opens it needs.
- Built the solution, then ran:
  - the class;
  - two mutations;
  - the whole constructor suite;
  - elevation;
  - the twenty invariants.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/FitRuns.fs` (+149 lines).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/FitResultsTests.fs` (+37 lines): seven opens and one fact. The
  file was already in the `.fsproj`.

Both files are UTF-8 with LF line endings: `git diff --numstat` equals `--ignore-cr-at-eol`.

## Testing state

All runs were on 2026-11-14. Every run finished within its effective deadline.

- `build` passed: exit 0, 0 errors.
  - The first build took 77 s and the final one 19 s (deadline 600 s).
  - The only warnings are the exempt NU1902 advisories and the vendored MathNet `SYSLIB0051` pair, which step 395
    also recorded.
- `FitResultsTests` passed 3/3.
- Mutation checks:
  - The `-1.0e-12` floor was changed to `-1.0e-5`, then to `0.0`.
  - Each change fails only the new fact: the first because `-1e-6` reads as realizable, the second because round-off
    or `-1e-14` reads as unrealizable.
  - The file was restored byte for byte from a copy, then rebuilt.
- `constructor-unit-tests` passed, whole suite: 1700/1700 in 115 s (deadline 180 s).
  - The count is 1699 → 1700, +1, as the acceptance requires.
  - The TRX shows the new fact `Passed`.
- `elevation` passed: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 217 s (deadline 600 s). The run suppressed
  1581 findings (1579 at step 395) and recorded 0 known.
  - The Domain SARIF holds only ELEV004 for `FitRuns.fs`. ELEV004 is measured but not gated (R7).
  - Two of those hits are new: the `eigenvalue.value` reads in `realizability`.
- The twenty `invariant-s39-*` greps all find nothing. They were run by decoding each gate's command.

## Artifacts

- `specs/0050/.artifacts/0396-build.log`
- `specs/0050/.artifacts/0396-constructor-unit-tests.log`
- `specs/0050/.artifacts/0396-elevation.log`

## Gotchas

- **`realizability` reads `CoherencyEigenvalue.value`.** These are two ELEV004 hits (excluded).
  - The how-to adds no member to `MuellerInverse.fs`, and that file is outside this step's edits.
  - So the comparison unwraps in place, as `stageWith` does with `ParameterSigma` and `ParameterValue`.
- **The floor is relative to the eigenvalues' sum** (the trace, m₀₀). A NaN anywhere fails the `>=` test, so the
  matrix reads as unrealizable. That is the conservative side.
- **Shared record labels.** These labels are now also declared by earlier records:
  - `parameter` / `owner` (`FitRunEntry`, `FitRunParameter`, `StudyUnknown`);
  - `items` / `rank` / `rmse` (`ObjectMatrixFit`);
  - `entry` / `outcome` (`FitRunMember`).

  The existing code still builds unchanged. The new fact annotates its record expression `: ObjectRealizability`, and
  steps 397-404 should annotate theirs the same way.
- **No wire members.** None of the new unions carries a `value` tag, `tryOfWire` or `text`. The file form is step
  404's, and the how-to adds no other type or member.
