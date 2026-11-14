# Step 0413 implementation log

## Progress

- Read AGENTS.md, the worker and project prompts, slice 0413, naming conventions, and applicable manuals 010, 013 and 015. Reviewed the preceding handoff, the immutable S115-WINDOW-PUBLISH obligation and the existing publishing domain members, window guard, item recording, model resolution and test fixtures.
- Before implementation: add four authorized regression facts and the two semantic commands, preserving partial-write state and resolving all promotion dependencies through existing stores.
- Added the four authorized facts first, using a test-local calibration run, the existing real-folder fixture, linked CPL1/CPL2 parameters, and a shared material/catalogue database for promotion. Covered partial item writes and retry, repeated calibration, missing run/study, absent sigma, material resolution errors, immutable old files, and read-only guard order.
- Implemented both messages and a shared guarded run/study lookup. Publishing reuses the existing `writtenInOrder` collector and `kitItemsWith`; promotion calls the domain operations and existing `tryItemModelChoice` resolver. No new write orchestration or model-resolution implementation was introduced.
- Before the final substantive step: finish black-box dispatch coverage inside the authorized promotion fact, review the complete diff and write the final handoff.
- Completed black-box dispatch coverage in the same authorized promotion fact, including the exact stored model version requested, provenance, snapshot path, fitted matrix entry, absent item/model and domain refusal. Added exact revision-history and unchanged-item-field assertions.
- The focused development build/test command initially stopped during compilation (76.802 s): the fixture used a nonexistent `MaterialError.describe` and F#'s reserved `component` identifier. Corrected the fixture to reuse `MaterialError.caption` and renamed the local to `muellerPart`; no suppression or production workaround was added.
- The corrected focused development run passed all 12 StudyWindowModelTests cases with zero failures/skips in 36.049 s including compilation (MTP test duration 8.844 s), with no compiler warnings. Source review then consolidated the identical absent-item/model status into one branch; rechecking that final product edit before handoff.
- The final focused development run passed all 12 cases, zero failures/skips, in 24.218 s including compilation (MTP duration 6.688 s), with no warnings. Reviewed the final product diff: both commands are guarded before resolution, use the run's own study, reuse existing domain/storage operations, and preserve all unrelated model state. Wrote the state-of-the-world handoff; no slice requirement is deferred.

## Files modified

- Modified `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`.
- Modified `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyWindowModelTests.fs`.
- Created this log, `0413-impl-plan.md` and `0413-state-of-the-world.md` at the task's concrete paths.

## Testing state

The implementation is complete. Focused development validation rebuilt OpticalConstructor.Tests and its references in Release/x64, then ran only `OpticalConstructor.Tests.StudyWindowModelTests` through MTP: 12 passed, 0 failed, 0 skipped. The final command took 24.218 s (test runner duration 6.688 s). Exactly four facts were added; every prior identity/assertion remains.

This is development evidence, not execution or a verdict for any frozen gate. Full solution build, elevation, all constructor-unit-tests, all ui-smoke, selected KitWindowTests and the twenty s40 invariants remain for the supervisor. No gate deadline or acceptance condition was changed. No development run exceeded either the 600 s build allowance or 180 s constructor-test allowance; no gate duration is claimed. `commit_ready` remains false pending supervisor verification.

The initial compile failure and FS0046 warnings were repaired and the affected test source recompiled. The initial log also contains two existing SYSLIB0051 warnings from vendored MathNet Exceptions.cs; subsequent focused builds have no warnings. Vendored source was not changed.

## Artifacts

- `specs/0050/.artifacts/0413-development-tests.log`: initial fixture compilation failure.
- `specs/0050/.artifacts/0413-development-tests-02.log`: corrected build and 12 passing cases.
- `specs/0050/.artifacts/0413-development-tests-03.log`: final build and 12 passing cases.
- `specs/0050/.artifacts/0413-study-window-tests/study-window.trx`: final focused MTP report.

## Gotchas

- `specs/0050/.manifest.state.json` was modified on entry and will be preserved.
- The worker role assigns gate execution and verdicts to the supervisor; source review does not establish gate readiness.
- The material store and catalogue import proxy must share one database; the fixture preserves that existing convention. The read-only fact opens the prepared folder through `openKitAt ReadOnlyKitAccess`, then proves neither material nor model resolution is reached.
- Publication uses the existing `writtenInOrder` helper, so a failed second write retains the first revision in both the model and disk. Retrying publishes only the remaining item.
- Final product/test numstats are 82 and 444 additions respectively, identical with and without CR ignoring; diff whitespace checks are clean. Worker-authored text uses LF.
- No git mutation, version bump, notebook change, external repository edit, gate/census/checkpoint change, analyzer suppression or extra test identity was introduced.
