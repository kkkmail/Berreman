# Implementation log — slice 0087

## Progress

### Attempt 02

- Read the retry task, protocols, slice, judge finding, immutable obligation
  and current implementation. The reported blocked-parent path is confirmed
  in source: `openOrCreate` returns `LibraryDatabaseExn` as a value, so the
  outer exception handler in `recreate` cannot normalize it.
- Before changing tests: extend the existing file-recreation fact using a
  fresh fixture with a file obstructing the parent directory. Require
  `FileIoExn`, unchanged obstruction content and no backup creation. Keep
  both declared fact identities; add no third fact.
- Added the blocked-parent scenario to the declared file-recreation fact.
  Before the production repair, run that fact against the current code to
  establish the reported failure. Local verification follows the higher-priority
  coding instructions; the supervisor still owns formal gates and checkpoints.
- The targeted regression failed as expected (one failed, exit 2), specifically
  because the blocked parent returned the wrong typed error. Evidence:
  `0087-attempt-02-red.log` and `0087-attempt-02-red-results/recreation-red.trx`.
  Before repairing production: map only returned filesystem exceptions at
  `recreate`'s `openOrCreate` call, retaining the exception payload and all
  unrelated typed errors without modifying `openOrCreate`.
- Added the narrow `Result.mapError` normalization for `IOException` and
  `UnauthorizedAccessException`. Other `StorageError` cases, including
  non-filesystem `LibraryDatabaseExn` payloads, pass through unchanged.
  Before final verification: build Release/x64, then run the constructor
  suite, elevation analysis and the eleven resolved section invariants.
- Release/x64 solution build passed in 7.67 seconds with zero warnings and
  zero errors. The whole constructor suite passed all 910 tests with zero
  failures/skips (18.005 seconds), including the extended regression.
  All eleven resolved section invariants passed. Elevation is still running.
  Owned files use LF and the source diff has no whitespace errors or EOL churn.
- Elevation passed with zero findings across five projects in 140 seconds
  (ELEV004 excluded as prescribed). The TRX confirms both declared recreation
  facts and the unchanged `openOrCreate` blocked-parent fact passed. No source
  changes followed the successful build. Finalized the pinned handoff files;
  no implementation requirement is deferred and no checkpoint was edited.

### Attempt 01 (historical)

- Read the task, worker protocols, project prompt, repository instructions,
  naming conventions, slice, resolved gate roster and existing storage/tests.
- The initial working tree contains a supervisor-owned modification to
  `specs/0050/.manifest.state.json`; leave it untouched.
- Before tests: add exactly the two declared recreation facts to the existing
  file-database fixture, covering the preservation and error paths as well as
  empty memory/missing-file creation. No schema migration or new fixture folder.
- Added the two required facts before production implementation, plus the
  fixture's elevated backups-folder accessor. They exercise an existing entry
  with a schema-2 stamp, byte-preserving backup, failed source-copy preservation,
  stale sidecars, same-time numbering, missing-file creation and both memory modes.
- Read the immutable `S24-RECREATE` oracle; before production changes, move the
  name selector to private backup metadata, then add the Storage boundary with
  copy-before-delete ordering and the existing database creation/stamping call.
- Implemented the recreation boundary and moved the existing name selector;
  backups and recreation now use the same naming body. The source and sidecars
  are deleted only after `File.Copy` returns, and filesystem exceptions become
  `FileIoExn`. Memory and missing-file paths call the existing `openOrCreate`.
- Before local verification: normalize owned files to LF, build Release/x64,
  then run constructor tests and changed-project elevation analysis. The
  higher-priority coding instructions require local verification despite the
  worker's act-only instruction; formal acceptance remains the supervisor's.
- The Release/x64 solution build passed in 62.92 seconds, with zero errors
  and no warnings from our code. Its two warnings are the existing vendored
  MathNet `SYSLIB0051` diagnostics. The owned source diff has no whitespace
  errors or CRLF churn. Before runtime verification: run the full constructor
  suite and changed-project/dependent elevation analysis from that build.
- All 910 constructor tests passed locally, with zero failures/skips, in
  17.558 seconds: the required increase of two over the existing count of 908.
  All eleven s11 invariant commands from the resolved snapshot passed too.
  Elevation analysis remains in progress. No source changes followed the build.
- Elevation analysis passed with zero findings across Storage and its four
  dependent projects in 135 seconds, with ELEV004 excluded as prescribed.
  The TRX confirms both exact declared test identities passed. All requirements
  are implemented and local verification is complete.
- Before final handoff: finalize the pinned state-of-the-world and this log,
  retain the existing checkpoint count in the YAML, and confirm owned files
  have LF endings and a clean whitespace diff. No git mutation, version bump,
  external-repository write, analyzer change or checkpoint edit was made.

## Files modified

- `specs/0050/.slices/0087-impl-plan.md`: scope, approach and risks.
- `specs/0050/.slices/0087-impl-log.md`: durable progress and handoff record.
- `specs/0050/.slices/0087-state-of-the-world.md`: delivery, verification and
  supervisor handoff, preserving the existing checkpoint baseline.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/LibraryBackupStore.fs`:
  shared private backup name selector and `LibraryDatabase.recreate`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryDatabaseTests.fs`:
  two recreation facts and fixture backups-folder accessor.

## Testing state

Attempt 02: the targeted pre-fix regression failed as expected; the fixed
Release/x64 solution build passed with zero warnings/errors, all 910 constructor
tests passed without skips, and all eleven section invariants passed. Elevation
passed with zero findings in five projects (140 seconds, ELEV004 excluded).
The following measurements are historical attempt-01 evidence.

- Release/x64 solution build: exit 0 in 62.92 seconds, zero errors and no
  warnings from our code; two existing vendored MathNet `SYSLIB0051` warnings.
- Full constructor suite: exit 0, 910 passed, zero failures/skips, 17.558 seconds.
- Eleven s11 invariant commands from `0087.gates`: exit 0, every invariant holds.
- Changed-project/dependent elevation analysis: exit 0, zero findings in five
  projects, 135 seconds, with ELEV004 excluded as prescribed.

The supervisor owns formal acceptance and measured checkpoint counts. Its
existing checkpoint 85 records 908 constructor tests; the two new facts give
910 locally. No checkpoint is edited.

## Artifacts

Attempt 02 evidence:

- `specs/0050/.artifacts/0087-attempt-02-red.log`
- `specs/0050/.artifacts/0087-attempt-02-red-results/recreation-red.trx`
- `specs/0050/.artifacts/0087-attempt-02-build.log`
- `specs/0050/.artifacts/0087-attempt-02-tests.log`
- `specs/0050/.artifacts/0087-attempt-02-test-results/constructor-unit-tests.trx`
- `specs/0050/.artifacts/0087-attempt-02-elevation.log`
- `specs/0050/.artifacts/0087-attempt-02-elevation-reports/`
- `specs/0050/.artifacts/0087-attempt-02-invariants.log`

Attempt 01 evidence:

- `specs/0050/.artifacts/0087-local-build.log`
- `specs/0050/.artifacts/0087-local-tests.log`
- `specs/0050/.artifacts/0087-local-test-results/constructor-tests.trx`
- `specs/0050/.artifacts/0087-local-elevation.log`
- `specs/0050/.artifacts/0087-local-elevation-reports/`
- `specs/0050/.artifacts/0087-local-invariants.log`

## Gotchas

- `openOrCreate` deliberately wraps filesystem failures as `LibraryDatabaseExn`.
  Normalize those values only at `recreate`; an outer `try/with` cannot see
  exceptions already captured in a `Result`. Do not change the older contract.
- The new database legitimately creates WAL/SHM files while open; final absence
  is checked after disposing the replacement handle.
- Reuse the step-0086 name selector by moving it, not by copying its body.
- No production-scope deviation was necessary. Local verification follows the
  higher-priority coding instructions; formal acceptance remains separate.
