# Implementation log — slice 0084

## Progress

### Attempt 02

- Attempt 02, before changes: read the retry task, worker protocols, gate contract, immutable S24-DATABASE-HANDLE obligation and existing implementation. vacuumInto currently relies entirely on SQLite's target refusal, which does not protect zero-byte files. The two existing facts will gain empty-target cases before adding the locked explicit refusal; their names and the four-fact count stay unchanged.
- Attempt 02, after regression changes: both existing facts now create an empty target and require LibraryDatabaseExn with the file still present and empty. The restore case uses the A+B backup against the live A-only handle, so assertLiveRestored checks that refusal did not restore B or replace the connection. Local regression build started; production code is still unchanged from attempt 01.
- Attempt 02 regression evidence: the local Release/x64 test-project build succeeded with zero warnings/errors. Both selected facts failed with the intended diagnostics: "VACUUM INTO overwrote an existing empty file" and "restore accepted an existing empty safety-backup file" (2 failed, 0 succeeded/skipped). Before the production fix: add File.Exists on the resolved full target path under syncRoot, returning LibraryDatabaseExn(IOException) before the command is created.
- Attempt 02, after production fix: vacuumInto now resolves and checks its target inside the existing lock/catch and returns the typed refusal for existing files before executing SQL. The same full path remains a bound parameter. restoreFrom reuses this refusal without a second implementation or any change to its validation/backup/copy order. Local verification of the final source follows.
- The final local Release/x64 test-project build succeeded with zero warnings/errors; all 895 constructor tests passed, zero failed/skipped. The four declared test identities and count are unchanged. Source diff whitespace checks are clean, the five owned files have LF endings, and source numstat matches with and without CR ignored. Updated the pinned state-of-the-world for the retry handoff.

### Attempt 01


- Read the worker protocols, project prompt, AGENTS.md, naming conventions, slice, compiled gate roster, S24-DATABASE-HANDLE oracle, and the referenced spec/manual constraints.
- Inspected the existing handle and tests. The handle has a single serialized connection and checks stamps before WAL changes; the test fixture already owns fresh folders below the test output.
- Before test changes: add the four required facts using the existing fixture and catalogue-import assertions, with all opened handles disposed.
- Added the four required facts and fixture path/connection-string helpers. Additional cases within those facts cover a blocked parent directory, a quoted backup filename, unchanged backup bytes on collision, absent backup directories, missing stamp/table, a missing source, and failure to create the safety backup without replacing live data.
- Before production changes: make origin immutable at construction, move stamp verification into a shared private member, initialize before publishing the handle, and add locked backup/restore members with exception-to-StorageError mapping.
- Implemented immutable origin, file-directory creation inside the catch, shared stamp verification, parameterized VACUUM INTO and read-only online restore. The same lock covers backup and replacement; the backup connection and contexts are scoped and disposed.
- Reviewed the source diff and resource lifetimes. Source numstat is identical with and without CR ignored. Before local verification: build the affected test project and run constructor tests to satisfy higher-priority development instructions, keeping these observations distinct from supervisor acceptance.
- Local Release/x64 build succeeded. It reported only the two existing SYSLIB0051 warnings in vendored MathNet Exceptions.cs (lines 24 and 77); no warning came from the changed F# code.
- All 895 constructor tests passed with zero failures and zero skips, including the four new facts. This is four more than the existing supervisor checkpoint 80 count of 891. No source changes were needed after verification.
- Wrote the required state-of-the-world at its pinned path and completed the handoff. No implementation requirement is missing; supervisor acceptance and commit readiness remain unclaimed.

## Files modified

- `specs/0050/.slices/0084-impl-plan.md`: approach and risks.
- `specs/0050/.slices/0084-impl-log.md`: progress and handoff trail.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryDatabaseTests.fs`: four new facts and reusable fixture paths.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/LibraryDatabaseStore.fs`: origin, shared schema validation, missing-folder creation, backup and restore.
- `specs/0050/.slices/0084-state-of-the-world.md`: completed implementation, local observations and supervisor handoff.

## Testing state

Attempt 02 local verification completed:

- Before the fix: Release/x64 test-project build succeeded (0 warnings/errors, 14.36 seconds), and the two extended facts failed for the exact empty-target overwrite/restore behavior (2 failed, 0 succeeded/skipped).
- After the fix: `dotnet build OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj -c Release -nologo -v:m`, from `Berreman/`: exit 0, zero warnings/errors, 7.42 seconds.
- After the fix: `dotnet test --no-build -c Release --no-ansi --results-directory C:/GitHub/Berreman/specs/0050/.artifacts/0084-attempt-02-test-results --report-trx --report-trx-filename constructor-tests.trx`, from the test project directory: exit 0, 895 succeeded, zero failed/skipped, 15.494 seconds.

Attempt 01 historical local verification:

- `dotnet build OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj -c Release -nologo -v:m`, from `Berreman/`: exit 0, zero errors, two existing vendored MathNet SYSLIB0051 warnings, 56.80 seconds.
- `dotnet test --no-build -c Release --no-ansi --results-directory C:/GitHub/Berreman/specs/0050/.artifacts/0084-local-test-results --report-trx --report-trx-filename constructor-tests.trx`, from the test project directory: exit 0, 895 succeeded, zero failed/skipped, 16.884 seconds.

No supervisor acceptance gate was run or certified. Its roster remains build, elevation, constructor-unit-tests (all discovered), and the eleven s11 invariants. Local observations do not certify those gates or author checkpoint baselines. Implementation is complete with no known blocker; acceptance and commit readiness await the supervisor.

## Artifacts

- `specs/0050/.artifacts/0084-attempt-02-red-build.log`
- `specs/0050/.artifacts/0084-attempt-02-red-tests.log`
- `specs/0050/.artifacts/0084-attempt-02-red-results/library-database-tests.trx`
- `specs/0050/.artifacts/0084-attempt-02-build.log`
- `specs/0050/.artifacts/0084-attempt-02-tests.log`
- `specs/0050/.artifacts/0084-attempt-02-test-results/constructor-tests.trx`
- `specs/0050/.artifacts/0084-local-build.log`
- `specs/0050/.artifacts/0084-local-tests.log`
- `specs/0050/.artifacts/0084-local-test-results/constructor-tests.trx`

## Gotchas

- `specs/0050/.manifest.state.json` was already modified on entry and is not part of this implementation.
- Higher-priority development instructions require local verification despite the worker protocol's act-only restriction. Supervisor acceptance remains separate. No functional scope deviation was needed.
- The original slice attributes existing-target refusal to SQLite. Attempt 02's regression evidence confirms SQLite permits existing empty files; the task's retry clarification requires our own locked refusal to satisfy the unchanged S24-DATABASE-HANDLE oracle. Both populated and empty targets now return LibraryDatabaseExn without being written. No gate/oracle change was needed.
