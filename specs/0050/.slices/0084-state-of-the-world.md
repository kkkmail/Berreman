# Step 0084 state of the world

## Where we are

Slice 0084 implements S24-DATABASE-HANDLE in section S2.4. The existing library handle reports its creation origin, creates missing file directories, exports compacted backups, and restores a validated backup through the same live connection. Attempt 02 adds explicit existing-target refusal, including zero-byte files, inside the locked backup boundary. Both authorized F# files are implemented and local verification succeeded. The supervisor owns acceptance and commit readiness.

## What's working

- Report created versus opened library origins without storing a new schema field.
- Create missing database directories inside the typed storage error boundary.
- Back up through parameterized VACUUM INTO and restore through SQLite online backup after validating the schema and saving the current library.
- Add four regression facts for origins, nested directories, backup preservation and restore visibility/refusal.
- Refuse populated and empty backup targets before executing VACUUM; preserve the empty file and live rows when a restore safety-backup target already exists.

## Tests

Attempt 02 first extended the existing vacuumInto and restoreFrom facts with zero-byte targets. Both failed before the fix, naming the empty-file overwrite and accepted restore safety-backup target. After the fix, the local Release/x64 build of OpticalConstructor.Tests and its dependencies succeeded with zero warnings/errors. The whole constructor suite passed: 895 succeeded, zero failed, zero skipped, including the four declared facts whose identities are unchanged. Logs and TRX reports are under `specs/0050/.artifacts/0084-attempt-02-*`.

Formal acceptance belongs to the supervisor: build, elevation, constructor-unit-tests (all discovered), and the eleven s11 invariants. No supervisor gate result is claimed. There is no known implementation blocker.

The YAML preserves the latest existing constructor baseline from supervisor checkpoint 80. It is not a new measured baseline for this slice.

```yaml
gates:
  constructor_unit_tests: 891
```

## Architecture

Origin is immutable and set only after successful initialization. The original stamp validation moved to a private member shared by opening and restore, preserving rejection before WAL changes. The same handle lock covers context use, VACUUM INTO, online restore and disposal. vacuumInto resolves the full target path and checks File.Exists within that lock and storage catch before creating the command; an existing file returns LibraryDatabaseExn(IOException). The full path remains a bound parameter. Restore uses a scoped read-only, unpooled source connection, validates its stamp before any backup write, calls the existing vacuumInto operation for the safety copy, and copies into the original connection only on success. IO failures map to LibraryDatabaseExn; schema refusals retain LibrarySchemaMismatchErr. No durable shape or format changed; no migration is needed.

## Deferred

No implementation requirement was deferred. Supervisor acceptance verification is the next assigned stage.

## Gotchas

- Higher-priority development instructions require local verification despite the worker protocol's act-only restriction. Local observations are separate from supervisor acceptance results.
- Backup and restore do not create backup directories; callers own them. The restore source is read-only and its connection is always disposed.
- SQLite permits an existing empty VACUUM INTO target. The explicit locked check is required by the retry task and unchanged acceptance oracle, and cannot be replaced by relying on SQLite's populated-target refusal.
- New facts also cover a blocked parent directory, quoted backup filename, missing backup directory, missing stamp/table, missing restore source and safety-backup failure.
- The pre-existing manifest-state modification remains untouched. Source numstat matches with and without CR ignored.
- No version, package reference, analyzer configuration, checkpoint, external repository, git index or history was changed.

## Changelog

- 2026-11-06: Added database origin, parent-directory creation, compacted backups, validated same-handle restore and four regression facts; local build succeeded and all 895 constructor tests passed.
- 2026-11-06 (attempt 02): Refused existing empty backup targets under the handle lock and extended the existing backup/restore facts; both regressions failed before the fix, then a zero-warning local build and all 895 constructor tests passed.
