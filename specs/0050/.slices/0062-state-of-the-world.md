# Step 0062 state of the world

## Where we are

Slice 0062 implements S21-OPEN-STAMP on top of the existing library model.
The storage handle, error contract and four declared facts are written in
the four authorized product/test files. Gate execution and acceptance belong
to the arc-runner supervisor under the IMPLEMENT worker's act-only protocol.

## What's working

- Added the library schema version, typed mismatch payload and database exception descriptions.
- Added a locked, unpooled SQLite handle with foreign keys, file WAL and schema stamping.
- Rejected incompatible or unstamped databases before persistent changes and rejected calls after disposal.
- Added all four declared facts covering lifetime, reopen, schema rejection and EF failure recovery.

## Tests

No build, tests, analyzers or invariant gates were executed by this worker.
The supervisor must execute the resolved 0062.gates roster: build, elevation,
constructor-unit-tests and the nine s08 invariants. There is no claimed pass
or newly measured count. Commit readiness is not asserted before verification.

The existing seven-table contract fact is retained. The four new facts also
cover byte-for-byte preservation on mismatch, missing stamp table and row,
named memory mode, pooling override, file-handle release, post-disposal calls,
foreign keys disabled in the input string, callback exceptions and malformed
connection strings. File fixtures use fresh directories beneath
AppContext.BaseDirectory/LibraryDatabaseTests and dispose before cleanup.

This YAML copies the supervisor's last available constructor baseline from
checkpoint 60, not an authored or newly measured baseline:

```yaml
gates:
  constructor_unit_tests: 797
```

## Architecture

LibraryDatabase owns one open SqliteConnection. Each run acquires its lock,
constructs and disposes a LibraryDbContext over that connection, and captures
all exceptions as LibraryDatabaseExn. Disposal uses the same lock. A closed
connection is rejected before EF can reopen it. The two public entry methods
carry the required StorageBoundary annotation.

Initialization uses EnsureCreated and LibrarySchemaVersion.current = 1,
without migrations. Existing databases with user tables receive a read-only
stamp preflight before WAL. This deliberately precedes the slice's literal
initialization order because setting WAL changes the file header; the oracle
requires rejected files to remain untouched. Accepted/new databases then
follow WAL, EnsureCreated and stamp creation/validation. The implementation
log records this decision at the point it was made. No durable model shape,
schema table, package version, configuration option or version ring changed.

## Deferred

No slice implementation is deferred. Verification is assigned to the
supervisor by the worker protocol, not skipped as an implementation task.

## Gotchas

- Every context borrows the connection; only the handle owns its lifetime.
- Missing stamp tables and missing Id=1 rows both report no stamp; neither is repaired.
- Change LibrarySchemaVersion.current together with the table-contract listing when the model changes.
- The pre-existing dirty specs/0050/.manifest.state.json belongs to the supervisor and was left untouched.
- No gates, oracles, checkpoints, analyzer configuration, manual notes or version files were edited.

## Changelog

- 2026-11-06: Implemented the library database handle, typed schema checks and four declared storage facts; handed verification to the supervisor.
