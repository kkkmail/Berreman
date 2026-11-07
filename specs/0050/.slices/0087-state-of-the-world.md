# Where we are

Slice 0087 implements the S2.4 schema-mismatch remedy: preserve the closed
library file in Backups, then recreate an empty stamped library. Both required
test identities and the production extension are present. Attempt 02 repairs
the delegated filesystem-error contract identified in review. Implementation
and all local verification are complete; formal acceptance belongs to the
supervisor.

## What's working

- Add `LibraryDatabase.recreate` with copy-before-delete ordering and typed IO failures.
- Share timestamped collision numbering between recreation and the existing backup proxy.
- Remove old SQLite sidecars and create a fresh stamped database through `openOrCreate`.
- Add two recreation facts covering file preservation, copy refusal, collisions, missing files and memory modes.
- Normalize delegated filesystem failures to `FileIoExn` without changing `openOrCreate` or unrelated errors.
- Cover blocked-parent failure and preservation within the existing file-recreation fact.

## Tests

Attempt 02 local verification:

- The new blocked-parent regression failed before the production repair,
  specifically because the delegated filesystem error was not `FileIoExn`.
- Release/x64 solution build: zero warnings/errors, 7.67 seconds.
- Whole constructor suite: 910 passed, zero failures/skips, 18.005 seconds.
  Both exact declared recreation facts and the existing `openOrCreate`
  blocked-parent contract passed; the prior count still increases by two.
- Elevation: zero findings across Storage and four dependent projects in
  140 seconds, with ELEV004 excluded as prescribed.
- All eleven s11 invariant commands from the resolved `0087.gates` snapshot passed.
- Owned source diff: no whitespace errors or CRLF churn; all owned files use LF.

Current logs and TRX/SARIF reports are under
`specs/0050/.artifacts/0087-attempt-02-*`; `0087-local-*` captures are historical
attempt-01 evidence.
Local verification follows the higher-priority coding instructions. The
supervisor retains formal gate and checkpoint ownership; the YAML below
preserves the existing checkpoint 85 count, not a new baseline.

```yaml
gates:
  constructor_unit_tests: 908
```

## Architecture

`LibraryDatabase.recreate` is a Storage boundary over the connection string,
backups folder and injected clock. Existing-file recreation copies without
overwrite before deleting the source and its WAL, SHM and journal files.
Memory and missing-file cases delegate creation to `openOrCreate`, preserving
its schema creation, stamping and lifetime contract. Returned filesystem
exceptions (`IOException` and `UnauthorizedAccessException`) become `FileIoExn`
at the recreation boundary; other typed errors pass through unchanged.
`openOrCreate` itself is unchanged. Filename selection moves
to the existing private backup metadata type and remains shared with the
backup proxy. No durable format changes or migrations are introduced.

## Deferred

No implementation requirement is deferred. Formal acceptance is the
supervisor's next stage.

## Gotchas

- Call recreation only when no handle holds the database, as after an
  `openOrCreate` schema mismatch. The existing mismatch path disposes its handle.
- The replacement uses WAL too; final sidecar absence is asserted after its
  handle is disposed. Invalid old sidecars exercise their removal before open.
- A locked source makes `File.Copy` fail; the test checks that the database
  bytes and all three old sidecars remain intact.
- A blocked parent fails during delegated directory creation. The regression
  asserts `FileIoExn`, unchanged obstruction content, no database and no backup
  folder. An outer catch alone cannot normalize an exception already in `Result`.
- Tests reuse the step-0084 fixture below the test output directory, and a
  plain SQLite connection inspects the incompatible backup's stamp and entry.
- The pre-existing supervisor manifest-state edit remains untouched.

## Changelog

- 2026-11-06: Added library recreation and shared backup name selection; the Release build, all 910 constructor tests, five-project elevation analysis and eleven invariants passed locally.
- 2026-11-06 (attempt 02): Normalized delegated filesystem errors and added blocked-parent coverage within the declared fact; regression reproduced before repair, then build, all 910 constructor tests, elevation and eleven invariants passed.
