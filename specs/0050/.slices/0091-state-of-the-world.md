# Where we are

Slice 0091 implements S31-FORK-PROVENANCE in S3.1. Fork ancestry is represented in the existing entry-file content and therefore travels through the shared database payload and kit snapshot representation. All resolved local checks passed; supervisor acceptance remains separate.

## What's working

- Add recursive fork provenance retaining the original entry id, version and provenance, including source attribution.
- Treat forks as user-managed entries and user categories, including forks of built-ins.
- Project forks to database provenance tag 5 with no catalogue affiliation.
- Extend the shared provenance schema with the recursive fork shape while keeping entry schema version 1.
- Extend the two existing facts for built-in ownership, source-pull forks and nested fork round-trips without adding or removing tests.

## Tests

- Release solution build passed in 14.75 seconds with zero warnings/errors after correcting the test fixture's CatalogueFiles import.
- Elevation passed over seven affected/dependent projects in 145 seconds: zero new/known findings, ELEV004 excluded.
- All 910 constructor cases passed in 17.038 seconds with zero failures/skips. The TRX includes both required named provenance facts as Passed. The count equals the latest constructor checkpoint; no tests were added or removed.
- All eleven s12 invariants passed using the encoded commands from 0091.gates.
- Source diff counts match with and without ignoring CR-at-EOL; git diff --check passed.

Local verification follows the higher-priority workspace requirement. Observed results do not replace supervisor acceptance or alter its checkpoints.

```yaml
gates:
  constructor_unit_tests: 910
```

Evidence is in specs/0050/.artifacts: 0091-build-repair.log, 0091-elevation.log and its SARIF folder, 0091-constructor-unit-tests.log/.trx, and 0091-invariants.log. The initial build diagnostic remains in 0091-build.log.

## Architecture

EntryProvenance and EntryForkInfo are mutually recursive domain types. EntryForkInfo carries elevated LibraryEntryId and VersionNumber values plus the original EntryProvenance. Ownership depends on the outer fork case, not its ancestry. The database keeps its existing payload/projection design, adding only the fifth tag and the no-catalogue branch.

The existing FSharp.SystemTextJson serializer writes the recursive record through its Case/Fields union encoding. The shared JSON schema reuses its guid, positiveVersion and provenance definitions. No converter, compatibility reader, migration, schema-version bump or duplicate representation is introduced.

## Deferred

None within the requested implementation. Supervisor acceptance and commit handling remain external.

## Gotchas

- CatalogueIds is nested in CatalogueFiles; the new built-in fixture requires that module import.
- The first build emitted two existing SYSLIB0051 warnings from vendored MathNet Exceptions.cs. The repaired incremental build reported zero warnings/errors.
- The pre-existing supervisor-owned specs/0050/.manifest.state.json change is untouched.
- No Git mutations, version bumps, other-repository changes, analyzer policy changes or gate/checkpoint edits were made.

## Changelog

- 2026-11-06: Added fork provenance and recursive schema support; build, elevation, all 910 constructor cases and eleven s12 invariants passed locally.
