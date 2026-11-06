# Step 0067 state of the world

## Where we are

Slice 0067 implements S22-ROW-OPERATIONS in spec 0050 S2.2. The shared SQLite
version-history operations, Built-in database fixture and four declared facts
are complete. The supervisor retains authority over the compiled acceptance
roster; this handoff does not assert that those gates passed.

## What's working

- Added validated exact-version, latest-pointer, ordered-history and kind reads.
- Added staged version upserts, relational reconciliation, latest selection and removal.
- Centralized keep, mutate, mint and new-entry staging with canonical sealing.
- Added a disposable Built-in SQLite fixture and four row-operation facts.

## Tests

Local development builds of OpticalConstructor.Tests and
OpticalConstructor.Ui.Tests succeeded. The latter proves that the shared
BuiltinCatalogue fixture compiles in its linked UI location. The initial build
emitted two existing SYSLIB0051 warnings from vendored MathNetNumerics, with no
warnings from our code. The UI build had zero warnings/errors.

All 18 selected tests passed: 15 LibraryDatabaseTests, including all four
declared additions, and 3 BuiltinCatalogueFixtureTests. Zero failures or skips.
They cover persistence across contexts, invalid-row rejection through all four
reads, explicit latest selection, sorted history, replacement relational keys,
repeated staging, no persistence without SaveChanges, lifecycle/pin/provenance
preservation, edit-version normalization, new entries and cascade deletion.

Higher-priority development instructions require these local checks despite the
worker's act-only protocol. They do not claim acceptance-gate results. The
supervisor must execute build, elevation, constructor-unit-tests and all ten
s09 invariants from 0067.gates. No acceptance failure is waived and commit
readiness is not asserted before that verification.

The following is the inherited supervisor baseline from checkpoint 65, not a
new measurement or worker-authored baseline:

```yaml
gates:
  constructor_unit_tests: 807
```

## Architecture

Nine StorageBoundary-marked LibraryDbContext extensions reuse toStoredVersion,
versionRow, facetRows, sampleMaterialRows and canonical sealing. Database and
EF exceptions remain caught by LibraryDatabase.run. Every staging operation
leaves SaveChanges to its caller, so successful operations commit once.

Upserts update every mapped version column through EF CurrentValues. Facet and
sample-material reconciliation preserves shared tracked keys, deletes obsolete
keys and restores or adds requested keys, including repeated stages within a
single context. Removal deletes the latest pointer and all owned versions;
the existing schema cascades their facet, sample-material and pulled-source rows.

KeepCurrent stages nothing. MutateInPlace normalizes the incoming version to the
latest version and preserves its state. MintNextVersion delegates to new-entry
staging at latest.next, active and unpinned, with an updated latest pointer.
The fixture stages the complete catalogue in one run, saves once and disposes
its handle if initialization fails.

No durable shape changes: the database schema, serialized format and proxy
contracts are unchanged. No migration, compatibility path, cache or retry was
introduced.

## Deferred

No implementation requirement is deferred. Supervisor acceptance execution
remains pending under the worker protocol.

## Gotchas

- Use MaterialLibrary.VersionNumber; Softellect.Sys.Primitives also defines a
  VersionNumber with a different representation.
- The latest row has a restrictive foreign key; it is removed before deleting
  the owning entry's versions. Sample material references do not constrain the
  referenced material version, only the owning sample version.
- Staging helpers must run inside LibraryDatabase.run and never save themselves.
- The three edited sources use LF and have no whitespace churn.
- Persistent local logs/TRX are under specs/0050/.artifacts/0067-local-*.
- The supervisor's pre-existing .manifest.state.json edit was left untouched;
  no git mutation, manual-note edit or analyzer configuration change was made.

## Changelog

- 2026-11-06: Implemented shared library database row operations and the Built-in
  fixture; added four facts; both affected test projects compiled and 18 selected
  tests passed locally; handed acceptance execution to the supervisor.
