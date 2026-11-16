# Where we are

Step 0441 opens S13.1's record-hash work (spec 0050 S13.1; 001 §B "Identity is GUID + version + hash", §C, §D).

- A version pulled from an online source now records which bytes of its record it was built from. Updating a source
  can then mint a new version only where a record's hash changed.
- The hash rides in the provenance. So the entry file, the library database's payload (the file text) and a kit's
  snapshot of a pulled version all carry it.
- The database's model and schema stamp are unchanged.

# What's working

- Add `recordHash : ContentHash` to `SourcePullInfo`, directly after `record`: the SHA-256 of the record's text when
  this version was pulled.
- Require `recordHash` (a 64-hex content hash) in the pulled-from-source provenance of `library-common.schema.json`.
  The entry schemas stay at version 1.
- Set the hash at every `SourcePullInfo` construction (`BuiltinCatalogue.licensedMaterialContent` and the two test
  literals), with no assertion changed.
- Add the fact `a pulled provenance keeps its record's hash through an entry file, and a file without it is
  refused`.

# Tests

These are development runs, evidence only; the supervisor's gates decide. Each used the gate's own command and
selection.

- **build:** exit 0 with 0 errors, 85 s against 600 s. The only warnings are the exempt NU1902 (12) and the vendored
  MathNet's pre-existing SYSLIB0051 (4).
- **elevation** (ELEV004 excluded, all 13 projects): `ELEV-GATE: OK - 0 new finding(s)`, 237 s against 600 s.
- **constructor-unit-tests** (`all_discovered`, the whole suite as the gate selects it): `Passed!`, 1762 of 1762,
  105 s against 180 s.
  - The count rises by 1, from 1761 (checkpoint 425's baseline).
  - The new fact passed (TRX, 0.27 s).
  - These unchanged facts passed: `every provenance round-trips through an entry file` and `a pulled entry and its
    forks carry its licence's attribution, and every other provenance none`.
- **The twenty-one s43 invariants:** each decoded `git grep` finds nothing (exit 1 for all 21).
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  constructor_unit_tests: 1762
```

# Architecture

- **`ContentHash` moved** within `LibraryEntryFiles.fs`, unchanged, to just above `SourcePullInfo`. The new field
  needs it, and it used to be declared further down the same file.
- **What stays the same:**
  - no provenance case or member changes;
  - `attributionOpt`, `protection` and `categoryOrigin` read no hash;
  - `LibraryDatabaseStore.fs` and `LibrarySchemaVersion.current` (1) are untouched.
- **Schema:** `recordHash` is a plain `$ref` to the file's existing `contentHash` definition, like the other `$ref`
  properties. The `Fields` description names the hash and cites S13.1.
- **The new fact:**
  - It reuses the file's existing `editedText` and `assertSchemaInvalid` helpers, so it sits after them.
  - Before removing the key, it asserts that the file's provenance holds the hash. The `SchemaValidationErr` it then
    checks therefore proves the key is required, not merely absent.
- R0001–R0005 are preserved. None of their files is touched.

# Deferred

Nothing.

# Gotchas

- **Requiring the key refuses no stored file.** No committed kit, catalogue or `data/` file holds a pulled provenance.
  `git grep` outside `specs/` finds one only in the domain, the schema, the database's case tag and the three test
  constructions.
- **A `SourcePullInfo` needs a `ContentHash`.** A future pull (the `MaterialSourceProxy` work of S13.1) must hash the
  record's text as its snapshot holds it, through `ContentHash.ofBytes`. There is no second hashing body.
- **`EntryFileJsonTests` now opens `Softellect.Sys.Primitives`**, for `FileByteContent`. It is opened before the
  Domain modules, as in `LibraryEntryFilesTests`, so a Domain name still wins any clash.

# Changelog

- 2026-11-16, step 0441:
  - `SourcePullInfo.recordHash` added, and required by the pulled-provenance schema;
  - every construction sets it;
  - added the round-trip-and-refusal fact.
