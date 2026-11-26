# Where we are

Step 0442 continues S13.1's online-source work (spec 0050 S13.1; 001 §C "What it holds: … pulled-source records (§D)",
§D "A pulled record becomes a user material version in the database, with provenance").

- The library database now holds a pulled-source row beside every version under `SourcePullProvenance`. The row is
  derived from the version's provenance, as its facet rows are derived from its envelope.
- Every path that stores a version keeps that row: a pull, a kit snapshot's import of a version pulled elsewhere, and
  the re-import of an exported one.
- The import seam answers the versions a source's records minted. A source update can then re-fetch exactly those.
- The database model and its schema stamp are unchanged. Step 60's `LibraryPulledSourceRow` already had the columns.

# What's working

- Add `CatalogueImport.PulledRecord` (a held version and the record it was pulled from) and its one ordering,
  `PulledRecord.inRecordOrder`.
- Add `CatalogueImportProxy.pulledRecordsOf : MaterialSourceName -> Result<PulledRecord list, CatalogueImportError>`,
  answered by the in-memory double and by `createInDatabase`.
- Add `LibraryEntryEnvelope.pulledSourceRows`. `stageVersion` keeps a version's `PulledSource` row exactly its
  envelope's: added, updated in place, or removed. A fork has none.
- Add the facts `the pulled records of a source are its held pulled versions, by record, entry and version` (over
  both stores) and `a pulled version's pulled-source row is stored beside its version row, and no other version has
  one`.

# Tests

These are development runs, evidence only; the supervisor's gates decide. Each used the gate's own command and
selection.

- **build:** exit 0 with 0 errors, 80 s against 600 s. The only warnings are the exempt NU1902 (12) and the vendored
  MathNet's pre-existing SYSLIB0051 (4).
- **elevation** (ELEV004 excluded, all 13 projects, no comparison base): `ELEV-GATE: OK - 0 new finding(s)`, 214 s
  against 600 s.
- **constructor-unit-tests** (`all_discovered`, the whole suite as the gate selects it): `Passed!`, 1765 of 1765,
  112 s against 180 s.
  - The count rises by 3, from 1762 (step 0441's run).
  - TRX: all three new facts passed, under `CatalogueImportInMemoryTests`, `CatalogueImportSqliteTests` and
    `LibraryDatabaseTests`.
- **kit-acceptance** over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Passed!`, 1418 s against 3600 s.
  - 14 succeeded and 1 skipped: AT-2's five-seed ensemble, which xUnit v3 marks Explicit (the gate's skip policy is
    `allow`).
  - `the built optics-mueller kit equals the committed kit file by file` and `the built absorbing-biaxial-noisy kit
    equals the committed kit file by file` passed. AT-1's and AT-2's expectations and tolerances are unchanged.
- **The twenty-one s43 invariants:** each decoded `git grep` finds nothing (exit 1 for all 21).
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  constructor_unit_tests: 1765
  kit_acceptance_tests: 14
```

# Architecture

- **One ordering, written once.** `PulledRecord.inRecordOrder` sorts in F#:
  - record text ordinally, then the entry GUID's text ordinally, then ascending version;
  - both stores call it, so neither SQLite's collation nor `Guid` comparison decides the order.
- **The row is derived, never written by hand.** `pulledSourceRows` is a pure member on the envelope, carrying
  step 62's `StorageBoundary` mark like `facetRows`. Every store path already goes through `stageVersion`, so no
  other call site changed.
- **Updated in place.** The `PulledSource` key is the version's own `(EntryId, Version)`:
  - a re-staged row (a pin, a retirement, an in-place edit) is found and updated with `SetValues`;
  - a Deleted one is restored first;
  - it is never removed and re-added, which EF's identity map would refuse.
- **`createInDatabase.pulledRecordsOf` is one `runImport`:**
  - it reads the source's rows and loads each version through `tryStoredVersion`;
  - a row whose version is missing, or whose provenance is not a pull of that source, is `LibraryRowInvalidErr`
    naming the entry and version (`LibraryVersionRef.describe`);
  - every `StorageError` becomes `ImportStoreUnavailableErr` through the existing `storageResult`.
- No cache, query option or other proxy field is added. `stageRemoval` is unchanged; its cascade removes the rows.
- R0001–R0005 are preserved. None of their files is touched.

# Deferred

Nothing.

# Gotchas

- **The contract fact goes beyond the brief in two places, and drops nothing:**
  - it retires entry C and asserts the same answers again, covering "whatever its lifecycle" and, on SQLite, the
    in-place re-stage of a pulled row;
  - A's two versions carry different record hashes and source versions, so the structural equality proves each
    record answers the `SourcePullInfo` it was put with.
- **The database fact pins the pulled version between its two assertions,** to prove a re-stage keeps exactly one
  row.
- **The GUIDs are chosen so A's text sorts before B's.** The answer B, A, A therefore proves the record's text is the
  first key.
- **The existing removal fact** adds a `PulledSource` row by hand to a built-in version after `stageVersion`. It
  still passes; a later `stageVersion` of such a version would now remove that row, as the rule requires.

# Changelog

- 2026-11-16, step 0442:
  - pulled-source rows are stored beside every pulled version;
  - `CatalogueImportProxy.pulledRecordsOf` answers a source's pulled versions over both stores;
  - added the two facts (three test identities).
