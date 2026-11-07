# Where we are

Slice 0117 implements S33-CATALOGUE-EXPORT in spec 0050 S3.3. Catalogue selection and whole-user-library export are implemented with the specified target, errors, injected identity provider and declared tests. The IMPLEMENT worker contract reserves execution and acceptance to the supervisor; this round makes no claim that gates passed.

# What's working

- Added catalogue export targets and diagnostic refusals for invalid catalogues, occupied folders and empty exports.
- Gather latest selected envelopes and recursively include referenced user-managed entries.
- Validate per-kind file names and refuse existing manifests before pinning every exported version and writing.
- Export all user entries, including inactive entries, using the existing catalogue writer.
- Added fourteen declared test cases across the shared store contract, SQLite pin rows and folder naming; extended export error rendering.

# Tests

No build, tests, analyzers or invariants were executed, as required by IMPLEMENT worker invariant 6. The supervisor's resolved roster is `build`, `elevation`, `constructor-unit-tests` and the eleven `invariant-s14-*` gates in `0117.gates`. Test discovery should increase by 14: six shared facts inherited by both stores, one SQLite-specific fact and one folder-target fact. Actual counts and results remain unmeasured; no baseline or checkpoint was changed.

```yaml
gates: {}
```

The new facts cover dependency closure and envelope round-trip, explicit reuse confirmation without library or log mutation, duplicate selections, omission of referenced built-ins, pin-before-write ordering, retained material versions, pinned sample removal refusal, duplicate-name refusal before target inspection, valid/malformed occupied manifests, inactive user entries, empty exports and all six entry kinds' SQLite pin rows.

# Architecture

`CatalogueImportContext.exportEntries` composes `tryLatestEnvelope`, `referencedEntryIds`, `tryHeldEntry`, `CatalogueData.tryCreate`, `readManifest`, `pinEntryVersion` and `writeCatalogue`. An immutable map deduplicates exported identities and prevents revisiting references. Explicit built-in selections remain exportable; referenced built-ins do not join the selection. All library failures use `ExportLibraryRefusedErr`; catalogue validation and write failures use `ExportCatalogueRefusedErr`.

`exportUserLibrary` selects every id from `heldUserEntries` and delegates to `exportEntries`. Storage supplies a new catalogue GUID and derives the target name from its folder's last segment, preserving the given folder. The factory signature, serialization, database schema, catalogue format, atomic writer and import confirmation behavior are unchanged. No durable migration or compatibility path is introduced.

# Deferred

No implementation requirement was deferred. Required gate execution belongs to the supervisor and remains pending.

# Gotchas

- A valid or rejected manifest refuses export before pinning. Other manifest read errors proceed to the existing writer, as the slice specifies.
- A catalogue write failure retains the pins already made; pinning is permanent. The existing writer refuses unlisted entry files.
- Every new export destination is a fresh GUID folder under `AppContext.BaseDirectory/LibraryExportTests`, created only by the writer during the tests.
- The pre-existing `.manifest.state.json` modification belongs to the supervisor and was left untouched. No git mutation or version bump was performed.
- The YAML block intentionally contains no worker-authored baseline. Measured counts belong to the supervisor's checkpoint record.

# Changelog

- 2026-11-07: Implemented catalogue and whole-user-library export with dependency closure, validation-before-pin ordering, occupied-folder protection and the fourteen declared test cases; handed off for supervisor verification.
