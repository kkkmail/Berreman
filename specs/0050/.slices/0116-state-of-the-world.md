# Where we are

Slice 0116 completes S33-ENTRY-EXPORT in S3.3. Single-entry export is implemented on the existing catalogue import context, with typed failures and the five declared regression cases. All resolved local checks passed; formal acceptance and checkpoint updates belong to the supervisor.

# What's working

- Added latest-envelope lookup and typed export failures with diagnostic rendering.
- Pin the selected version before writing its canonical entry file atomically.
- Expose the entry writer and optional export folder through the existing context.
- Verify export round-trip, drop reuse, pin ordering, retained material versions and identical built-in bytes over both stores.
- Verify missing entries, read/pin refusals and filesystem write failures without unintended writes.

# Tests

- Expected red: the pre-implementation build rejected the missing export API and error cases.
- Release solution build passed with zero warnings and errors (46.12 seconds).
- Elevation passed across seven projects with zero gated findings (151 seconds); ELEV004 excluded as prescribed.
- Full constructor suite passed 1000/1000, with no failures or skips (47.060 seconds).
- All eleven s14 invariants passed using the commands from `0116.gates`.
- TRX comparison with slice 0115 confirmed exactly the five declared additions, increasing 995 to 1000, and no removals.
- The four edited source files are LF-only, with identical normal/ignore-CR diff counts and no whitespace errors. No product edits followed verification.

```yaml
gates:
  constructor_unit_tests: 1000
```

This is the measured local result. No supervisor checkpoint or baseline registry was edited. Captured logs, SARIF reports, TRX and test identity comparison are under `specs/0050/.artifacts/0116-attempt-01-*`.

# Architecture

`CatalogueImportProxy.tryLatestEnvelope` composes the existing held-entry and held-envelope readers. `CatalogueImportContext.exportEntry` maps read and pin errors through `ExportLibraryRefusedErr`, rejects an absent envelope with `ExportedEntryNotHeldErr` naming its id, pins the envelope's version, then invokes the injected writer and returns that envelope.

The storage factory keeps its existing signature and writes `envelope.fileText` through `LibraryFileText.writeAtomically`. A storage failure becomes `ExportFileWriteErr` carrying the destination and the storage error's description. `exportFolderOpt` defaults to `None`; the configured folder and dialog wiring are outside this slice's effects. No serializer, durable format, database schema, context threading, overwrite policy or migration was added.

# Deferred

None within slice 0116.

# Gotchas

- A write failure leaves its version pinned; pinning is permanent and precedes filesystem writes.
- Test files live under fresh GUID folders in `AppContext.BaseDirectory/LibraryExportTests`, never Documents or committed data.
- The in-memory import map and material store are separate test-double state. The required regression exports version 1, edits through the material store and resolves versions 1 and 2 without changing that existing architecture.
- The session's verification instructions take precedence over the task file's act-only wording; the supervisor remains the formal gate authority.
- The pre-existing `.manifest.state.json` change belongs to the supervisor and was left untouched.

# Changelog

- 2026-11-07: Added single-entry export with permanent pinning, canonical atomic writes and typed failures; all resolved checks passed, including 1000 constructor tests and the five declared additions.
