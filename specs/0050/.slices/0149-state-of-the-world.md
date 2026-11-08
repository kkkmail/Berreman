# Where we are

Step 0149 implements S43-RECORD in section S4.3. KitLibraryContext now records
experiments and named scenes through the content writers from step 0148 after
snapshotting their library closures. Both authorized source files and all four
declared test additions are complete. Attempt 02 repairs the versioned-library
fixture in the two successful-recording facts after the supplied scene failure.
The supervisor owns gate execution and post-repair verification.

# What's working

- Added recordExperiment and recordScene with snapshots and pins before content writes.
- Reused the existing reference collectors, closure traversal and typed file writers.
- Preserved snapshot errors and wrapped content-write errors in KitLibraryKitErr.
- Added the four specified facts over the Built-in library and in-memory kit fixture.
- Repaired version-selection fixtures to keep imported versions and pins in one in-memory database.
- Covered unresolved references and read-only refusal for both recording operations.

# Tests

This worker executed no build, test, analyzer or invariant gate, as required by
IMPLEMENT Invariant 6. The supplied retry diagnostic reports a pre-repair run
with 1151 constructor cases: 1150 succeeded, one failed and none skipped. The
scene fact could not pin sample version 2 because its fixture added that version
only to the import map, not to the sample store used for pinning. Both
successful-recording facts now use the existing in-memory database import proxy,
which stores envelopes and pin states together. No passing post-repair result is
claimed. The supervisor must run the unchanged 0149.gates roster: build,
elevation, all-discovered constructor-unit-tests and the eleven s17 invariants.

The four authorized facts were added without changing existing identities or
assertions. At each content-write boundary, the successful-recording tests
assert the exact closure, pin list and pin/write operation sequence. An
experiment still records sample version 1 when the library's latest is version
2; the scene records version 2 through its value-id binding. Both round-trip
their content, and repeated recording adds no snapshots or pins. Writer errors
retain snapshots for a successful retry. Missing references prevent content
writes, while already completed snapshots survive a later missing reference.
Read-only coverage includes both populated and empty reference sets.

Checkpoint 147 supplies the inherited constructor baseline below. The supplied
failed run's total of 1151 matches the four authorized additions but does not
establish a new successful baseline. Only the supervisor publishes the next
measured checkpoint.

```yaml
gates:
  constructor_unit_tests: 1147
```

# Architecture

KitContentFiles.fs extends KitLibraryContext with exactly recordExperiment and
recordScene. Each calls snapshotReferences on the content's libraryReferences,
then uses Result.bind to call its existing KitProxy writer and maps KitError to
KitLibraryKitErr. The existing traversal owns resolution, closure, pinning,
deduplication and read-only refusal. No transaction, rollback, proxy field,
format, cache or migration was introduced. Deletion remains outside this slice.

Test data uses Domain scene and experiment records directly, sharing the
existing initialized in-memory kit fixture and pin recorder. The two
version-selection cases reuse BuiltinCatalogue.freshLibraryDatabase and
CatalogueImportProxy.createInDatabase; each fact disposes its own in-memory
database. Version insertion and pinning therefore address the same store.
All existing assertions and test identities are preserved. No UI initializer,
scratch-kit construction contract, shared import implementation or filesystem
fixture was changed.

# Deferred

No implementation requirement is deferred. Verification is pending with the
supervisor under the worker protocol; commit readiness is not claimed without
those gate results.

# Gotchas

- A missing first reference leaves paths unchanged. A failure after a successful
  snapshot retains that snapshot but never writes the experiment or scene file.
- Exact experiment sample versions and latest scene bindings intentionally
  differ; tests hold a newer library sample to distinguish those behaviors.
- Product diff numstats match with and without ignoring CR at line ends. All
  newly written source and worker-document text uses LF.
- The supervisor-owned manifest-state and expected-red modifications were preserved. No gate,
  oracle, census, checkpoint, analyzer configuration, version ring, notebook,
  sibling repository or git index was changed by this worker.

# Changelog

- 2026-11-07: Added snapshot-first experiment and scene recording and the four
  authorized regression facts; completed the worker handoff documents.
- 2026-11-07 (attempt 02): Repaired both version-selection fixtures to use the
  existing in-memory database library; preserved all assertions and handed
  post-repair verification back to the supervisor.
