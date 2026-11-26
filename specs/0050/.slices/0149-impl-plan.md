# Implementation plan

Implement S43-RECORD in the two authorized source files. Add exactly the two
KitLibraryContext extension members in KitContentFiles.fs, composing the
existing reference collectors, snapshotReferences and typed content writers.
Snapshot failures stop before content writes; content errors become
KitLibraryKitErr, and snapshots already written remain available.

Add exactly the four declared facts in KitLibraryTests.fs. Reuse its Built-in
catalogue, pin recorder and initialized in-memory kit fixture. Cover complete
closures, pins before content IO, content round trips, unresolved references
and read-only refusal. Exercise failure propagation and retained snapshots
within these identities without adding tests to the census.

No formats, proxy fields, project entries or adjacent workflows change. Preserve
the supervisor's existing manifest-state modification. Write all three worker
documents at the supplied paths using LF. Under IMPLEMENT Invariant 6, leave
build, elevation, constructor tests and s17 invariants to the supervisor; report
no measured result or new baseline.

## Attempt 02 repair

The supplied constructor-test failure names sample version 2 during scene
recording. CatalogueImportProxy.createInMemory stores later envelopes only in
its import map, while sample pinning delegates to its separate SampleProxy,
which still holds version 1. The recording members correctly propagate that
refusal; the new test's version setup does not supply a pinnable version 2.

In the two authorized successful-recording facts, reuse
BuiltinCatalogue.freshLibraryDatabase and CatalogueImportProxy.createInDatabase
for the Built-in library. The database is in memory and is disposed by each
fact; the existing snapshotFixture still supplies the pin recorder and the
in-memory kit. This places imported envelopes and pins in the same version
store, preserves the version 1 versus version 2 assertions, and requires no
production change or new test identity. Preserve every existing assertion.

Update the log and state-of-the-world at their supplied paths. Leave gate
execution and the measured baseline to the supervisor under IMPLEMENT
Invariant 6; preserve the supervisor-owned manifest and expected-red changes.
