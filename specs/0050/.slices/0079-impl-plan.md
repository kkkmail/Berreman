# Implementation plan — 0079

Implement the SQLite preset store over the existing LibraryDatabase entry-version operations, and compose the database live Library through the existing liveLibrary function. Preserve active-only offerings, lifecycle-independent resolution, provenance-derived protection, deterministic name/id ordering, and typed failures. Reuse the existing content builders/decoders, duplicate-id error, and By kind tree builder.

Add PresetDatabaseStore.fs immediately after SourceDatabaseStore.fs in the Storage project. Update only the LibraryEntryStore and liveLibrary documentation in ElementId.fs. Extend LibraryProxyTests.fs with LibraryProxySqliteTests inheriting the ten shared contract facts and adding the four specified database cases; track and dispose all databases opened by the fixture.

Risks are accidental inclusion of samples or user sources in presets, lost decoder/database errors, duplicate-id writes, and incorrect inactive-entry resolution. Keep each preset operation inside one LibraryDatabase.run and each successful write to one SaveChanges call. No product composition root, durable schema, or existing store behavior changes.

The IMPLEMENT worker protocol assigns acceptance checks to the supervisor. Higher-priority development instructions require a local development build and relevant tests; record these separately without claiming acceptance gate results. The supervisor retains the build, elevation, constructor suite, and eleven section invariants. Do not alter its existing manifest state or author measured baseline counts.
