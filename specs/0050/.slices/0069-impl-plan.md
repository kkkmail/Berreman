# Step 0069 implementation plan

Implement STORE_XDUO_0001's SQLite material adapter in MaterialDatabaseStore.fs,
compiled immediately after LibraryDatabaseStore.fs. Reuse the entry builders,
decoders, database staging operations, version payload, versioning decision and
material refusal members. Each operation uses one database.run; successful writes
finish with one SaveChanges. Preserve latest-material identity, lifecycle,
provenance, pinning and sample-reference-before-pin removal refusal ordering.

Add MaterialStoreUnavailableErr and cover it in all exhaustive consumers,
including the BuiltinCatalogue test fixture. Add MaterialProxySqliteTests in the
existing test file, inherit all 22 contract facts, and add the two specified
provenance/disposed-database facts. Track and dispose every fixture database.

The freshProxy fixture must match the existing reference-free in-memory factory:
remove sample entries from its fresh catalogue database. composedProxies retains
the samples and uses live database reads/removals. This preserves the existing
contract assertions, including the pin-only removal refusal for glass152.

Record the inherited contract's implementation in this arc's lifecycle registry
without changing its identity or earlier arc history. Write the required handoff
documents. Acceptance-gate execution remains with the supervisor; local
development verification required by the coding instructions is recorded
separately. No production composition, schema, migration, version bump or analyzer
configuration changes are planned.
