# Step 0067 implementation plan

Implement the shared LibraryDbContext read and staging operations in
LibraryDatabaseStore.fs, reusing row validation, canonical sealing, relational
projections and the existing database exception boundary. Reads select the
explicit latest pointer or version history. Staging never saves; callers commit
once after successful staging. Reconcile relational keys without replacing an
already tracked entity with another instance of the same key.

Add freshLibraryDatabase to BuiltinCatalogue.fs, importing all catalogue
envelopes in one run and disposing the handle on initialization failure.
Add the four exact LibraryDatabaseTests facts with persisted readback across
contexts, state/provenance checks, relational replacement and cascade removal.
Keep changes within the three authorized source files and worker output paths.

Risks are EF identity tracking, latest-pointer foreign-key ordering, preserving
historical versions, and accidentally saving inside staging. Exercise those
through the declared facts. The development environment requires scoped local
compilation/tests; these do not replace or claim the supervisor's acceptance
gates. Record that procedural conflict and all verification evidence in the log.

No schema, serialized format, package version, analyzer configuration or public
proxy contract changes are required. Preserve the supervisor's existing edit to
.manifest.state.json.
