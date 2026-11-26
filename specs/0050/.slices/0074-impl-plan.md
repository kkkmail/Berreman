# Step 0074 implementation plan

Implement the SQLite SampleProxy adapter over LibraryDatabase and its existing
entry/version staging operations. Reuse SampleQuery.selectFrom, sample
validation/versionPayload, decideVersioning, typed refusal members, and the
existing material-reference projection. Preserve provenance on edits and use
UserProvenance for new entries; keep application composition unchanged.

Add SampleStoreUnavailableErr and update exhaustive consumers. Add the store
after CategoryDatabaseStore in the Storage project. Extend SampleProxyTests.fs
with a disposable SQLite subclass inheriting the 21 shared facts and the two
specified database facts. Update STORE_XDUO_0002 in this arc's lifecycle registry,
preserving its inherited declaration metadata.

Risks are version-specific material keys, cross-kind GUID collisions, pin
preservation, and exception conversion. Read the existing materials adapter and
database operations before implementation. The worker role assigns acceptance
gates to the supervisor; no gate results or new baselines will be asserted.

The existing BuiltinCatalogue.sampleStore fixture also exhaustively matches
SampleError. Extend that match for the added case as a necessary fixture repair
under the executable contract's authorization, and record the extra file.
