# Step 0076 implementation plan

Implement the unversioned SourceProxy surface over the existing versioned
LibraryDatabase operations. Reuse source validation, LibraryEntryContent
builders/decoders, decideVersioning and the row staging helpers. Preserve stored
provenance, exclude protected sources, and check every version before removal.

Modify ElementId.fs to add the two typed database/pin errors and the shared
unknown-source constructor. Add SourceDatabaseStore.fs directly after
SampleDatabaseStore.fs in the Storage project. Extend SourceLibraryTests.fs
with SourceProxySqliteTests, inheriting the two existing contract facts and
adding the three required SQLite facts. No composition-root wiring, migration,
cache, version bump or changes outside these authorized source files.

Risks are accidental inclusion of catalogue sources, loss of provenance,
checking only the latest pin, and exceptions escaping a proxy field. Each field
will enter LibraryDatabase.run once, and writes will save once. Tests will use
fresh catalogue-loaded SQLite memory databases and dispose every opened handle.

Formal gate execution belongs to the supervisor under the IMPLEMENT worker
protocol. Required development verification is recorded separately, without
claiming any acceptance gate passed or changing supervisor baselines.
