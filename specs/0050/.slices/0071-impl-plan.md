# Step 0071 implementation plan

Implement STORE_XDUO_0003 with a CategoryProxy.createInDatabase extension in
Storage, compiled immediately after MaterialDatabaseStore. Reuse LibraryDatabase.run,
the existing entry-version operations, category codecs, decideVersioning and the
four CategoryError refusal members. Keep each operation within one database run
and each successful write within one SaveChanges call. Preserve provenance during
updates and inspect latest materials, including inactive ones, before removal.

Add CategoryStoreUnavailableErr and handle it in the import diagnostic and UI
caption. Document both category store implementations. Extend CategoryProxyTests.fs
with CategoryProxySqliteTests, inheriting all eight existing contract facts and
adding the two specified provenance/failure facts. Each fact owns and disposes
its seeded in-memory SQLite database. Update only STORE_XDUO_0003 in the current
contract registry, preserving inherited declaration metadata.

Risks are cross-kind identity collisions, decoder failures, provenance changes,
and accidentally performing the reference lookup in a separate database run.
Keep the unchanged contract facts as the behavioral oracle. Acceptance gates and
checkpoint publication belong to the supervisor. Focused local development
build/tests required by the coding instructions are distinct from acceptance;
capture their output only in the supplied .artifacts folder.

Write the implementation log and state-of-the-world at the exact task paths.
Preserve the pre-existing .manifest.state.json change. No version bump, migration,
cache, category pin operation, analyzer configuration change or unrelated edit.
