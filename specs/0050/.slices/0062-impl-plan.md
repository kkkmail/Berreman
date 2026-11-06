# Step 0062 implementation plan

Implement S21-OPEN-STAMP using the existing seven-table LibraryDbContext,
Softellect.Sys.Primitives.ConnectionString and StorageError channel. Add the
model version and mismatch payload in Errors.fs, then compile a sealed
LibraryDatabase handle immediately after CatalogueFolderStore.fs. The handle
owns one unpooled connection, serializes context use and disposal, enables
foreign keys and file WAL, and creates or validates the schema stamp.

Extend LibraryDatabaseTests.fs with the four declared facts before implementing
the seam. Cover memory lifetime, file reopen and WAL, incompatible/missing
stamps without schema or row repair, and recovery after an EF foreign-key
failure. Keep file fixtures in fresh folders under the test assembly output.
Preserve the existing table/column contract fact and project references.

Risks are connection ownership on unsuccessful open, EF reopening a disposed
connection, and accidentally repairing an unrelated database. Handle those
explicitly at the storage seam. No migration, version bump, configuration knob,
package change or unrelated product edit is planned.

The IMPLEMENT worker's act-only protocol assigns all gates to the supervisor.
Do not run build, tests, analyzers or invariant gates here. Write the required
implementation log and state-of-world handoff at the supplied paths; retain
the supervisor's existing measured baseline without inventing a new count.
