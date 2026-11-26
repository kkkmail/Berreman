# Implementation plan — slice 0087

Implement `LibraryDatabase.recreate` in `LibraryBackupStore.fs`, reusing the
existing timestamp and collision-numbering rule by moving its name selection
onto the private backup metadata type. Parse the connection string at the
Storage boundary; memory and missing-file paths delegate to `openOrCreate`.
For an existing file, copy without overwrite before deleting the database and
SQLite sidecars, then delegate creation and stamping to `openOrCreate`.

Add the two prescribed facts to `LibraryDatabaseTests.fs`, using its existing
output-directory fixture and a fixed clock. Cover preservation of schema 2 and
stored content in the backup, an empty schema-1 replacement, sidecar cleanup,
copy refusal without deletion, collision numbering, missing-file creation and
both in-memory connection-string forms. Keep the fact-count increase at two.

The main hazards are deleting before a successful copy, duplicating filename
selection, retaining database handles during recreation, and mistaking the new
library's WAL files for old sidecars. Dispose every handle before recreation
and before checking final sidecar absence. Preserve the supervisor's existing
manifest-state edit, leave version rings and other repositories untouched,
and write only the pinned round documents and authorized source files.

## Attempt 02 repair

Extend the declared file-recreation fact with the existing blocked-parent
fixture pattern, asserting `FileIoExn`, preserved obstruction content and no
backup folder. Normalize only `LibraryDatabaseExn` values carrying filesystem
exceptions (`IOException` or `UnauthorizedAccessException`) at `recreate`'s
delegation to `openOrCreate`. Keep other typed errors and `openOrCreate` itself
unchanged. Preserve the two fact identities and the count increase of two.

Record the regression failure before the repair, then verify the Release/x64
build, constructor suite, elevation and resolved section invariants locally as
required by the higher-priority coding instructions. Formal acceptance and
checkpoint updates remain owned by the supervisor.
