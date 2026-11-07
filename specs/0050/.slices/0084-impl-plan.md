# Implementation plan — slice 0084

## Attempt 02 repair

Preserve the existing implementation and four declared fact identities. Extend the vacuumInto and restoreFrom facts with existing zero-byte target files, checking that refusal preserves the empty file and, for restore, the live rows and connection. Then resolve the full target path and refuse any existing file inside vacuumInto's locked storage boundary before creating the VACUUM command. Return LibraryDatabaseExn and retain parameter binding and caller-owned directories. Restore inherits the refusal through its existing call to vacuumInto.

Run focused local regression verification as required by the development instructions, separately from supervisor acceptance, and update the pinned log and state-of-the-world. Do not change the gate contract, immutable oracle, baseline, manifest state or other implementation files.

## Original implementation

Extend the existing `LibraryDatabase` handle in `LibraryDatabaseStore.fs` with an immutable `LibraryDatabaseOrigin`, parent-directory creation for file databases, parameterized `VACUUM INTO`, and validated online restore. Share the existing schema-stamp verification between opening and restoring; preserve schema rejection before WAL changes. Keep all IO inside the existing StorageBoundary exception mapping and all live-handle operations under its lock.

Add exactly the four named facts in `LibraryDatabaseTests.fs`, reusing its disposable test-output fixture and unconstrained catalogue-import rows. Cover new/existing origins, nested directory creation, stamped backups and collision preservation, and restore visibility on the same handle with a pre-restore backup and schema refusal before writes. Exercise missing stamps and IO refusal within those facts where appropriate.

Only the two authorized F# files and this slice's plan, log and state-of-the-world will change. The supervisor's existing manifest modification is unrelated and will be preserved. No version, package, stored-format, gate or analyzer changes are needed.

Risks are resource disposal on refusal, accidental writes before schema validation, and losing the live handle during restore. Use scoped connections and commands, a read-only unpooled backup connection, shared validation, and SQLite's online backup. Higher-priority development instructions require local verification: build the affected test project in Release/x64 and run the constructor tests, recording observations separately from the supervisor's acceptance gates. The arc-runner retains acceptance authority; no checkpoint baseline will be authored.
