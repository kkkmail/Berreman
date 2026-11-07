# Implementation plan — slice 0086

Implement `STORE_BigBoy_0003` with a Storage adapter over the existing `LibraryDatabase.vacuumInto` and `restoreFrom` operations. Inject `LibraryBackupTime`, generate collision-free timestamped names, list only canonical backup names in descending timestamp/sequence order, and translate database and directory failures to the declared backup errors. Keep schema validation and the online restore in the existing database seam.

Extend `LibraryBackupProxyTests.fs` with the concrete file-backed contract class and its five prescribed facts. Import the built-in catalogue through the existing database stores; retain and dispose every opened database. Cover snapshot isolation, live-store restoration and the safety copy, foreign-schema refusal, numeric collision ordering/filtering, and disposed-database errors.

Add the Storage compile entry immediately after `PresetDatabaseStore.fs`, document both proxy constructors, and mark the current arc's existing contract registry entry implemented at step 86. Write the pinned implementation log and state-of-the-world without changing supervisor state, gates, acceptance oracle or checkpoint baselines.

Risks: lexical filename ordering differs from numeric suffix ordering; malformed names must not enter the listing; schema rejection must happen before the safety backup; directory IO failures must remain typed. Local build/test verification follows the coding instructions; the supervisor retains formal gate authority and checkpoint ownership.
