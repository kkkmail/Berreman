# Implementation plan — slice 0085

Declare `STORE_BigBoy_0003` in Domain's new `LibraryBackup.fs`, compiled directly after `WorkbenchSettings.fs`. Add the elevated backup name, documented errors with condition-and-reason descriptions, and the reference-equality functional proxy. Its in-memory double holds only minted names in creation order and shares one mint operation between explicit backups and successful restores. Unknown names are refused before mutation.

Add `LibraryBackupProxyContract` with the four specified facts and `LibraryBackupProxyTests` supplying the in-memory double. Compile the file directly after `LibraryDatabaseTests.fs`. The facts use returned names so the database adapter can inherit the same contract without assuming the double's naming format.

Only the four authorized source/project files and the pinned worker outputs need changes. The contract registry already records declaring step 85 with lifecycle `declared`; leave it unchanged. No database content, schema, storage implementation or UI wiring belongs to this slice.

Risks: preserve newest-first listing, refuse unknown names without minting, and distinguish the safety backup from the restored name. Interpret the spec's phrase "third name" as a fresh distinct safety-backup name: the same paragraph explicitly requires one initial backup and a final two-name list. Preserve that exact sequence. The supervisor owns acceptance gates and checkpoint baselines; higher-priority development instructions also require local build/test verification, recorded separately from acceptance.

## Attempt 02

Extend the existing unknown-backup fact to bind the returned `UnknownLibraryBackupErr` and assert its `describe` text on the expected branch. Require the unlisted-backup condition, the full reason and the refused name, retaining the existing reason and unchanged-list assertions. Preserve all four fact identities, the production proxy, project compile entries and compiled gate roster. Record local build and focused contract-test verification separately from supervisor acceptance.
