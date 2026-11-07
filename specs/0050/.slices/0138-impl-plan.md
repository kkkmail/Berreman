# Implementation plan — 0138

Implement S42-SNAPSHOT-FILES within the five authorized source files. Reuse the library envelope's canonical filename components, Storage's `fileText` / `parseEntry`, and `writeNewBytesAtomically`. Extend `KitError`, `KitProxy`, its read-only view, and the shared in-memory folder with immutable snapshots ordered by filename. Keep the repaired lock guard unchanged.

First add the ten authorized discovered tests (three inherited contract facts run against both stores, one domain fact and three filesystem facts), and extend the existing error-rendering test. Then implement domain naming and proxy state, followed by filesystem serialization and validated listing. Cover duplicate writes with unchanged content, read-only behavior, catalogue byte equality, invalid or misnamed entries, and ignored non-entry files.

Files: `Kit.fs`, `KitFolders.fs`, `KitFolderStore.fs`, `KitTests.fs`, and `KitProxyTests.fs` in their declared projects. Risks are publication races, loss of canonical bytes, and weakening the shared lock guard; retain atomic non-overwriting publication and existing guard behavior. No durable format migration or compatibility reader is introduced.

The IMPLEMENT worker protocol assigns all gates to the supervisor. This round will not execute or report gate results, modify the test census or baselines, or mutate git state. Write the implementation log and state of the world at the task's exact paths.
