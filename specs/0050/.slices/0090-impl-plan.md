# Implementation plan

Wire the existing LibraryBackupProxy into AppContext, LibraryWindowContext and every Library-window launch path. Add the specified semantic messages, mutually exclusive restore confirmation state, stable control IDs, status/error projection, and Restore/Cancel actions using existing view helpers.

Update the authorized test fixtures for the added proxy argument. Add the six specified model/ID facts and the real database-backed headless button acceptance test to LibraryWindowTests. Reuse the existing backup implementation, test app scope and semantic input helpers.

Files: the thirteen product/test files listed by slice 0090, plus these required slice outputs. Risks are stale library selections after restore, overlapping confirmations, missed launch arguments, and restoring through the live database proxies. No durable format changes or version bumps are needed.

The task's WIRE_UI protocol reserves acceptance for the supervisor. Higher-priority workspace instructions require local validation, so run the resolved build, elevation, UI suites and section invariants, capturing their evidence under the supplied artifacts folder. Report local observations separately from supervisor acceptance; do not edit measured checkpoints.
