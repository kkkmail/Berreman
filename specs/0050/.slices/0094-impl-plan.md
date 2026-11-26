# Implementation plan

Reshape CatalogueImport around whole envelopes and explicit confirmation. Reuse the decision table, entry decoders, reference projection and Storage sealing member. Keep the in-memory adapter's version history in its closure, with first-entry writes delegated to the four existing stores. Validate all choices before any writes and apply confirmed choices in dependency order.

Update the recording and in-memory import tests first, then implement the Domain context and Storage factory. Adapt BuiltinCatalogue, diagnostic DefaultStores, database/backup fixtures and UI fixtures to explicitly apply decisions. Production AppContext must retain decisions and the matching context for confirmation without applying them.

Files are the slice's declared Domain, Storage, Ui and test files, plus LibraryBackupProxyTests.fs: discovery found another caller of the removed proxy import API there. This necessary fixture repair preserves its assertions and count. Required worker outputs remain at the supplied paths.

Risks are first-error/partial-write behavior, KeepLatestVersion metadata, retirement order, and accidental automatic startup writes. Verify using the resolved build, elevation, constructor and UI selections plus the eleven invariants; local verification follows the higher-priority workspace instruction, with supervisor acceptance remaining external. Do not mutate Git, version rings, analyzer configuration or other repositories.
