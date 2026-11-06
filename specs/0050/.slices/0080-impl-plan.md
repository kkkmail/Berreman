# Implementation plan - 0080

Extract the existing four save adapters and their error-reason matches from CatalogueImportProxy.createInMemory into the explicitly typed static member CatalogueImportProxy.ofStores. Keep import ordering and all store contracts unchanged. Rebuild the in-memory proxy through this shared member; no production database composition or durable shape changes belong to this slice.

Add exactly the specified fact to LibraryDatabaseTests. Open and dispose a fresh SQLite :memory: library, compose its four database stores through ofStores, and import the shipped catalogue through importFrom. Reuse the catalogue decoding helpers, compare all six kinds with their decoded values using F# equality, check the latest row identities and version-one counts for every kind, and require ImportRejectedErr on re-import.

Product/test edits are limited to OpticalConstructor.Domain/CatalogueImport.fs and OpticalConstructor.Tests/LibraryDatabaseTests.fs. Preserve LF endings and leave the pre-existing manifest-state modification alone. The main risks are F# extension-module visibility and inadvertently testing the pre-populated fixture instead of an empty database.

The supervisor owns the build, elevation, constructor-unit-tests and eleven s10 acceptance invariants in 0080.gates. Higher-priority development instructions require local verification: compile the affected test project and run the constructor suite, recording those observations separately from acceptance gates. Copy only an existing supervisor baseline into the handoff; never author a checkpoint or claim an acceptance result.
