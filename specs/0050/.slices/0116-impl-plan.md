# Implementation plan — 0116

Implement S33-ENTRY-EXPORT on the existing catalogue import context. Add the typed export error and latest-envelope reader, then compose read → pin → atomic entry-file write. Reuse the canonical envelope serializer and existing atomic writer; default the optional export folder to None. No durable format or schema changes are needed.

Modify only CatalogueImport.fs, CatalogueImportStore.fs, CatalogueImportInMemoryTests.fs and CatalogueImportProxyTests.fs. Add the two specified inherited contract facts (in-memory and SQLite) and the export-error rendering fact. Cover the round-trip/drop reuse, pin-before-write ordering, retained material versions, canonical Silicon bytes and refusal paths.

Run the resolved Release build, elevation, constructor suite and eleven s14 invariants under the session's verification instructions. Store captured output in the supplied arc artifacts folder. Leave formal acceptance and checkpoint maintenance to the supervisor; preserve its pre-existing manifest-state edit.

Risks: adding context fields must preserve all record construction sites; an export failure after pinning intentionally leaves the version pinned; test output must remain beneath the test assembly's output folder. No overwrite policy, configured folder wiring or UI is added in this slice.
