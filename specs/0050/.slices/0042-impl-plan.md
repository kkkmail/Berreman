# Step 0042 implementation plan

Replace the parameterless scene's self-seeded stores with a fresh
`CatalogueImportProxy.createInMemory` bundle populated through
`CatalogueProxy.createBuiltIn` and `importFrom`. Keep all five `DefaultStoreSet`
fields and compose `liveLibrary` over the imported entries and samples plus a
fresh user-source store. Log an import failure with its description and retain
the successfully imported prefix.

Reuse `Library.createInMemory().libraryTrees` in the import bundle's read proxy
to preserve the existing By-kind Select tree. Its entry queries continue to
read the imported store; no grouping builder or ordering is introduced.

Add the specified `TableAndElementRotationTests` fact first, comparing the
default Main scene's materials, samples and categories by id with the shared
`BuiltinCatalogue` fixture, and resolving every fixture preset to itself.
Only the three authorized F# files change. No serialized shape or migration
changes. The main risks are catalogue load failures, retained grouping ids and
the existing UI consumers' assumptions about preset order.

The resolved acceptance roster is build, elevation, all constructor tests,
all UI smoke tests, the eleven selected UI classes, and the six s06
invariants. Formal acceptance belongs to the supervisor. Preserve its baseline
counts and pre-existing manifest-state modification.
