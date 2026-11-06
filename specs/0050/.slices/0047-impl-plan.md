# Step 0047 implementation plan

Derive the Library's By kind representation with a pure `LibraryTree.byKind`
member in `ElementId.fs`. Preserve the prescribed kind and facet order, omit
empty groups, and retain entry order within leaves. Reuse that derivation in
the mutable library-entry store and in `liveLibrary`, collecting the five
offered kinds through its existing listing function and propagating its first
error. Remove the seeded-tree override from `CatalogueImport.fs`.

Add exactly the three declared facts to `LibraryProxyTests.fs` and the declared
Select-bay fact to `LibraryControlsTests.fs`. Use the existing BuiltinCatalogue
fixture and fresh stores. Cover the explicit expected tree, the empty store
and insertion, live membership without duplicate ids, and a newly saved mirror
under its geometry group in the existing pure UI projection.

The main risks are changing listing semantics, duplicating compound polarizers
offered for both kinds, and disturbing leaf order. Keep `liveLibrary`'s public
shape and the UI consumer unchanged, and retain the self-seeding helper and
`seedTrees` as directed. No durable format changes are needed.

The IMPLEMENT worker protocol assigns all gate execution to the supervisor.
Write the implementation log and state-of-the-world at the supplied paths;
do not run gates or claim their results. Preserve the pre-existing modified
manifest state and leave versioning, gate definitions and external repos alone.

## Attempt 02 repair

The supervisor reported FS0491 at the new UI fact's direct access to the
internal `libraryState` member. Repair only `LibraryControlsTests.fs`: render
the public `mainView` in the existing headless harness with a Sample selected
and quick-pick enabled. Read the Library tree by its existing id and assert
that the saved mirror's identified leaf is within the Mirrors group, with the
expected caption and group/leaf indentation. Keep the test identity and all
production interfaces unchanged. Record the repair in the log and handoff;
gate execution remains the supervisor's responsibility.
