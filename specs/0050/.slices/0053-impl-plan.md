# Step 0053 implementation plan

Re-point the four authorized constructor test files to the existing
BuiltinCatalogue fixture, as required by spec 0050 S1.7 and 001 §B. Preserve
every fact name, assertion, numeric expectation, test catalogue id and version.
Use known ids for individual entries and fixture lists for complete corpora.
Keep synthetic inputs and the existing serialization and folder operations.

Files to modify:

- Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryFacetsTests.fs
- Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryEntryFilesTests.fs
- Berreman/OpticalConstructor/OpticalConstructor.Tests/EntryFileJsonTests.fs
- Berreman/OpticalConstructor/OpticalConstructor.Tests/CatalogueFolderTests.fs

Update seed-related comments to name fixture members and remove imports made
unused by the substitutions. No product file, fixture member, fact, project
entry or durable shape changes. Review the source diff and LF endings before
writing the prescribed implementation log and state of the world.

The main risks are selecting a different entry due to catalogue row order and
changing serialization coverage while replacing seed lists. Preserve existing
explicit ids, select the formerly first polarizer by its known id, and combine
all fixture samples and presets where the old library corpus was used.

Under implement_worker.system-md and Invariant 6, the supervisor runs build,
elevation, constructor-unit-tests and the seven s07 invariants after this
worker exits. This worker runs no gates and claims no measured result.
