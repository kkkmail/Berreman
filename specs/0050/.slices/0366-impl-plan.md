# Step 0366 — impl plan

## Approach

The five study messages become one semantic union over the pure updates steps 0358–0364 built, applied to what the
logic receives: a `StudyContext` of the kit's lists and the library's material resolution.

`Studies.fs`, last in the file:

1. `[<ReferenceEquality>] StudyContext = { studies; experiments; setups; items; resolveMaterial }` with
   `member this.itemsUsedBy (study : Study) : Item list`. It reads `Experiment.itemsUsedBy` (step 261) of the study's
   experiments as `experiments` holds them, over `setups` and `items`.
2. `StudyMsg = CreateStudy | AddExperimentsToStudy | OpenMaterial | SetParameterScope | LinkParameters`, each case
   documented.
3. `member this.applyTo (context : StudyContext) : Result<Study, StudyError>`:
   - `CreateStudy` is `Study.create` over the context's studies and experiments.
   - Every other case finds its study by name, refusing with `UnknownStudyErr`, then answers one of:
     - `withExperiments`;
     - `openMaterial` of `resolveMaterial`'s entry;
     - `withParameterScope` or `withLink` over `itemsUsedBy`.
4. `StudyError` gains `UnknownStudyErr` (`the kit has no such study: <reason>`) and `MaterialUnavailableErr` (`the
   material version could not be read: <reason>`).

`Studies.fs` opens `OpticalConstructor.Domain.Setups`, because `Experiment.itemsUsedBy` is an optional extension
declared there.

Tests:

- New `StudyMessageTests.fs` (`module StudyMessageTests`), appended last to the compile list. Its four facts run over
  an in-memory kit:
  - `KitProxy.createInMemory` and `KitContext.openKit` read-write, with a stepping clock;
  - a `KitLibraryContext`;
  - CPL1, CPL2 and the Phone recorded through `recordItem`;
  - one setup holding them, recorded through `recordSetup`;
  - E1, E2 and E3 recorded through `recordExperiment`;
  - a user absorbing crystal material saved through `materials.saveMaterial`.

  Every answered study is recorded through `recordStudy`.
- `StudySessionTests`' render fact gains the two new error rows.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyMessageTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudySessionTests.fs`

## Risks

- **The brief's literal fixture cannot snapshot the saved material.** The brief names `freshStores ()`'s import seam
  and the set's `materials.saveMaterial`.
  - In the in-memory set, `saveMaterial` writes only the `MaterialProxy` (`CatalogueImport.fs` `createInMemory`).
  - The import proxy's `tryHeldEnvelope` reads its own private map, so `recordStudy` would refuse the opened version
    with `UnheldEntryErr`.
  - The fixture uses the database-backed pair step 0359's study-recording fact uses instead:
    `BuiltinCatalogue.freshLibraryDatabase ()`, `MaterialProxy.createInDatabase` and
    `CatalogueImportProxy.createInDatabase` over one database.
  - So the saved material resolves and snapshots. The deviation is recorded in the impl-log.
- **`itemsUsedBy` answers `Item list`, while `Experiment.itemsUsedBy` answers a `Result`.**
  - An experiment whose revision or held item the kit lacks contributes nothing.
  - The answer is the kit's items in kit order that the resolving experiments use. That is the call's own answer
    whenever every experiment resolves.
- **ELEV:** the new names were checked free across the solution. Everything new is a member or a type, with no
  primitive in a signature.
