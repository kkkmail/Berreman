# Step 0440 — implementation plan

AT-2 proves the exchange path as AT-1 does (spec 0050 S12.2; 001 §Q 'Send', §H). The only edited file is
`Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`, as the step's effects authorize.

## Approach

1. **`runFolderOf (run : FolderName) : FolderName`.** This is the test output's `KitAcceptanceTests/<run>`. Both
   builds compute it inline today; the round trip needs it a third time. Both builds now call it, so the path has one
   home.
2. **`sendAndReopen (context : AppContext) (model : TableAndElementRotationModel) (run : FolderName)`.** It takes the
   round trip step 430's fact composes inline and answers the reopened `KitFolder` and its window model.
   - It works on its own copy of the model. The copy's `kitShell` holds `KitArchiveProxy.create ()` and
     `KitArchiveJobHost.immediate`, which records `SendKitProgressed` and `SendKitEnded`.
   - It sends `SendKit` of `kitFile.name.zipFileName` in the run folder, outside the kit. The run folder is created
     first, because the archive does not create the zip's parent.
   - It keeps the send assertions: the job, the file count, one progress per file, the idle job, the status, the zip
     on disk, and no `.oc/` entry.
   - A start screen over `context` then sends `OpenKitZip` into the fresh folder `received/<kit file stem>`. Its clock
     is the kit's own (`model.kit.clock`), its id and author are the kit file's, and its recorded messages fold
     through `updateKitStart`.
   - It keeps the extraction assertions, and the reopened folder and access.
   - The reopened window model is `TestAppScope.inverseModelOver context opened`.
   - A failure after the open closes the reopened kit, then rethrows. On success the caller closes it through the
     window model's `kit`.
3. **`startResidualOf`.** `studyResidualAtStart` answering `Ok` with a non-empty vector. This is AT-1's inline
   `residualAtStart`, shared so both facts assert the residual the same way.
4. **Step 430's fact.** It now calls `sendAndReopen built.context built.model run`, then keeps its assertions:
   `assertSameKitFiles`, and the residual element for element.
5. **The new fact** `the absorbing-biaxial-noisy kit travels through Send kit and Open kit from zip unchanged, its
   study's residual at the start point included`.
   - It runs over `absorbingBuild.Force ()` under the fixed run `AT-2-send`. A previous run's folder is removed first,
     and the folder is removed in `finally`.
   - It asserts `assertSameKitFiles built.folder reopened` and `assertNoisyObservationFiles reopened`, which names
     the five observation files explicitly.
   - The start residual of `AbsorbingTriclinic` must be `Ok` for both and equal element for element.
   - The shared build is never closed or written. `SendKit` only reads the kit folder, and the reopen writes only into
     the received copy.

## Risks

- **xUnit v3 fact order.** The new fact may run before or after the other AT-2 facts.
  - The build's model value is immutable, and nothing in it is replaced.
  - `KitOpening` resolves snapshots without importing, so the shared context's stores do not change.
  - The start screen hands the kit to `showKit` directly, with no import dialog.
- **Time.** The fact reuses the lazy build, which already pays for the fit. It adds a zip, an unzip, five
  observation-file regenerations and two residuals, a few seconds in all.

## Verification

The step's own selections, run as development evidence:

- build;
- elevation;
- kit-acceptance over `KitAcceptanceTests`;
- ui-tests over `StudyViewTests` and `DataStepTests`;
- ui-smoke, all discovered;
- the twenty-one s42 invariant greps.
