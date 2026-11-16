# Step 0439 — implementation plan

AT-2's read path on the committed kit (spec 0050 S12.2; 001 §Q 'Open'; .spec-md §3.12). Test-only: the one file
`OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs` changes, and no product code.

## Approach

1. **Shared with AT-1.** Step 429's fact composes the read-only open and the unchanged check inline. Both become
   private functions over `CommittedKit`:
   - `openCommittedReadOnly (kit : CommittedKit) : CommittedKitSession`. It does what step 429 did inline:
     - records the committed folder's relative files, directories (`.oc/` included) and bytes;
     - creates `TestAppScope.emptyLibraryAppContext (FolderName "KitAcceptanceTests")` and asserts the library is
       empty;
     - builds a start screen whose clock is fixed at the kit's refit instant, and sends `OpenKitReadOnly` of the
       committed folder;
     - builds the window model with `TestAppScope.inverseModelOver`.

     It asserts what the open guarantees whatever the kit: no status, the committed folder, read-only access, and a
     window holding the opened kit's experiments, setups, items, studies and fit runs. It answers the context, the
     opened kit, the window model, the clock and the recording. If anything fails after the open, it closes the kit,
     checks the folder is unchanged, and re-raises.
   - `assertCommittedUnchanged (kit : CommittedKit) (recorded : CommittedKitRecording) : unit`. The folder's relative
     paths, directories and bytes must equal the recorded ones.
   - `assertEmptyLibrary (context : AppContext) : unit`. The item-model, live-library, material, sample and source
     stores list no entry. Step 429's local check, which covered the first two stores, becomes this one function.
   - `committedFolder ()` takes the kit: `committedFolder (kit : CommittedKit)`. Its callers name `OpticsMuellerKit`.
   - Step 429's fact calls these functions and keeps every assertion it makes.
2. **Light reuse of the AT-2 helpers.** Two helpers only ever read `built.model`, so each now takes a
   `TableAndElementRotationModel`:
   - `assertAbsorbingStudy`;
   - `assertNoisyObservationAttachments`.

   Their existing callers pass `built.model`. The read-only fact then checks the committed study and attachments
   through the same helpers.
3. **The new fact**, `the committed absorbing-biaxial-noisy kit opens read-only against an empty library, writes
   nothing, and reproduces AT-2's data, model and fit`. Over `openCommittedReadOnly AbsorbingBiaxialNoisyKit`:
   - (a) `assertNoisyObservationFiles` of the committed folder: each seed's committed file equals `observationFileOf`
     regenerated now.
   - (b) The library snapshot. The empty-library check passes again after the open. Each of the 8 items resolves
     through `model.tryItemModelChoice` to its stored instance and to the shipped model entry. The model version is
     absent from the database and present in the overlay. The laser's preset is absent from the database. Each
     plate resolves to the dropped sample `absorbingPlate`, which is absent from the database. Its material version
     resolves from the kit's snapshot to `absorbingMaterial` and is absent from the database.
   - (c) The kit holds:
     - the kit file (`assertKitFile`);
     - 8 items with AT-2's ids;
     - 5 setups (`assertAbsorbingSetups`) and 5 experiments (`assertAbsorbingExperiments`);
     - the study (`assertAbsorbingStudy` over the window and its read-only proxy);
     - the one fit run (`assertAbsorbingFitRun`).

     Every attachment answers `AttachedDataUnchanged`, and each reads to its plate's seed-1 observations
     (`assertNoisyObservationAttachments`).
   - (d) Data identity (`assertAbsorbingStudyData`) and model identity (the axes inside `assertAbsorbingStudy`, plus
     `assertAbsorbingStartResidual`), over `byPlate (noisyObservationsOf (List.head noisySeeds))`.
   - (e) `StudyFitParam.runFit` of `absorbingFitRequest` under `UnwatchedIterations`. Its inputs are
     `model.studyForwardContext`, the session clock fixed at 2026-06-01T12:02:00Z, `createMathNetSvd ()`,
     `UniformDeviateProvider.fromSeed (NoiseSeed 1)` and `model.kitFitRuns`. It must answer `Ok run` with the study,
     the stamp, `JointStage`, `CalibrationHeldFixed` and `NonlinearStudyEngine`, and
     `assertAbsorbingRecovery (studyIn model absorbingStudyName) run` must hold.
   - (f) `assertCommittedUnchanged` at the end of the body and again in `finally`, after the kit closes.

## Risks

- The fit takes about 816 s, and the class also runs AT-2's lazy build, which costs about 816 s more. The expected
  class total is about 1750 s, inside kit-acceptance's 3600 s.
- If the snapshot's material or sample does not equal the in-code record after its JSON round trip, the comparison
  falls back to ids. This is checked on the first run.
- The read-only proxy must not write. Every read goes through `model.kit.proxy`, which is the read-only view, never
  through a fresh `KitProxy.create` over the committed folder.

## Runs (development evidence)

- build;
- the two read-only facts filtered by method, then `KitAcceptanceTests` as the gate selects it;
- ui-tests (`StudyViewTests`, `DataStepTests`) and ui-smoke;
- elevation;
- the s42 invariants.
