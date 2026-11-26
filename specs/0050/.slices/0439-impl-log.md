# Step 0439 — implementation log

## Rulings read

- R0001–R0005 (`.manual/030`, `031`, `033`, `035`, `036`, the "Binding ruling" section of each). None of them
  conflicts with this step, and every one is preserved:
  - the series dispatch and the circular analyzer are unchanged;
  - the `ElementIdProvider` injection stays;
  - the single `rhoT` read, the server collector, the concurrent residual and the 1800 s fit bound are untouched.
- This step edits no production code.

## Progress

1. **Session types.** Two private records sit beside the module's other acceptance types:
   - `CommittedKitRecording`: the relative files and directories, `.oc/` included, and each file's bytes.
   - `CommittedKitSession`: the context, the opened kit, the window model, the start screen's clock and the
     recording.

   The slice names the context, the window model and the recording. The opened kit is added so step 429's
   assertions on `opened.*` stay verbatim and the kit can be closed. The clock is added so a refit stamps its run
   with the very clock the start screen read, as step 429 did.
2. **`committedFolder (kit : CommittedKit)`.** It now takes the kit and names its `data/` folder when the folder is
   missing. Its six callers pass `OpticsMuellerKit`.
3. **`relativeDirectories (folder : FolderName)`.** This is step 429's inline `directoriesInCommittedKit`, moved
   beside `relativeFiles`.
4. **Shared read path** (each private function has a one-line doc citing spec 0050 S12.1/S12.2):
   - `refitTimeOf (kit)`: AT-1's 2026-05-31T12:03:00Z, step 429's instant, and AT-2's 2026-06-01T12:02:00Z.
   - `assertEmptyLibrary (context)`: the item-model, live-library, material, sample and source stores list no entry.
     It is step 429's local check widened to the stores slice (b) names.
   - `assertCommittedUnchanged (kit) (recorded)`: step 429's closing check. The relative paths, directories and
     bytes must equal the recorded ones.
   - `openCommittedReadOnly (kit)`, which:
     - records the folder;
     - creates `emptyLibraryAppContext (FolderName "KitAcceptanceTests")` and asserts it is empty;
     - builds the start screen through the existing `kitStartOver`, with the kit's id and author and a clock fixed at
       `refitTimeOf kit`, and sends `OpenKitReadOnly`;
     - asserts no status, the committed folder and read-only access;
     - builds the window with `TestAppScope.inverseModelOver` and asserts its read-only access and that it holds the
       opened experiments, setups, items, studies and fit runs.

     A failure after the recording closes any opened kit and checks the folder, then re-raises.
   - `readOnlyStudyFit (session)`: the `StudyFitParam` that step 429 composed inline. It takes the window's forward
     context, the session clock, `createMathNetSvd ()`, `UniformDeviateProvider.fromSeed (NoiseSeed 1)` (as
     `StudyFitEnvironment.create` composes them) and `model.kitFitRuns`.
   - `closeCommittedReadOnly (kit) (session)`: closes the kit, then asserts the folder unchanged in a `finally`.
5. **Step 429's fact** now calls `openCommittedReadOnly OpticsMuellerKit`, `readOnlyStudyFit`,
   `assertCommittedUnchanged` and `closeCommittedReadOnly`. Every assertion is kept, and the generic open assertions
   moved into the shared open. Its lab files are read from `opened.folder`, which the open asserts is the committed
   folder.
6. **Two AT-2 helpers take the window model.** `assertAbsorbingStudy` and `assertNoisyObservationAttachments` only
   ever read `built.model`, so each now takes `(model : TableAndElementRotationModel) (reader : KitProxy)`. Their
   facts pass `built.model`, and nothing else changed.
7. **`assertAbsorbingSnapshotResolution (session)`.** The 8 items' ids must be AT-2's. Each item must resolve
   through `model.tryItemModelChoice` to:
   - its stored instance;
   - its shipped model entry (`spec.modelId`), with the version absent from the database and present in the overlay;
   - for the laser, its preset, absent from the database;
   - for each plate, `absorbingPlate` at its first version, absent from the database. The plate's substrate material
     version is `absorbingMaterialVersion`, absent from the database, and the overlay resolves it to
     `absorbingMaterial`.
8. **The new fact** runs over `openCommittedReadOnly AbsorbingBiaxialNoisyKit`. All its reads go through the
   window's read-only `model.kit.proxy`; it never creates another `KitProxy` over the committed folder.
   - (a) `assertNoisyObservationFiles` of the committed folder: exactly the five seed files, each byte-identical to
     `observationFileOf` regenerated now, and each parsing back.
   - (b) `assertEmptyLibrary` after the open, then `assertAbsorbingSnapshotResolution`.
   - (c) The kit, checked by the existing helpers:
     - `assertKitFile`;
     - `assertAbsorbingSetups`, `assertAbsorbingExperiments` and `assertAbsorbingConfigurations`;
     - `assertConfigurationInputs`;
     - `assertAbsorbingStudy`;
     - `assertAbsorbingFitRun` of the one stored run.

     Every attachment's `attachmentStatus` is `Ok AttachedDataUnchanged`, `assertNoisyObservationAttachments` holds,
     and then `assertCommittedUnchanged` holds.
   - (d) `assertAbsorbingStudyData` and `assertAbsorbingStartResidual` over
     `byPlate (noisyObservationsOf attachedSeed)`.
   - (e) `(readOnlyStudyFit session).runFit absorbingFitRequest UnwatchedIterations` must answer `Ok run` with the
     study, the stamp 12:02, `JointStage`, `CalibrationHeldFixed`, `NonlinearStudyEngine` and the five plates' runs.
     `assertAbsorbingRecovery (studyIn model absorbingStudyName) run` must hold.
   - (f) `assertEmptyLibrary` and `assertCommittedUnchanged`, then `closeCommittedReadOnly` in `finally`.
9. **Build.** `dotnet build Berreman.slnx -c Release -nologo -v:m` exited 0 in 86 s with 0 errors. The only warnings
   are the exempt NU1902 (12) and the vendored MathNet SYSLIB0051 (4).
10. **The s42 invariants.** Each of the 21 decoded `git grep` commands found nothing
    (`.artifacts/0439-invariants.log`).
11. **The two read-only facts, filtered by method.** The run used the gate's command with `--filter-method` wildcards
    on the two fact names, and passed 2 of 2 in 626 s. The AT-2 fact took 618.9 s; AT-1's took 3.4 s. Log:
    `.artifacts/0439-kit-acceptance-readonly.log`.
    - Every equality on the snapshot path held on the first run. The plate's sample equals `absorbingPlate` and the
      material equals `absorbingMaterial`, both after their JSON round trip, so no fallback to ids was needed.
    - `git status` shows nothing under `data/` afterwards.
12. **elevation**, ELEV004 excluded: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` in 344 s, against 600 s.
    The scope is all 13 projects, because no comparison base was supplied outside the arc. It ran concurrently with
    item 11. Log: `.artifacts/0439-elevation.log`.
13. **ui-tests** (`StudyViewTests`, `DataStepTests`): `Passed!`, 18 of 18, in 35 s against 360 s. Log:
    `.artifacts/0439-ui-tests.log`.
14. **ui-smoke**, all discovered: `Passed!`, 264 of 264, in 299 s against 600 s. Log: `.artifacts/0439-ui-smoke.log`.
15. **kit-acceptance over the class**, with the gate's command and selector: `Passed!` in 1408 s, against 3600 s.
    Log: `.artifacts/0439-kit-acceptance.log`.
    - Total 14: 13 succeeded, 1 skipped, 0 failed. The count rises from 12 to 13.
    - The skip is the explicit five-seed ensemble (TRX `NotExecuted`), as at 0437 and 0438.
    - The new fact passed in 549.1 s.
    - The AT-2 build, its fit included, is charged to the first AT-2 fact that ran, the grid fact, at 790.1 s.
    - A sha256 manifest of `data/absorbing-biaxial-noisy/` and `data/optics-mueller/`, 175 files, is identical before
      and after the run (`.artifacts/0439-data-sha256-before.txt`, `…-after.txt`).
16. **Line endings.** The test file, the plan and this log hold no CR; `git diff --numstat` equals its
    `--ignore-cr-at-eol` form.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`

## Testing state

These are development runs, evidence only; the supervisor's gates decide.

- **build:** exit 0, 86 s (600 s).
- **elevation:** OK, 0 new findings, 344 s (600 s).
- **ui-smoke:** 264 of 264, 299 s (600 s).
- **ui-tests:** 18 of 18, 35 s (360 s).
- **kit-acceptance:** 13 succeeded and 1 explicit skip, 1408 s (3600 s).
- **The 21 s42 invariants:** each finds nothing.

No run exceeded its gate's effective deadline. Every requirement of the slice is addressed, and nothing is deferred.

## Artifacts

All in `specs/0050/.artifacts/`:

- `0439-build.log`
- `0439-invariants.log`
- `0439-elevation.log`
- `0439-ui-tests.log`
- `0439-ui-smoke.log`
- `0439-kit-acceptance-readonly.log`
- `0439-kit-acceptance.log`
- `0439-data-sha256-before.txt`
- `0439-data-sha256-after.txt`

## Gotchas

- **The session record has more than the slice names.** The slice says `openCommittedReadOnly` answers the context,
  the window model and the recording. `CommittedKitSession` also holds:
  - the opened kit, so step 429's assertions on `opened.*` stay verbatim and the kit can be closed;
  - the start screen's clock, so a refit stamps with the clock the open used, as step 429 did.
- **Step 429's assertions now live in two places.** Its generic open assertions moved into the shared open, so they
  now run for both kits:
  - the `OpenKitReadOnly` status, the opened folder and read-only access;
  - the window's access and the window holding the opened experiments, setups, items, studies and fit runs.

  Its kit-specific counts stay in its fact. Its empty-library check now also covers the material, sample and source
  stores, which slice (b) names for AT-2. All of them are empty in both sessions.
- **AT-1's start screen now uses the shared `kitStartOver`.** It takes the committed kit's own id and author, where
  it used to take `KitIdProvider.system`. `OpenKitReadOnly` never mints a kit id, so the open is unchanged.
- **Reads go only through the window's read-only proxy.** No new `KitProxy` is created over a committed folder, so a
  writing proxy can never touch `data/`.
- **The second AT-2 fit is not compared to the stored run.** The read-only refit runs with `model.kitFitRuns` (the
  committed run), as the slice prescribes, so it is held to the recovery bands only. The stored run is checked
  separately by `assertAbsorbingFitRun`.
- **Timing.** The new fact's fit took 549–619 s. The class now takes about 23.5 min, against the 3600 s deadline.
  R0004 and R0005 bound AT-2's fit through the shell at 1800 s; that fit is unchanged, measured inside the grid
  fact's 790 s.
