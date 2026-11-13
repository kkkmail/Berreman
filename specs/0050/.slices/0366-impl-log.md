# Step 0366 — impl log

## Progress

- Read the protocol, the project prompt, AGENTS.md, slice 0366 and its gates snapshot, and step 0365's state of the
  world.
- Read `Studies.fs`, `StudyTests.fs`, `StudyMaterialTests.fs`, `StudySessionTests.fs`, `KitStudyFileTests.fs` and
  `KitLibraryTests.fs` (the `StudyFileFixtures`, `snapshotFixture` and the study-recording facts of step 0359).
- Also read `KitContentFiles.fs` (`recordItem`, `recordSetup`, `recordExperiment`, `recordStudy`, `readStudies`),
  `Setups.fs` (`Experiment.itemsUsedBy`), `KitLibrary.fs` (`snapshotReferences`) and `CatalogueImport.fs`
  (`InMemoryCatalogueStores`).
- Checked by grep that `StudyContext`, `StudyMsg`, the five message cases, `UnknownStudyErr` and
  `MaterialUnavailableErr` are free names across the solution (ELEV014, ELEV016).
- `Studies.fs`:
  - It opens `OpticalConstructor.Domain.Setups`. `Experiment.itemsUsedBy` (step 261) is an optional extension
    declared there, so it is visible only with the module open. Unique names (ELEV014–016) make the open safe.
  - `StudyError` gains, last, two cases rendered as the step words them:
    - `UnknownStudyErr`: `the kit has no such study: <reason>`;
    - `MaterialUnavailableErr`: `the material version could not be read: <reason>`.
  - Last in the file:
    - `[<ReferenceEquality>] StudyContext = { studies; experiments; setups; items; resolveMaterial }` with
      `member this.itemsUsedBy (study : Study) : Item list`.
    - `StudyMsg` with its five cases, each documented and citing S11.1.
    - `member this.applyTo (context : StudyContext) : Result<Study, StudyError>`.
  - `applyTo`:
    - `CreateStudy` is `Study.create name experiments context.studies context.experiments`.
    - Every other case first finds its study in `context.studies` by exact name. When none has it, the answer is
      `UnknownStudyErr "'<name>' is not among the kit's <n> studies"`.
    - Then each case answers:
      - `AddExperimentsToStudy`: `withExperiments experiments context.experiments`;
      - `OpenMaterial`: `context.resolveMaterial material`, then `openMaterial material entry`;
      - `SetParameterScope`: `withParameterScope scoped (context.itemsUsedBy held)`;
      - `LinkParameters`: `withLink link (context.itemsUsedBy held)`.
    - The study is found before the material is resolved, so a message naming no study answers `UnknownStudyErr`
      whatever its version.
- `StudySessionTests.fs`: `every study error renders its condition and its payload` gains rows 17 and 18 for the two
  new cases. The distinctness check covers them.
- New `StudyMessageTests.fs` (`module StudyMessageTests` in `namespace OpticalConstructor.Tests`), appended last to
  the compile list with no comment. It has four facts, the four ids of the census.
- Development build, constructor suite, elevation and the 20 invariant greps were run over the final tree (see
  Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs` — the `Setups` open, two `StudyError` cases,
  `StudyContext`, `StudyMsg` and `applyTo`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyMessageTests.fs` — new, four facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — `StudyMessageTests.fs`
  appended last.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudySessionTests.fs` — two new render rows.

## Testing state

These are development runs of each gate's own command from `0366.gates`, over the final tree:

- **build:** exit 0, `0 Error(s)`, 6 warnings, all the exempt NU1902 and the vendored MathNet SYSLIB0051. The first
  build took 80 s, the incremental ones 23 s (deadline 600 s). Log: `.artifacts/0366-dev-build.log`.
- **constructor-unit-tests:** `Passed!`, 1608 of 1608, 1604 + 4. It took 71 s, then 78 s over the final tree
  (deadline 180 s). The TRX shows the four `StudyMessageTests` facts and the widened render fact passed. Log:
  `.artifacts/0366-dev-constructor-unit-tests.log`.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 233s` (deadline 600 s), with 1520 ELEV004
  findings suppressed and 0 known findings. Log: `.artifacts/0366-dev-elevation.log`.
- **invariant-s36-\* (20):** each decoded `--find-nothing` grep finds nothing. The new, still untracked test file was
  also grepped directly for every pattern, since `git grep` skips untracked files.

## Artifacts

- `specs/0050/.artifacts/0366-dev-build.log`
- `specs/0050/.artifacts/0366-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0366-dev-elevation.log`

## Gotchas

- **Deviation (fixture library): a database-backed library instead of `freshStores ()`.**
  - The brief puts the kit's `KitLibraryContext` over `BuiltinCatalogue.freshStores ()`'s import seam and saves the
    material through "the set's" `materials.saveMaterial`.
  - In the in-memory set, `saveMaterial` writes the `MaterialProxy` alone. The import proxy's `tryHeldEnvelope` reads
    its own private map (`CatalogueImport.fs` `createInMemory`), so `recordStudy` cannot snapshot the opened version.
  - Measured: with that fixture, `OpenMaterial snapshots …` failed on `recordStudy` with "neither the library nor the
    kit holds the reference: 36600000-0000-4000-8000-000000000001 v1" (`UnheldEntryErr`). The other three facts
    passed. The probe edit was reverted and the tree rebuilt.
  - The fixture instead uses the pair step 0359's study-recording fact uses: one
    `BuiltinCatalogue.freshLibraryDatabase ()` under `CatalogueImportProxy.createInDatabase` and
    `MaterialProxy.createInDatabase`.
  - The material is still saved through the set's `materials.saveMaterial`, and `resolveMaterial` still reads its
    `resolveVersion`. Everything else is as briefed.
- **`itemsUsedBy` answers `Item list`, while `Experiment.itemsUsedBy` answers a `Result`.** Each of the study's
  experiments (as `experiments` holds them) is resolved alone, and one whose setup revision or held item the kit
  lacks adds none.
  - The answer is the kit's items, in kit order, that the resolving experiments use. That is exactly the
    single-call answer when every experiment resolves.
  - So one inconsistent run does not hide the items of the rest. `recordExperiment` refuses such a run anyway.
- **Study lookup is by exact `StudyName`.** The window sends the name as the kit read it. A case-variant name is a
  different name here; `CreateStudy` refuses it through `Study.create`'s file-stem rule.
- **The fixture's items are step 0362's drafts** (same ids, labels, models and mountings), left at their models'
  known values. `StudyTests` keeps its own private freed copies. Neither the scope nor the link needs an unknown, and
  `StudyTests.fs` is outside this step's edits, so the drafts were not shared.
- **`specs/0050/.manifest.state.json` was already modified** when the round started. It is the supervisor's, and this
  round did not touch it.
