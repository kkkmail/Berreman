# Step 0449 — impl log

## Progress

- Read the task, the system and project prompts, the slice, and the 0448 state of the world.
- Read the operator rulings R0001–R0005 (`.manual/030`, `031`, `033`, `035` and `036`, their "Binding ruling" sections).
  - Each ruling grants extra authority at step 0427, 0428 or 0436 only.
  - At 0449 they ask only that their behaviour is preserved.
  - This step touches none of that behaviour: not `StudyEngine`, `ItemEvaluation`, `StudyForward`,
    `TableAndElementRotationView` or `FittedProblems`, not the solver, and not the GC setting in any project file.
- Read the seams this step composes:
  - `MaterialSourceStore.createRefractiveIndexInfo`, which builds without touching anything, and
    `SourceArchiveDownloader.http`;
  - `ClockProvider.system`, an extension in `KitOpening.fs`, which compiles before `AppContext.fs`;
  - `CatalogueImportContext.create`, which answers `materialSources = []`;
  - `TestAppScope.testLocations` and `freshAppContext`;
  - the `DefaultStores` and `initWith` compositions in `TableAndElementRotationView.fs`.
- Wrote the impl plan.
- Edited `AppContext.fs`:
  - the private `compose`, which `create` and `recreateLibrary` share, now sets `materialSources` beside
    `exportFolderOpt` in the same record update;
  - the value is `[ MaterialSourceProxy.createRefractiveIndexInfo locations.materialSourcesFolder
    SourceArchiveDownloader.http ClockProvider.system ]`;
  - four opens were added (`Domain.Kit`, `Domain.MaterialSources`, `Storage.MaterialSourceStore` and
    `Ui.KitOpening`);
  - the `catalogueImports` field doc now names the source and the folder and cites S13.1.
- Added the fact `the app scope offers refractiveindex.info over its configured sources folder, holding no snapshot`
  to `AppContextTests`, with one open (`Domain.MaterialSources`).
- Built the UI test project: 0 errors, and no warning from our code (see Testing state).
- Ran the class, filtered: 19 of 19 pass. The class had 18 facts before this step.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs`: the composition, four opens and the field doc.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/AppContextTests.fs`: one open, and the new fact after
  `the app's import context suggests the configured catalogues folder for exports`.

No other file changed. `CatalogueImportContext.create` (Storage), `DefaultStores`, `initWith` and every test import
context still compose `materialSources = []`.

## Testing state

These are development checks only. Under the IMPLEMENT worker's Invariant 6, the gate engine runs the step's roster
after this session exits, so no gate was run here.

- `dotnet build OpticalConstructor.Ui.Tests.fsproj -c Release -nologo -v:m`:
  - exit 0 in 79 s, against a 600 s deadline; 0 errors;
  - 6 warnings, none from our code:
    - SYSLIB0051 in the vendored `MathNetNumerics/Numerics/Exceptions.cs`; these were already there, and step
      0446's build log has them too;
    - NU1902 advisories, which are exempt.
  - Log: `.artifacts/0449-dev-build-ui-tests-project.log`.
- `dotnet test --no-build -c Release -p:Platform=x64 --no-ansi --filter-method
  "OpticalConstructor.Ui.Tests.AppContextTests.*" --filter-method "OpticalConstructor.Ui.Tests.AppContextTests"`:
  - `Passed!`: 19 total, 19 succeeded, in 43 s against a 360 s deadline.
  - Log: `.artifacts/0449-dev-test-AppContextTests.log`.
  - `--list-tests` lists `OpticalConstructor.Ui.Tests.AppContextTests.the app scope offers refractiveindex.info over
    its configured sources folder, holding no snapshot`.
  - The class has one more fact than at HEAD (18 → 19).
- The s43 invariants: each of the twenty-one base64-decoded `git grep` commands in `.slices/0449.gates` finds
  nothing in the working tree.
- LF: `git diff --numstat` matches the `--ignore-cr-at-eol` output, and neither edited file contains a CR.
- Not run here:
  - elevation, the whole-solution build, ui-smoke and the whole constructor-unit-tests suite are the gate engine's;
  - no file in `OpticalConstructor.Tests` or below `Ui` changed, so constructor-unit-tests stays at step 448's 1795;
  - the new fact has no `ui-smoke` trait.

## Artifacts

- `specs/0050/.artifacts/0449-dev-build-ui-tests-project.log`
- `specs/0050/.artifacts/0449-dev-test-AppContextTests.log`

## Gotchas

- **`ClockProvider.system` is an extension in `OpticalConstructor.Ui.KitOpening`.** That module has to be opened for
  `AppContext.fs` to see the member, and it compiles earlier in the Ui project. `createRefractiveIndexInfo` is likewise
  an extension of `MaterialSourceProxy` in `Storage.MaterialSourceStore`.
- **The brief says "the record's doc names the sources in no more words".** I read this as: the doc on the
  `catalogueImports` field, where the sources live, names refractiveindex.info and its folder in one added line
  citing S13.1. The type-level doc is unchanged.
- **The fact asserts `Directory.Exists` is false for the sources folder.** That is stronger than "the folder is
  empty": it also proves composing creates no folder, as the brief requires.
  - The folder is `TestAppScope/AppContextTests/Sources` under the test output.
  - No other fact writes there: `fileLocations` uses a GUID sub-owner, and no UI path runs `updateSnapshot` yet.
