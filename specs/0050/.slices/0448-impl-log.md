# Step 0448 — impl log

## Progress

- Read the task, the system and project prompts, the slice and the 0447 state of the world.
- Read the operator rulings R0001–R0005 (`.manual/030`, `031`, `033`, `035`, `036`, "Binding ruling" sections).
  - Each ruling grants its extra authority at step 0427, 0428 or 0436 only.
  - At 0448 they ask only that their behaviour is preserved. This tests-only step touches none of it: no
    `StudyEngine`, `ItemEvaluation`, `StudyForward`, `TableAndElementRotationView`, `FittedProblems`, solver or
    project-file GC setting.
- Read the seams the facts drive:
  - `MaterialSourcePulls`: `pullRecord`, `updateSourceSnapshot` and `refreshPulls`;
  - `MaterialSourceStore.createRefractiveIndexInfo`;
  - `MaterialSourceFixture`;
  - `CatalogueImportProxy.createInDatabase` and `CatalogueImportContext.create`;
  - `MaterialProxy.createInDatabase`;
  - `LibraryDatabase.run` and `LibraryPulledSourceRow` (the `PulledSources` set, SQL table `PulledSource`);
  - `CatalogueImportProxy.attributionLinesOf`.
- Wrote `MaterialSourcePullSqliteTests.fs`, the brief's four facts, and appended it last in the compile list, with no
  comment.
- The first build failed with three FS0039 errors: `MaterialError` has no `describe`.
  - Added a private `materialRefusal` that answers the reason every `MaterialError` case carries, as
    `BuiltinCatalogue.materialStore` and `KitOpeningTests` match the cases.
  - Rebuilt: 0 warnings, 0 errors.
- Ran the new class, filtered: 4 of 4 pass.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialSourcePullSqliteTests.fs` (new): `module
  MaterialSourcePullSqliteTests` in `namespace OpticalConstructor.Tests`, with a two-line module doc citing spec 0050
  S13.1.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one
  `<Compile Include="MaterialSourcePullSqliteTests.fs" />`, after `MaterialSourceStoreTests.fs`.

No product file changed.

## Testing state

These are development checks only. Under the IMPLEMENT worker's Invariant 6, the gate engine runs the step's roster
(build, elevation, constructor-unit-tests and the twenty-one s43 invariants) after this session exits, so no gate
was run here.

- `dotnet build OpticalConstructor.Tests.fsproj -c Release`:
  - the first try failed with FS0039 ×3 (see Progress);
  - after the fix it exits 0 in 25 s, with 0 warnings and 0 errors.
  - Log: `.artifacts/0448-dev-build-tests-project.log`.
- `dotnet test --no-build -c Release --filter-method "OpticalConstructor.Tests.MaterialSourcePullSqliteTests.*"
  --filter-method "OpticalConstructor.Tests.MaterialSourcePullSqliteTests"`:
  - `Passed!`, total 4, succeeded 4, in 4.9 s.
  - Log: `.artifacts/0448-dev-test-MaterialSourcePullSqliteTests.log`.
- The s43 invariant patterns:
  - each decoded `git grep` of `.slices/0448.gates` finds nothing in the tracked tree;
  - the same patterns, run over the new, untracked test file and the edited project file, find nothing.
- The `constructor-unit-tests` count should rise by exactly 4, from step 447's 1791 to 1795. That figure is
  1791 + 4 and was not measured here; the gate engine measures it.

## Artifacts

- `specs/0050/.artifacts/0448-dev-build-tests-project.log`
- `specs/0050/.artifacts/0448-dev-test-MaterialSourcePullSqliteTests.log`

## Gotchas

- **The pulled entry's id is fixed.** The import context is `CatalogueImportContext.create
  (CatalogueImportProxy.createInDatabase database)` with its `newEntryId` replaced by one fixed GUID, as
  `MaterialSourcePullTests` does. Every expected `SourcePullResult` is then exact. The material store is also
  addressed by that `MaterialId` (through `toLibraryEntryId`) rather than by a GUID re-wrapped from the pull's answer.
  The context is otherwise exactly the brief's composition.
- **The page hash** is `ContentHash.ofBytes` of the page text's UTF-8 bytes. Those are exactly the bytes `writeSnapshot`
  and `writeArchive` write, so this is the page file's hash, computed independently of the adapter.
- **Unused archive.** Facts 1 and 4 never update, but each fact still gets the adapter the brief names, over a
  `copyingDownloader`. Their archive is of `siPage ()`.
- **The snapshot's version and file count.** `refreshedWith` expects the installed snapshot `{ 2026-11-04, 3 files }`.
  The brief names only its version; three files is what the adapter installs from `writeArchive`'s archive: the
  catalog and two pages.
- **n 3.790 at 0.70 µm** is asserted twice:
  - exactly on the tabulated table's last `n`, which is not interpolated, so exact float equality is right;
  - and as whole-physics equality with `changedSiPage ()`'s import.
  Version 1's table still reads 3.785.
- **Rows are compared as elevated values.** `PulledSources` rows are materialised inside `run`, because the context is
  disposed afterwards. Each is mapped at that read into a private record of elevated values, so the comparisons are
  structural, not by reference.
- **`MaterialError` has no `describe` member**, unlike most error unions here. A test that renders one matches its
  cases.
