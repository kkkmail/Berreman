# Step 0213 — impl log

## Progress

- [x] (1) `Kit.fs`: opens `Placement`, `Library` and `ItemModels`; appends `ItemError`, `ItemId`, `ItemLabel`,
  `ItemNotes`, `ItemRevision` and `ItemModelInstance`, in the brief's order
- [x] (2) `KitItemTests.fs` with the four declared facts; compile entry appended to the test fsproj, no comment
- [x] build, constructor-unit-tests, elevation, invariants (development runs)

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs` (+89 lines):
  - opens `OpticalConstructor.Domain.Placement`, `.Library` and `.ItemModels`, placed in compile order before
    `LibraryEntryFiles`;
  - `ItemError`, with seven `of reason : string` cases and a `describe` member in step 39's form:
    `InvalidItemLabelErr`, `DuplicateItemLabelErr`, `DuplicateItemIdErr`, `UnknownItemParameterErr`,
    `InvalidParameterStateErr`, `InvalidItemModelBindingErr` and `UnknownKitItemErr`;
  - `ItemId of Guid` with `.value`;
  - `ItemLabel of string` with `.value`, `tryCreate` and `fileStem`;
  - `ItemNotes of string` with `.value`;
  - `ItemRevision of int` with `.value`, `first` and `next`;
  - `ItemModelInstance` = `PresetInstance of model : ItemModelVersionId * preset : ElementValueId` |
    `SampleInstance of model : ItemModelVersionId * sample : SampleVersionId` |
    `ItemModelVersionInstance of ItemModelVersionId`, with `entryReferenceOpt` and `bindingReferenceOpt`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitItemTests.fs` (new): `module KitItemTests` in
  `namespace OpticalConstructor.Tests`, holding the four declared facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one line,
  `<Compile Include="KitItemTests.fs" />`, appended after `ItemModelProxyTests.fs`, the last compile entry.

## Decisions

1. **`entryReferenceOpt` is `Some model.entryReference` in every case.** The brief requires this, and it holds
   even for a preset id that is not a GUID. It reuses step 210's `ItemModelVersionId.entryReference`. The option
   shape matches `ElementBinding.entryReferenceOpt`, which step 215's `Item.libraryReferences` chains with the
   binding.
2. **`bindingReferenceOpt` follows step 137's rules:**
   - a preset is `tryToLibraryEntryId |> Option.map EntryLatestReference`, the same expression as
     `ElementBinding.BoundByEntryId`;
   - a sample version is the `EntryVersionReference` of `sampleId.toLibraryEntryId` and its version;
   - a standalone model is `None`.

   A non-GUID preset answers `None` for the binding but keeps its model reference. Step 216's `ItemDraft.addTo`
   refuses it with `InvalidItemModelBindingErr`.
3. **The sample's `LibraryVersionRef` is built in `Kit.fs`.** `VersionRef.entryReference` (`Lifecycle.fs`) holds
   the same two-field construction, but it compiles after `Kit.fs` and lies outside this step's authorized edits,
   so it cannot be called here. The construction reuses `SampleId.toLibraryEntryId`, as `LibraryEntryFiles.fs`
   (`ItemModelVersionId.entryReference`, `LibraryEntryContent.references`) and `KitLibrary.fs` also do. It is
   annotated `: LibraryVersionRef` so record-label resolution cannot drift as more modules are opened.
4. **`InvalidItemModelBindingErr` renders as `the item model binding is not valid: <reason>`.** The brief gives no
   text for it; the wording follows the `… is not valid: <reason>` form of the other cases. `UnknownKitItemErr`
   renders exactly as the brief gives it: `the kit has no item of that id: <reason>`.
5. **`ItemNotes` has a `.value`.** The brief lists none, but `KitNotes` has one, and AGENTS.md gives every
   single-case primitive DU a `.value` accessor. It is the serialization seam that step 219 needs.
6. **The tests build their expected references by hand, independently of the members under test.** The members
   under test are `entryReferenceOpt`, `bindingReferenceOpt` and `ItemModelVersionId.entryReference`. The id
   conversion `toLibraryEntryId` is not one of them, so the oracle reaches the entry id through that named member
   on the source type. A cross-type re-wrap through `.value` is not needed for independence, and AGENTS.md forbids
   it, fixtures included (attempt 02, code judge cycle 1).
   - The model reference is the `EntryVersionReference` of `model.itemModelId.toLibraryEntryId` and
     `model.version` (`exactModelVersion`). The test also checks that it equals the library's own
     `ItemModelVersionId.entryReference`.
   - The sample reference is the `EntryVersionReference` of `SampleIds.glassPlate1mmId.toLibraryEntryId` at
     `VersionNumber.first`.
   - The preset reference stays `LibraryEntryId (Guid.Parse preset.value)`, not a re-transcribed GUID literal.
     Turning the preset text into a GUID is the behaviour being checked, and its only named member,
     `tryToLibraryEntryId`, is that conversion.

   A wrong GUID or a wrong version still fails the fact.
7. **The tests check a bit more than the brief lists, inside the four declared identities:**
   - the `CPL_2.item-json` file name;
   - a padded label (`" QZ "`) kept as given;
   - revision 2;
   - two non-GUID preset ids (`not-a-guid` and an upper-case GUID) keep the model reference and bind nothing.

   No new test identity was added.

## Attempt 02 (code judge cycle 1: route back)

- **The judge's finding:** `KitItemTests.fs` re-wrapped across types through `.value` at line 18
  (`LibraryEntryId model.itemModelId.value`) and line 63 (`LibraryEntryId SampleIds.glassPlate1mmId.value`).
  This is the `ObjectTwo one.value` form that AGENTS.md disallows in fixtures as well. The ELEV gate does not
  scan test projects, so nothing downstream catches it.
- **The repair:** line 18 is now `entryId = model.itemModelId.toLibraryEntryId`, and line 63 is now
  `entryId = SampleIds.glassPlate1mmId.toLibraryEntryId`.
  - Both members already exist (`LibraryEntryFiles.fs`, `ItemModelId` / `SampleId` extensions).
  - The preset oracle (`Guid.Parse preset.value`) is unchanged.
  - Every assertion and the four fact names are unchanged.
  - Nothing else was touched: `Kit.fs` and the fsproj are byte-identical to attempt 01.
- Decision 6 has been corrected.
- **Development runs after the repair:**
  - build: exit 0, 0 warnings, 0 errors. `OpticalConstructor.Tests.dll` was rebuilt after the edit.
  - `KitItemTests` (filtered): 4 of 4 passed.
  - The whole constructor suite: 1292 passed, 0 failed, 0 skipped, in 1 min 05 s.
  - elevation (`ASG_ARC_BASE_COMMIT` = `.arc-base-json` origin `f395597`, ELEV004 excluded):
    `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 197 s against a 600 s deadline.
- **Invariants:** the new test file was grepped directly for every s22 pattern, with no match. `git diff
  --numstat` equals `--ignore-cr-at-eol`, and the test file holds no CR.

## Testing state

These are development runs. The arc-runner's gate engine gives the verdict (Invariant 6). For the attempt-02
results, see the section above. The attempt-01 record follows.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0, 0 warnings, 0 errors.
  - The first attempt failed with FS0039 because `FileName` was not in scope in the new test. I added
    `open Softellect.Sys.Primitives` to the test file.
  - That first, wider rebuild also recompiled the vendored `MathNetNumerics/Numerics` C# project. Its output
    showed `SYSLIB0051` from `Exceptions.cs`, which is pre-existing and also appears in the arc's older
    `b11-build.log`. This step did not touch it, and every later build reported 0 warnings.
- **KitItemTests** (filtered): 4 passed.
- **Mutation check:** I changed `bindingReferenceOpt`'s preset case to `None`, rebuilt, and ran the class.
  - `an item's model is referenced as the library snapshots it` failed, and the other three passed.
  - I restored `Kit.fs` from a byte copy (the copy was then deleted), rebuilt with 0 warnings, and the class
    passed 4/4.
- **constructor-unit-tests** (whole suite, the gate's command form with TRX): 1292 passed, 0 failed, 0 skipped,
  in 65 s against a 180 s estimate. That is 1288 → 1292 (+4), and the TRX holds the four `KitItemTests` results.
- **elevation** (`ASG_ARC_BASE_COMMIT` = `.arc-base-json` origin `f395597`, ELEV004 excluded):
  `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 190 s against a 600 s deadline. All 12 projects ran
  because the slnx changed earlier in the arc.
- **The fourteen `invariant-s22-*` gates** (their snapshot commands): each prints `INVARIANT HOLDS` and exits 0.
  - `git grep` does not see the untracked `KitItemTests.fs`, so I also grepped it directly for every invariant
    pattern: no match.
- **LF line endings:** `git diff --numstat` equals `--ignore-cr-at-eol` for both modified files. No file of this
  step holds a CR, and the new file has no BOM.

## Artifacts

- `specs/0050/.artifacts/0213-dev-build.log`: the attempt-02 build (0 warnings, 0 errors).
- `specs/0050/.artifacts/0213-dev-constructor-unit-tests.log`: the attempt-02 whole constructor suite (1292
  passed).
- `specs/0050/.artifacts/0213-dev-elevation.log`: the attempt-02 ELEV run from the arc origin (OK, 0 new
  findings).

## Gotchas

- **`FileName` lives in `Softellect.Sys.Primitives`, not in `LibraryEntryFiles`.** A test that names it must open
  `Softellect.Sys.Primitives`.
- **`VersionRef.entryReference` (`Lifecycle.fs`) cannot be called from `Kit.fs`, because it compiles later.** A
  sample-version reference needed before `Lifecycle.fs` is built from `SampleId.toLibraryEntryId` and the version
  (Decision 3).
- **`git grep`-based invariants do not see untracked files.** A new file has to be checked against them directly
  until it is committed.
