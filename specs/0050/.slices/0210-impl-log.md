# Step 0210 — impl log (IMPLEMENT: the item model becomes a library kind)

## Progress

**Read first**

- The task, both system prompts, the project prompt, AGENTS.md, the slice and its gates.
- Manual 005, which names step 0210: keep the integrity fact's identity and extend its counts. Manual 013: the arc
  origin `f395597c…` for elevation.
- Oracle obligation S71-LIBRARY-KIND and step 0209's handoff.

**Product code**

- Domain (`LibraryEntryFiles.fs`):
  - `ItemModelEntryKind`: tag 7, `.item-model-json`, schema v1, last in `all`.
  - `ItemModelId.toLibraryEntryId` and `ItemModelVersionId.entryReference`.
  - `ItemModelPayload of ItemModelForm`, with `ofItemModel` and `toItemModel` (marked `StorageBoundary`).
  - `references` answers `[]` for an item model.
- Every exhaustive match on the kind or the payload gained the case, with no wildcard. The import apply order ends
  with item models. The in-memory import decodes an item model and pushes `{ version; entry }` through
  `ItemModelProxy.saveItemModel`; an `ItemModelError` becomes `ImportRejectedErr` with its `describe`.
- Storage:
  - `library-item-model.schema.json` (a `PreserveNewest` content item) and its `SchemaValidation` lazy.
  - The `ParameterKind` int-tag adapter in `EntryFileJson.fs`.
  - The manifest schema admits row kind 7.
- UI: the import-row caption `Item model`.

**Tests**

- Added `BuiltinCatalogue.itemModels` / `itemModel`.
- Extended the integrity, wire-tag and import-tape facts.
- Added the two new facts.
- Gave the compile-forced arms to the two other test files.

**The exporter**

- Backed up `Builtin/` to `.artifacts/0210-builtin-before/`.
- Ran `dotnet fsi Berreman/OpticalConstructor/Catalogue/export-item-models.fsx` once from the repository root,
  after a full Release build. It printed "export-item-models wrote 72 entries, 11 of them item models"
  (`.artifacts/0210-export-item-models.log`).
- `git status` shows exactly the eleven new `.item-model-json` files and the rewritten `.catalogue-json` (+77
  lines). Every other entry file was rewritten byte for byte.
- `git check-ignore` reports none of the eleven. All are LF-only, with no BOM.

**Development runs**

- First constructor run: 496 failures, all from one cause. The manifest schema capped row `kind` at 6, so the
  shipped catalogue no longer loaded. After raising it to 7, two `EntryFileJsonTests` fixtures that assumed six
  kinds failed; both were repaired.
- The final runs are green; the counts are under Testing state.

**Attempt 2 (retry hint: close 'the in-memory import MUST push item models through ItemModelProxy.saveItemModel')**

- Extended only the existing fact `the shipped item models include every required form under their well-known ids`
  (`BuiltInCatalogueIntegrityTests.fs`); no test identity added, so the constructor count stays 1281.
  - It imports the shipped catalogue into the in-memory seam (`BuiltinCatalogue.freshStores ()`:
    `CatalogueImportProxy.createInMemory` plus the default apply).
  - `stores.itemModels.listItemModels ()` must equal `BuiltinCatalogue.itemModels ()`, both sorted by id (the
    store lists by name, then id).
  - `resolveItemModelVersion` of the circular polarizer at version 1 must answer that catalogue item model.
- `CatalogueImportInMemoryTests`' `tryHeldEnvelope` arm is kept: it covers the shared database seam.
- Nothing else changed.

## Files modified

**Within the authorized effects**

- `OpticalConstructor.Domain/LibraryEntryFiles.fs`
- `OpticalConstructor.Domain/CatalogueImport.fs`
- `OpticalConstructor.Storage/SchemaValidation.fs`
- `OpticalConstructor.Storage/LibraryDatabaseStore.fs`: `facetRows` and `sampleMaterialRows` (an item model has
  no rows).
- `OpticalConstructor.Storage/EntryFileJson.fs`
- `OpticalConstructor.Storage/schema/library-item-model.schema.json` (new)
- `OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`: one content item, no comment.
- `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`
- `Catalogue/export-item-models.fsx` (new, throw-away; step 212 deletes it).
- `Catalogue/Builtin/.catalogue-json` and the eleven `Catalogue/Builtin/*.item-model-json` (new).
- `OpticalConstructor.Tests/BuiltinCatalogue.fs`
- `OpticalConstructor.Tests/BuiltInCatalogueIntegrityTests.fs`
- `OpticalConstructor.Tests/LibraryEntryFilesTests.fs`
- `OpticalConstructor.Tests/CatalogueImportProxyTests.fs`

**Outside the listed effects, each forced by this step's own requirements** (see Decisions 3)

- `OpticalConstructor.Domain/KitLibrary.fs`: one exhaustive kind match (FS0025).
- `OpticalConstructor.Storage/PresetDatabaseStore.fs`: one exhaustive kind match (FS0025).
- `OpticalConstructor.Storage/schema/library-catalogue.schema.json`: row `kind` maximum 6 → 7.
- `OpticalConstructor.Tests/CatalogueImportInMemoryTests.fs`: one exhaustive kind match (FS0025).
- `OpticalConstructor.Tests/EntryFileJsonTests.fs`: two fixtures that assumed six kinds.

## Decisions

1. **The `ParameterKind` wire tag lives in `EntryFileJson.fs`, as the slice requires, not in `CanonicalJson.fs`.**
   `CanonicalJson.fs` is outside the effects.
   - On write, the adapter takes the content's canonical node (`nodeOf`, the shared `CanonicalJson.options`) and
     rewrites each detector noise axis's `kind` as its int `value`.
   - On read, it maps each int back through `ParameterKind.tryOfWire` to the node the canonical stack binds
     (`nodeOf kind`), and only then deserializes.
   - An unknown or non-integer tag is `MalformedJsonErr "'<tag>' is not a ParameterKind tag"`. That is the typed
     file-read error the existing `ClosedSetTagConverter` gives every other closed set; nothing throws past
     `parseEntry`.
   - The axes are located by case name (`nameof ItemModelPayload`, then the three detector cases'
     `nameof`). There is no second serializer.
2. **The hash covers the content as its file holds it.**
   - `sealed`, `fileText` and `parseEntry`'s check share one private `fileNode` / `fileHash`, so an entry file is
     still "content + the hash over exactly it" (001 §B).
   - Every other kind's node is unchanged by the adapter, so the 61 existing files and their hashes are identical.
   - The integrity fact used to check `contentHashOf envelope.content`. That differs for a detector item model,
     whose stored tags are ints, so it now checks `envelope.content.sealed`. This is within that fact's
     `change_expectation` effect.
3. **Out-of-effects edits, each required by this step's own text.**
   - **Requirement (2)** says "every match … the compiler now reports incomplete gains the case", and its list is
     "Among them". `KitLibrary.fs` (`overlaidWith`'s decode), `PresetDatabaseStore.fs` (`decodePreset`) and
     `CatalogueImportInMemoryTests.fs` were three more exhaustive matches the build reported (FS0025 is an error).
     - Each product arm answers `Ok None`: an item model is neither a library preset nor a sample.
     - The test arm checks that each imported item model's held envelope decodes back to the catalogue's own item
       model, through `CatalogueImportProxy.tryHeldEnvelope`, which both the in-memory and the database seams
       have. No assertion was removed.
   - **`library-catalogue.schema.json`** capped a manifest row's `kind` at 6, so the manifest holding the eleven
     rows failed validation, and so did every suite that reads `Builtin/`. The acceptance requires the eleven
     listed in the manifest and the whole-catalogue fact to pass, so the maximum is now 7.
   - **`EntryFileJsonTests.fs`** had two fixtures that tag 7 becoming valid breaks:
     - `a schema-invalid catalogue manifest is SchemaValidationErr` used a row of kind 7 as its invalid case. It
       now uses 8; same assertion.
     - `the built-ins form one catalogue with unique file names per kind` built its catalogue from the decoded
       built-ins and asserted every kind is present. It now includes the shipped item models (through
       `ofItemModel`), which keeps that assertion true and adds item models to its seal / manifest round trip.
   - No test identity was added, renamed or removed outside the census.
4. **The new decoder spells out every other payload case instead of `| _ ->`**, honouring "no wildcard is
   added". A future kind will therefore have to decide its arm.
5. **The exporter sets `APP_CONTEXT_BASE_DIRECTORY`.**
   - The brief reads the catalogue through `CatalogueProxy.create … readCatalogue ()`, which validates every file
     against the build-copied schemas. The schema loader resolves `schema/` against `AppContext.BaseDirectory`,
     which under `dotnet fsi` is the SDK's folder.
   - So the script points it at the Tests output, the same folder its `#I` loads. Step 35's writer-only script
     never read a schema.
   - Errors print through each error's `describe`, not `%A` (ELEV002's spirit; scripts are not analyzed).
6. **The noise budget is read once from `BerremanTests.NoiseParam.decentLab`** in the freshly built assembly. Its
   three `.value`s become `ParameterValue`s passed to `ItemParameterAxis.detectorNoiseAxes`; nothing is
   transcribed. The persisted catalogue holds the generated axes: rotation 0.003490658503988659 rad (0.2°),
   element 0.005, throughput 0.01.
7. **The binding descriptions** name the typed binding each one reuses:
   - `SourcePresetBinding`: the preset's λ and tuning, with polarization and intensity scale as parameters.
   - `PolarizerPresetBinding`: the preset's behaviour, with no parameters of its own.
   - `StackSampleBinding`: the sample's stack, with each finite layer's thickness as a parameter.
8. **The shipped-item-models fact reads the detector budget from the catalogue** (step 339 owns parity with the
   bench).
   - It checks the axes are exactly `detectorNoiseAxes` of their own defaults, all positive, and shared by all
     three detector forms.
   - It extends its scope to the file seam the slice requires: the intensity detector's file holds int kind tags
     and reads back equal, and a copy with tag 8 is refused with `MalformedJsonErr` naming `ParameterKind`.

## Testing state

These are development runs (Invariant 6: the gate engine gives the verdict). Logs are in `.artifacts/`.

**Attempt 2 re-runs** (after the one fact extension; the other gates' inputs are unchanged):

- **build:** exit 0, 0 warnings, 0 errors (`0210-build-attempt-02.log`); the test assembly was rebuilt after the
  edit.
- **constructor-unit-tests:** 1281 passed, 0 failed, 0 skipped, unchanged from attempt 1
  (`0210-constructor-unit-tests-attempt-02.log`, `.trx`). The extended fact is `Passed` in the TRX.
- **elevation** (`ASG_ARC_BASE_COMMIT=f395597c…`, `-ExcludeRules ELEV004`): `ELEV-GATE: OK - 0 new finding(s) in
  12 project(s)` (`0210-elevation-attempt-02.log`).
- **LF:** the edited test file has no CR; `--numstat` equals `--ignore-cr-at-eol`.
- ui-smoke and ui-tests were not re-run: the edited file compiles only into `OpticalConstructor.Tests`, so neither
  UI suite's input changed.

**Attempt 1 runs:**

- **build:** `dotnet build Berreman.slnx -c Release`: exit 0, 0 warnings, 0 errors (`0210-build-final.log`).
- **constructor-unit-tests:** 1281 passed, 0 failed, 0 skipped (1279 → 1281, +2).
  - The TRX shows all five named identities `Passed`, the two new ones under `BuiltInCatalogueIntegrityTests` and
    `LibraryEntryFilesTests`.
- **ui-smoke:** 237 passed. It equals checkpoint 201's 237, so it is unchanged.
- **ui-tests (selected `AppContextTests`, `ImportDialogTests`):** 27 passed, 0 failed. Neither class was touched.
- **Extra regression check, outside the gate's selection:** the whole UI suite (no smoke, no kit-acceptance) passed
  655, 0 failed (`0210-ui-tests-whole-1.log`).
- **elevation** (`ASG_ARC_BASE_COMMIT=f395597c…`, `-ExcludeRules ELEV004`): `ELEV-GATE: OK - 0 new finding(s) in
  12 project(s)` (`0210-elevation-1.log`, and `0210-elevation-final.log` on the final tree).
- **The fourteen `invariant-s22-*` gates**, run through ASG's evaluator: every one "INVARIANT HOLDS"
  (`0210-invariants-1.log`). A `git grep --untracked` over the new files finds none of their patterns.
- **LF:** `git diff --numstat` equals `--ignore-cr-at-eol`. A byte check finds no CR and no BOM in any changed or
  new product, test or catalogue file.

## Artifacts

- `0210-build-1.log` … `0210-build-4.log`, `0210-build-final.log`: solution builds.
- `0210-export-item-models.log`: the exporter's single run.
- `0210-builtin-before/`: `Builtin/` as it was before the export.
- `0210-constructor-unit-tests-1.log` … `-3.log`, `-final.log`.
- `0210-ui-smoke-1.log`, `-final.log`; `0210-ui-tests-selected-1.log`, `-final.log`; `0210-ui-tests-whole-1.log`.
- `0210-elevation-1.log`, `0210-elevation-final.log`; `0210-invariants-1.log`.
- `0210-scripts/`: the one-off Python edit scripts used for exact multi-line replacements.
- Attempt 2: `0210-build-attempt-02.log`, `0210-constructor-unit-tests-attempt-02.log` (and `.trx`),
  `0210-elevation-attempt-02.log`.

## Gotchas

- **A catalogue that adds a kind must also raise `library-catalogue.schema.json`'s row `kind` maximum.**
  `writeCatalogue` reads no schema, so the exporter succeeds and only the next read fails.
- **`contentHashOf content` is no longer an entry's hash for a detector item model.**
  - Use `content.sealed` (or `fileText` / `parseEntry`). The hash covers the file's int tags, not the canonical
    stack's case names.
  - For every other kind the two still agree, so existing fixtures that hash with `contentHashOf` keep passing.
- **Under `dotnet fsi`, `AppContext.BaseDirectory` is the SDK folder.** A script that reads library files must set
  `APP_CONTEXT_BASE_DIRECTORY` before the first schema loads (the loads are lazy).
- **The exporter is not idempotent.** A second run reads the eleven back, so `CatalogueData.tryCreate` refuses the
  duplicates with `DuplicateEntryVersionErr`. Step 212 deletes it.
- **`specs/0050/.manifest.state.json` was dirty before this round** (CRLF, the arc-runner's own). It was left
  untouched.
