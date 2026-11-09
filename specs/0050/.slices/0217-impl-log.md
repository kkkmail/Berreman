# Step 0217 — impl log (IMPLEMENT: the `.item-json` file and its schema)

## Progress

- [x] Read the protocol (arc-runner + implement_worker system prompts), the project prompt, AGENTS.md, the slice and its
  `.gates` snapshot, and steps 0215/0216's SoW. No operator note was in flight.
- [x] `Kit.fs`: added `ItemFile` (`schemaVersion`, `item`, `currentSchemaVersion = FileSchemaVersion 1`), appended
  after `ItemDraft` (and therefore after `Item`), and `type Item with member this.itemFile`. Mirrors
  `ExperimentFile` / `Experiment.experimentFile`. There is no `ItemFile → Item` member; a file is read back
  through its `item` field (ELEV006).
- [x] `schema/item.schema.json` (new), draft 2020-12, `$id` `https://berreman/schema/item.json`, in the shape of
  `experiment.schema.json` (step 146):
  - `schemaVersion` is checked unconditionally against the common `positiveVersion`.
  - Under `if` 1 / `then`, the root holds exactly `schemaVersion` + `item`, and `item` holds exactly `id` (common
    `guid` by `$id`), `label` (non-empty string), `model` (true), `parameters` (array), `dial` (true), `photoOpt`
    (`null` or non-empty string), `notes` (string), `revision` (integer ≥ 1) and `history` (array).
  - Added as a `Content` item, `PreserveNewest`, right after `schema/experiment.schema.json` in
    `OpticalConstructor.Storage.fsproj`, with no comment.
- [x] `SchemaValidation.fs`: `validateItemFile` over `validateWith`. Its schema sits behind a private `lazy` that
  goes through `tryLoadLibrarySchema`, which loads the common schema first, exactly as
  `validateExperimentFile` / `validateSceneFile` do. It is placed after `validateSceneFile`.
- [x] `KitContentFiles.fs`:
  - `type ItemFile with member this.fileText` renders `nodeOf this` through `canonicalFileText`;
  - `type KitFileText with member this.parseItemFile` is
    `parseVersionedText SchemaValidation.validateItemFile ItemKitFile.extension ItemFile.currentSchemaVersion`.
- [x] `KitContentFileJsonTests`: added the four declared facts and their fixtures. The items are built only
  through step 216's `ItemDraft.addTo []`:
  - CPL1, `LibraryModelChoice` circular polarizer, `DialMounted DialPositive`, then edited to:
    - revision 2;
    - `c2.retardance` unknown under a prior;
    - `c2.fastAxisOffset` calibrated;
    - the revision-1 parameters as history;
    - photo `data/photos/CPL1.jpg` and notes.
  - the laser, `SourcePresetChoice` over `PresetIds.heNe633`, `FixedMounting`;
  - LP1, `PolarizerPresetChoice` ideal polarizer over `PresetIds.polarizerLp`, `DialMounted DialNegative`;
  - Plate1, `StackSampleChoice` over the 1 mm glass plate v1, `FixedMounting`.
- [x] Development runs: build, class tests, mutations, full constructor suite, invariants, elevation (below).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs` (+19)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/item.schema.json` (new, 43 lines)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/SchemaValidation.fs` (+7)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitContentFiles.fs` (+10)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj` (+3)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitContentFileJsonTests.fs` (+1 open, four facts and
  fixtures)

Every one is in the slice's authorized `edits`. All are LF only (checked byte by byte), and
`git diff --numstat` equals `--ignore-cr-at-eol`.

## Decisions

- **`parseItemFile` passes the existing private `this.toContentJsonText`, not `this.value`.** The brief writes
  `... ItemFile.currentSchemaVersion this.value`, but `parseVersionedText` takes a `JsonText`, and the experiment and
  setup readers beside it pass `toContentJsonText`. That is the same text with no re-wrap at the call site.
- **`label` is `{ "type": "string", "minLength": 1 }`, as the brief says ("a non-empty string").** The stricter
  `pattern: "\\S"` that `setup.schema.json` uses for a scene name was considered and not taken, because the brief
  names the rule exactly.
- **`revision` is an inline `{ "type": "integer", "minimum": 1 }`.** It is not the common `positiveVersion`, whose
  description says it is an entry or schema version; an item revision is neither.
- **`ItemFile` is appended at the end of `Kit` (after `ItemDraft`), not between `Item` and `ItemModelChoice`.** It
  is still after `Item`, as the brief asks, and step 216's ordering is untouched.
- **`fileText` repeats the existing one-line composition** (`nodeOf this |> Result.map (… canonicalFileText |>
  KitFileText)`). This is how `ExperimentFile`, `SceneFile` and both `KitFolderStore` files render. The brief
  prescribes exactly this composition of the shared stack. Factoring a helper across `KitFolderStore` would go
  outside this step's authorized edits.
- **The tests go beyond the brief's minimum.** They:
  - round-trip a source, an ideal-polarizer and a stack item beside CPL1 and the laser, each checking its exact
    model version and its typed binding (`draft.model.instance`, `libraryReferences`), as the brief's "MUST cover
    source, ideal-polarizer and stack items" asks;
  - refuse every missing `item` key and several more invalid shapes;
  - check that the serializer answers `MalformedJsonErr` for a malformed model, parameter, state, dial and history
    record, which the schema leaves to it.

  No fixture constructs a bare `PresetInstance` / `SampleInstance`.

## Testing state

Development runs only. The arc-runner's gate engine gives the verdict (Invariant 6).

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`):
  - first build: exit 0, 0 errors. Its only warnings were the two pre-existing `SYSLIB0051` in the vendored
    MathNet `Numerics` (step 216's SoW records them).
  - final build: 0 warnings, 0 errors.

  Logs: `0217-dev-build-01.log`, `0217-dev-build-02.log`.
- **KitContentFileJsonTests** (class filter): 13 passed (9 + 4). All four declared identities were listed and pass.
  Log: `0217-dev-class-tests-01.log`.
- **The facts bite.** Logs are under `.artifacts/0217-mutations/`.
  - Schema mutations, made on the runtime copy and then restored byte for byte, are each caught:
    - no root `additionalProperties: false`;
    - no `label` `minLength`;
    - no `revision` `minimum`;
    - the body validated unconditionally (caught by both the version and the schema facts);
    - `id` any string;
    - `photoOpt` anything.
  - Code mutations, rebuilt and then restored byte for byte (`cmp`), are each caught:
    - `parseItemFile` under `ExperimentKitFile.extension` (caught by the version fact);
    - `parseItemFile` validated against the experiment schema (caught by the round-trip and schema facts).
- **constructor-unit-tests** (the gate's command, whole suite): 1304 passed, 0 failed, 0 skipped. That is 1300 + 4,
  in 69 s against a 180 s estimate. Log: `0217-dev-constructor-unit-tests.log`.
- **The fourteen `invariant-s22-*`:** each holds (`git grep` finds nothing). Log: `0217-dev-invariants.log`.
- **elevation** (`ASG_ARC_BASE_COMMIT=f395597…`, `-ExcludeRules ELEV004`): `ELEV-GATE: OK - 0 new finding(s) in
  12 project(s)`, 189 s against a 600 s deadline. The arc's `.slnx` change widens the scope to all 12 projects.
  Domain and Storage each show `findings=0`. Log: `0217-dev-elevation.log`.
- **After elevation, the test's two `%A` failure messages were replaced with plain text** (AGENTS.md: never render
  a DU with `%A`). The checks that followed:
  - rebuild: 0 warnings, 0 errors (`0217-dev-build-03.log`);
  - whole constructor suite again: 1304 passed, 0 failed, 0 skipped, in 67 s (`0217-dev-constructor-unit-tests.log`,
    overwritten with this final run).

## Artifacts

All under `specs/0050/.artifacts/`:

- `0217-dev-build-01.log`
- `0217-dev-build-02.log`
- `0217-dev-class-tests-01.log`
- `0217-dev-constructor-unit-tests.log`
- `0217-dev-invariants.log`
- `0217-dev-elevation.log`
- `0217-mutations/`, which holds:
  - `mutate_schema.py`
  - `schema-mutations.log`
  - `code-mutations.log`
  - `KitContentFiles.fs.orig`

## Gotchas

- **`ItemFile` is now the latest record in `Kit` with `schemaVersion`** (after `KitFile`) **and the only one with
  `item`.** An unannotated `{ schemaVersion = …; … }` written after `Kit` is opened infers `ItemFile`, so annotate
  it. Every existing construction already does, and the build is green.
- **`JsonSchema.FromText` registers each `$id` process-wide.** `item.schema.json` is loaded once, behind
  `itemFileSchema`'s `lazy`. Never load it a second time.
- **Mutation testing in the bin folder leaves the runtime copy newer than its source.** `PreserveNewest` will then
  not re-copy it, so restore it by copying the source over it, as this round did.
