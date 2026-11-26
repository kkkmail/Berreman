# Step 0217 — impl plan (IMPLEMENT: the `.item-json` file and its schema)

## Approach

An item persists as its own kit file, `<label>.item-json`, through step 146's JSON stack. No second parser or
writer is written.

1. **`Kit.fs`.** Append after `ItemDraft`, which is after `Item`:
   - the record `ItemFile = { schemaVersion : FileSchemaVersion; item : Item }`, with
     `static member currentSchemaVersion = FileSchemaVersion 1`;
   - `type Item with member this.itemFile : ItemFile`.

   This mirrors `ExperimentFile` / `Experiment.experimentFile` (Experiments.fs). No member converts the other way
   (ELEV006): a file is read back through its `item` field.
2. **`schema/item.schema.json`.** A copy of the shape of `experiment.schema.json` (draft 2020-12, `$id`
   `https://berreman/schema/item.json`).
   - `schemaVersion` is checked unconditionally against the common `positiveVersion`.
   - Only under `if` it is 1 (`then`), the root holds exactly `schemaVersion` and `item`, and `item` holds exactly
     `id`, `label`, `model`, `parameters`, `dial`, `photoOpt`, `notes`, `revision` and `history`.
   - `model` / `dial` are `true`, and `parameters` / `history` are arrays. The serializer binds them, so a
     malformed one is `MalformedJsonErr`.
   - The file is added as a `Content` item, `PreserveNewest`, beside `experiment.schema.json` in the Storage fsproj.
3. **`SchemaValidation.fs`.** Add `validateItemFile` over `validateWith`. Its schema is loaded once behind a `lazy`
   through `tryLoadLibrarySchema`, which loads the common schema first, exactly as `validateExperimentFile` does.
4. **`KitContentFiles.fs`.**
   - `type ItemFile with member this.fileText` renders `nodeOf this` through `canonicalFileText`.
   - `type KitFileText with member this.parseItemFile` calls
     `parseVersionedText validateItemFile ItemKitFile.extension ItemFile.currentSchemaVersion`. It passes the
     existing private `toContentJsonText`, because `parseVersionedText` takes a `JsonText`.
5. **Tests in `KitContentFileJsonTests`.** Add the four declared facts. The items are built only through step 216's
   `ItemDraft`, so no fixture constructs a bare preset or sample instance:
   - CPL1 over `LibraryModelChoice circularPolarizer`, edited to revision 2, with an unknown-under-prior
     retardance, a calibrated fast-axis offset, the revision-1 parameters in its history, a photo and notes;
   - the laser over the source preset HeNe 633;
   - LP1 over the ideal-polarizer preset;
   - a stack item over the 1 mm glass plate.

   All of them round-trip with their exact model version and typed binding.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/item.schema.json` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/SchemaValidation.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitContentFiles.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitContentFileJsonTests.fs`

## Risks

- **Record-label shadowing.** `ItemFile` repeats `schemaVersion` (`KitFile`) and adds `item`. It is appended last,
  so only later files can see the shadowing, and the build will show whether any unannotated record expression
  there breaks.
- **`JsonSchema.FromText` registers each `$id` globally.** A new `$id` that is loaded once behind a `lazy` is safe.
- **The serializer must reject each malformed body that the schema lets through.** The tests check that it does.
