# Step 0167 — IMPLEMENT — impl-plan

## Approach

The kit history file becomes canonical JSON, validated against its own schema on read. It reuses step
124's stack (`nodeOf`, `canonicalFileText`, `parseVersionedText`, `validateWith`) and adds no second
parser and no new converter. Step 166 already writes `KitStar` as its int tag.

1. `schema/kit-history.schema.json` (draft 2020-12, `$id` `https://berreman/schema/kit-history.json`),
   shaped like `kit.schema.json`:
   - `schemaVersion` is checked as a positive version unconditionally.
   - Only under `if schemaVersion = 1` (`then`) is the body closed. It holds `schemaVersion` and
     `entries`, and each entry holds exactly `folder`, `kitId` (the common `guid`), `name`
     (pattern `\S`), `lastOpened` and `star` (integer 1 or 2).
   - A file of another version therefore reaches the typed version check.
2. `OpticalConstructor.Storage.fsproj`:
   - a `Content` item for the schema, right after `kit.schema.json`'s item;
   - one `<Compile Include="KitHistoryStore.fs" />` right after `KitContentFiles.fs`;
   - no comments.
3. `SchemaValidation.fs`: `validateKitHistory` over `validateWith`. Its schema sits behind a `lazy`
   that loads the common schema first (`tryLoadLibrarySchema`), as `validateKitFile` does.
4. New `KitHistoryStore.fs`:
   - `KitHistoryText of string` with `.value`;
   - `type KitHistory with static member fileName` and `member this.fileText`;
   - `type KitHistoryText with member this.parseKitHistory` over `parseVersionedText`, with
     `FileExtension ".json"`. The private `toJsonText` bridge follows the existing `KitFileText` and
     `LibraryFileText` pattern.
5. New `OpticalConstructor.Tests/KitHistoryJsonTests.fs` (`module KitHistoryJsonTests`), appended at
   the end of the compile list. It holds the three declared facts, modelled on `KitFileJsonTests`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/kit-history.schema.json` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/SchemaValidation.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitHistoryStore.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryJsonTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- Invariant 6 means no local build or test run, so every expression copies a form that already
  compiles in the tree (`KitFileJsonTests`, `KitFolderStore`, `KitContentFiles`).
- Name collisions under ELEV014/015/016/019: `KitHistoryText`, `KitHistoryStore`,
  `validateKitHistory`, `kitHistorySchema*` and `parseKitHistory` were each grepped and are unused.
  `fileText` and `fileName` are new on the `KitHistory` receiver.
- The schema's `$ref` to `library-common.json` resolves only if the common schema is loaded first.
  `tryLoadLibrarySchema` guarantees that.
