# Step 0167 — IMPLEMENT — impl-log

## Progress

- [x] Schema `schema/kit-history.schema.json`.
- [x] Storage fsproj: Content item and Compile line.
- [x] `SchemaValidation.validateKitHistory`.
- [x] `KitHistoryStore.fs`: `KitHistoryText`, `KitHistory.fileName` / `fileText`, `KitHistoryText.parseKitHistory`.
- [x] `KitHistoryJsonTests.fs` (three facts) and its compile line.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/kit-history.schema.json` (new)
  - Draft 2020-12, `$id` `https://berreman/schema/kit-history.json`, shaped like `kit.schema.json` (step 124).
  - `schemaVersion` is required and checked unconditionally against the common `positiveVersion`.
  - The body is closed only under `if schemaVersion = 1` / `then`:
    - top level: `schemaVersion` and `entries`, both required, `additionalProperties: false`;
    - each entry: exactly `folder` (non-empty string), `kitId` (the common `guid`, by `$id`), `name` (pattern
      `\S`), `lastOpened` (string) and `star` (integer, enum 1 or 2), all required and closed.
  - A file of another version therefore passes the schema and reaches the typed version check.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`
  - `<Compile Include="KitHistoryStore.fs" />`, directly after `KitContentFiles.fs`.
  - A `Content` item for `schema\kit-history.schema.json` (`PreserveNewest`), directly after `kit.schema.json`'s.
  - No comments.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/SchemaValidation.fs`
  - Private `kitHistorySchemaFile` and `kitHistorySchema`. The schema is loaded once behind a `lazy`, through
    `tryLoadLibrarySchema`, so the common schema loads first, as `validateKitFile` does.
  - `validateKitHistory (element : JsonElement) : Result<unit, StorageError>` over `validateWith`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitHistoryStore.fs` (new)
  - `module KitHistoryStore` in `namespace OpticalConstructor.Storage`; it opens
    `OpticalConstructor.Domain.KitHistory`.
  - `KitHistoryText of string` with `.value`.
  - `type KitHistory with`:
    - `static member fileName : FileName = FileName "kit-history.json"`;
    - `member this.fileText`, which is `nodeOf this` rendered by `canonicalFileText`.
  - `type KitHistoryText with`:
    - a private `toJsonText` bridge, as `KitFileText` and `LibraryFileText` carry;
    - `member this.parseKitHistory` = `parseVersionedText SchemaValidation.validateKitHistory (FileExtension ".json")
      KitHistory.currentSchemaVersion this.toJsonText`.
  - Each doc is one to three lines citing spec 0050 S5.1.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryJsonTests.fs` (new, `module KitHistoryJsonTests`).
  The fixture holds two entries, each with a fixed GUID:
  - first, the starred `C:/kits/Optics QZ — µ` named `Optics QZ — µ`, opened at
    `2026-11-03T12:00:00Z` + 1234567 ticks;
  - then the unstarred `D:/lab/Bench 2` named `Bench 2`, opened an hour earlier.

  The three facts:
  - `a kit history round-trips through its canonical text`:
    - the parsed history equals the original;
    - the text has no `\r` and ends with exactly one `\n`;
    - top-level keys are `entries`, `schemaVersion`, and each entry's are `folder`, `kitId`, `lastOpened`,
      `name`, `star`;
    - `µ` and `—` are unescaped (no `\u` anywhere);
    - the first entry's `star` is written as `1` and its `lastOpened` as `"2026-11-03T12:00:00.1234567Z"`;
    - rewriting the parsed history gives the same text;
    - `KitHistory.empty` round-trips too, and so does its text.
  - `a kit history of another schema version is UnsupportedSchemaVersionErr`: both the body with
    `schemaVersion` 2 and `{ "schemaVersion": 2 }` alone answer
    `UnsupportedSchemaVersionErr { fileExtension = FileExtension ".json"; found = 2; expected = 1 }`.
  - `a schema-invalid or malformed kit history is refused before it binds`: each of these answers
    `SchemaValidationErr` with non-blank messages, and `{` answers `MalformedJsonErr`:
    - an entry without `folder`;
    - an entry with an extra `colour`;
    - `star` 3;
    - `kitId` `not-a-guid`;
    - `name` `"   "`;
    - a top-level `recent`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
  - `<Compile Include="KitHistoryJsonTests.fs" />` appended at the end of the compile list, with no comment.

## Decisions

- **`this.value` versus `JsonText`.** The brief writes the last argument as `this.value`, but
  `parseVersionedText` takes a `JsonText`, not a `string`. I bridged it with a private member,
  `toJsonText : JsonText = JsonText this.value`, on `KitHistoryText`. That is the named-member form AGENTS.md
  prescribes for a cross-type reuse, and the existing readers already use it: `KitFileText.toJsonText`,
  `KitFileText.toContentJsonText` and `LibraryFileText.toJsonText`. It adds no parser.
- **Placement.** `toJsonText` sits in the same `type KitHistoryText with` block as `parseKitHistory`, as
  `KitFolderStore` places its private bridge beside its reader. The brief orders the declarations as
  (a) the text type, (b) the `KitHistory` extension, (c) the text's reader, and the file follows that order.
  A non-adjacent intrinsic augmentation in the same module has a precedent: `KitFolder` in `Domain/Kit.fs`.
- **No second validator body.** `validateKitHistory` passes its own lazy schema to the shared `validateWith`.
- **The schema's description.** It is JSON content, worded as `kit.schema.json`'s, not a project-file comment.
- **Fixture GUIDs.** They are written lower case, as the serializer writes them and the common `guid` pattern
  requires.
- **No `KitHistory.extension` member.** `FileExtension ".json"` is written inline, as the brief specifies.

## Testing state

As Invariant 6 requires, this IMPLEMENT worker ran no gates: no `dotnet build`, no `dotnet test`, no analyzer and
no invariant. After exit the gate engine runs:

- build;
- elevation;
- constructor-unit-tests (all discovered);
- the thirteen s19 invariants.

The only commands I ran were read-only (reads, greps, `git status`, `git diff --numstat` with and without
`--ignore-cr-at-eol`, `file`), plus one scripted text edit of the new test file.

Because nothing compiled locally, every expression copies a form that already compiles in the tree:

- `KitFileJsonTests` for the JSON-node edits, the asserts and the error matches;
- `EntryFileJsonTests` for `node.["entries"].[0].AsObject()`;
- `KitFolderStore` and `KitContentFiles` for `fileText` and the `parseVersionedText` reader.

Expected: constructor-unit-tests rises by exactly 3, from 1,149 (checkpoint 165, unchanged by step 166) to 1,152.
No test was removed or renamed.

## Artifacts

None. The step needs no captured logs.

## Gotchas

- `specs/0050/.manifest.state.json` was already modified when the round started. It is the supervisor's file, and
  I left it alone.
- Line endings: every new file is LF with no `\r`. The edited files' `git diff --numstat` matches
  `--ignore-cr-at-eol` (4/0, 7/0, 1/0).
- `KitHistoryStore.fs` opens `OpticalConstructor.Domain.KitHistory`, so `KitHistory` there names the record type,
  not the module. Code added later must not reuse that name.
- JsonSchema.Net registers a schema by `$id` process-wide and throws on a second load. The history schema has its
  own `$id` and loads once, behind its `lazy`.
