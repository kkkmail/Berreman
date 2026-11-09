# Step 0217 — state of the world (IMPLEMENT: the `.item-json` file and its schema)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), is landing the shared item
contracts. They must exist before S7, so that copying an experiment can map its items (001 §H, §S6; S7.1).

- Steps 213–216 added the item and `AddItem`:
  - its identity and exact model version;
  - its parameters, dial and revisions;
  - `ItemDraft`.
- This step stores an item as its own kit file, `<label>.item-json`. The file is canonical JSON, validated against
  its own versioned schema, and reuses step 146's stack.
- Step 218 reads and writes a kit's items through `KitProxy`.
- Step 219 records an item through `recordItem` (snapshot, pin, then write).

## What's working

- Add `Kit.ItemFile` (`schemaVersion`, `item`, current version 1) and `Item.itemFile`. A file is read back through
  its `item`.
- Add `schema/item.schema.json`, build-copied beside `experiment.schema.json`.
  - `schemaVersion` is checked unconditionally.
  - Version 1's root and `item` keys are closed.
  - `id` is a GUID, `label` a non-empty string, `photoOpt` null or a non-empty string, and `revision` ≥ 1.
- Add `SchemaValidation.validateItemFile`. Its schema is loaded once, after the common schema.
- Add `ItemFile.fileText` (canonical LF JSON with one final newline) and `KitFileText.parseItemFile`. The parse
  validates first, then answers `UnsupportedSchemaVersionErr` for another version and `MalformedJsonErr` for a body
  the serializer cannot bind.
- Add four `KitContentFileJsonTests` facts. They cover a standalone model's item, a source item, an ideal-polarizer
  item and a stack item. Constructor unit tests: 1300 → 1304.

## Tests

These are development runs, recorded in `0217-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 errors. The final build had 0 warnings.
- **constructor-unit-tests:** 1304 passed, 0 failed, 0 skipped (+4), in 69 s against a 180 s estimate. All four
  declared identities pass:
  - `an item is stored under its label`;
  - `an item file round-trips through its canonical text`;
  - `an item file of another schema version is UnsupportedSchemaVersionErr`;
  - `a schema-invalid or malformed item file is refused before it binds`.
- **The facts bite.** Six schema mutations and two code mutations were each caught. The schema and the source were
  then restored byte for byte.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded): `ELEV-GATE: OK - 0 new finding(s) in 12
  project(s)`, 189 s against a 600 s deadline. The arc's `.slnx` change widens it to all 12 projects.
- **The fourteen `invariant-s22-*` gates:** each holds.
- The UI suites are not in this step's roster. No UI file or shared fixture changed.

```yaml
gates:
  constructor_unit_tests: 1304
```

## Architecture

- **The item file is a Domain record with a Storage rendering.** This is the `ExperimentFile` / `SceneFile` pattern
  of step 146:
  - `Kit.ItemFile` is pure data;
  - `KitContentFiles` adds `fileText` / `parseItemFile` over the shared `nodeOf`, `canonicalFileText` and
    `parseVersionedText`;
  - no second parser or writer exists, and there are no up-converters or fallback readers.
- **The schema validates structure, and the serializer validates meaning.**
  - The schema closes the root and the item's keys and checks every scalar.
  - `model`, `dial`, the parameter list and the history are left to the serializer, so a malformed one is
    `MalformedJsonErr`.
  - Only version 1's body is validated, so another positive version reaches the typed version check before
    binding.
- **The file is named by the item's label.** The name is `ItemKitFile.fileNameOf label.fileStem`, the shared name
  rule (001 §A, "a file that exists many times is named by its own identity").

## Deferred

Nothing in this step's scope. By the spec's plan:

- the kit's item read and write through `KitProxy` is step 218;
- `recordItem`, the reopen-against-an-empty-library resolution and its typed missing-model error are step 219.

None of these is deferred by this step.

## Gotchas

- **`ItemFile` is now the latest record in `Kit` with `schemaVersion`, and the only one with `item`.** Annotate any
  later record expression with those labels.
- **`JsonSchema.FromText` registers `$id`s process-wide.** `item.schema.json` is loaded once, behind its `lazy`.
- **`parseItemFile` passes the private `toContentJsonText`, as its siblings do.** The brief's `this.value` would
  not type-check against `parseVersionedText`'s `JsonText`.
- **The test fixtures (`cpl1`, `laserDraft`, `lp1Draft`, `plateDraft`, `itemsWithDrafts`) are private to
  `KitContentFileJsonTests`.** They are built only through `ItemDraft.addTo []`.

## Changelog

- 2026-11-09 — step 0217: added `Kit.ItemFile` / `Item.itemFile`, `schema/item.schema.json`,
  `SchemaValidation.validateItemFile`, `ItemFile.fileText` and `KitFileText.parseItemFile`; added four
  `KitContentFileJsonTests` facts; constructor unit tests 1300 → 1304.
