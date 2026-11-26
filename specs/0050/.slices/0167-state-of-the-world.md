# Step 0167 — IMPLEMENT — state of the world

## Where we are

Spec 0050, S5.1 kit history:

- step 164 added the history model and `KitStar`;
- step 165 declared the `KitHistoryProxy` contract;
- step 166 made `KitStar` an int tag on the canonical JSON stack;
- this step gives the history a file form: canonical JSON, validated by its own schema on read, with another
  version a typed error.

Step 168 builds the file-backed `KitHistoryProxy.create` on `KitHistory.fileName`, `fileText` and
`parseKitHistory`.

## What's working

- Add `schema/kit-history.schema.json`. It closes version 1's body (entries of folder, kitId, name,
  lastOpened, star) and lets other versions reach the typed version check.
- Add `SchemaValidation.validateKitHistory` over the shared `validateWith`, with its schema loaded once after
  the common schema.
- Add `KitHistoryStore`:
  - `KitHistoryText`;
  - `KitHistory.fileName` (`kit-history.json`);
  - `KitHistory.fileText`, which writes canonical JSON;
  - `KitHistoryText.parseKitHistory`, which validates, checks the version and binds.
- Add `KitHistoryJsonTests`:
  - the canonical round trip, the empty history included;
  - `UnsupportedSchemaVersionErr` for version 2;
  - schema refusals and malformed text.

## Tests

As Invariant 6 requires, this worker ran no gates: `gates_run: []`, `gate_results: {}`. The gate engine runs
build, elevation, constructor-unit-tests and the thirteen s19 invariants.

Expected effects:

- constructor-unit-tests rises by exactly 3, from 1,149 to 1,152, through the three declared
  `KitHistoryJsonTests` facts (S51-TEXT). Nothing is removed or renamed.
- ui-smoke is not in this step's roster.

```yaml
gates:
  constructor_unit_tests: 1152
```

## Architecture

- The history file reuses the one versioned-file pipeline from step 124: `nodeOf` → `canonicalFileText` to
  write, and `parseVersionedText` (parse → schema → version → bind) to read. It adds no parser, no converter
  and no up-converter.
- `kit-history.json` is written in one place, `KitHistory.fileName`. The history file's extension is `.json`,
  so a version mismatch reports `fileExtension = .json`.

## Deferred

Nothing.

## Gotchas

- `parseVersionedText` takes a `JsonText`, so the reader passes `this.toJsonText`, a private bridge member, rather
  than the brief's literal `this.value`. This is the same form `KitFileText` and `LibraryFileText` use.
- JsonSchema.Net registers schemas by `$id` process-wide. Load `kit-history.schema.json` only through
  `validateKitHistory`'s `lazy`, never a second time.
- Inside `KitHistoryStore`, `KitHistory` names the Domain record, because the module opens
  `OpticalConstructor.Domain.KitHistory`.

## Changelog

- 2026-11-08 — Step 0167: the kit history's canonical JSON text and its schema-validated, version-checked
  reader, with three new `KitHistoryJsonTests` facts.
