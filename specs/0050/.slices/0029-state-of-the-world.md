# Step 0029 — IMPLEMENT — state of the world

## Where we are

This is spec 0050, section S1.4: "Library entry files: envelope, provenance, canonical JSON, content
hash, schemas".
- Steps 21–28 built the canonical JSON options, the entry vocabulary, the content hash and the
  int-tagged closed sets.

This step generalises `SchemaValidation` from the one project schema to any build-copied schema (001 §B
"JSON Schema per kind … REUSED"; .spec-md §6 "JSON + schema"). One evaluation body,
`SchemaValidation.validateWith`, now serves the project, groups and environment files. Step 30 adds the
per-kind entry schemas with their files and validates through the same body.

## What's working

- Added `SchemaValidation.validateWith`, the one JSON-Schema evaluation body over any lazily loaded
  schema. The project `validate` calls it.
- Deleted the copies of that body in `GroupsLibrary.validateGroupsLibrary` and
  `UserEnvironment.validateUserEnvironment`. Both now call `validateWith` and keep their names, lazies
  and `$id`s.
- Environment schema errors now keep their nested instance locations, as the groups errors already did.
- Made `collectMessages` private to `SchemaValidation`, because `validateWith` is its only caller.

## Tests

The arc-runner's gate engine is the gate authority. These are local observations:

- `build`: exit 0, `0 Warning(s)`, `0 Error(s)` on the final build (`.artifacts/0029-build-final.log`).
- `constructor-unit-tests`, whole suite: 761/761 passed (`.artifacts/0029-constructor-unit-tests-whole.log`).
  - `RoundTripTests` (3), `GroupsRoundTripTests` (9) and `EnvironmentRoundTripTests` (33) pass
    unchanged.
- `ui-smoke`: 208/208 passed (`.artifacts/0029-ui-smoke.log`).
- `ui-tests`, the selected classes (`AppShellSeamTests`, `DataFilePickerFolderTests`,
  `LocalizationTests`): 26/26 passed (`.artifacts/0029-ui-tests-selected.log`).
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 5 project(s)`, with 0 known findings
  (`.artifacts/0029-elevation-local.log`).
- The six s04 invariants: each grep finds nothing.

```yaml
gates:
  constructor_unit_tests: 761
  ui_smoke_tests:         208
  ui_tests:                26
```

## Architecture

- **One evaluation body, each owner its own schema.** `validateWith` takes the schema as a
  `Lazy<Result<JsonSchema, StorageError>>`. Each owner still loads its schema once, behind its own
  `lazy`, because `JsonSchema.FromText` registers a document's `$id` process-wide and throws on a
  second load.
- **Validation is now identical across the project, groups and environment files:** the same
  `OutputFormat.List` evaluation, the same `results.Details`-recursive message flattening, and the
  same `schema validation failed` fallback.
- `ProjectJson` is untouched; it still calls `SchemaValidation.validate`.

## Deferred

Nothing. Every requirement of the slice landed this round:
- `validateWith` holds the body;
- the project, groups and environment validators call it and keep their names, lazies and `$id`s;
- their docs say so;
- the round-trip facts pass unchanged.

The per-kind entry schemas belong to step 30 by the slice's own words.

## Gotchas

- **The groups and environment fallbacks are now the generic `schema validation failed`.** The
  `groups schema validation failed` and `environment schema validation failed` texts are gone. No test
  asserts them as present; `GroupsRoundTripTests` asserts the groups text is absent.
- **`Report.validateEntryElement` still evaluates on its own.** It reports a fixed
  `document violated the materialEntry $def` message, and `Report.fs` is outside this step's
  authorized edits.
- **`SYSLIB0051` from the vendored MathNet shows up only when MathNet recompiles** (as in steps 20, 23
  and 24). Incremental builds show `0 Warning(s)`.

## Changelog

- 2026-11-05: step 0029 moved the JSON-Schema evaluation body into `SchemaValidation.validateWith`.
  `validate` (project), `GroupsLibrary.validateGroupsLibrary` and
  `UserEnvironment.validateUserEnvironment` call it. The environment validator's nested messages are
  kept, and `collectMessages` is private. Constructor suite 761/761, ui-smoke 208/208, selected
  ui-tests 26/26, ELEV clean.
