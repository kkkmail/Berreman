# Step 0030 — IMPLEMENT — state of the world

## Where we are

This is spec 0050, section S1.4: "Library entry files: envelope, provenance, canonical JSON, content
hash, schemas".
- Steps 21–28 built the canonical JSON options, the entry vocabulary (kinds, ids, provenance, content,
  envelope, manifest), the content hash and the int-tagged closed sets.
- Step 29 made `SchemaValidation.validateWith` the one schema-evaluation body.

This step lands the entry files themselves (001 §B: "One entry per file", "Every entry file carries",
"JSON Schema per kind", "No up-converters"). An entry's content is sealed with its hash, written as
indented canonical JSON, and read back through its kind's schema, the schema-version check and the hash
check, each failure a typed `StorageError`. The catalogue manifest's file and hash come next (step 31).

## What's working

- Added the `StorageError` cases `UnsupportedSchemaVersionErr` (`SchemaVersionMismatchInfo`, keyed by file
  extension so kit files can reuse it) and `ContentHashMismatchErr` (`ContentHashMismatchInfo`).
- Added draft 2020-12 JSON Schemas for the six library-entry kinds over one shared
  `library-common.schema.json`. Each kind's body is checked only at schema version 1, so another version
  reaches the typed version check.
- Added `SchemaValidation.validateEntry`, one schema per kind, each loaded once after the common schema.
- Added `EntryFileJson`: `LibraryEntryContent.sealed`, `LibraryEntryEnvelope.fileText` (indented
  canonical JSON, one final `\n`) and `LibraryFileText.parseEntry`. The reader checks parse, schema,
  version, binding and hash in that order.
- Added `EntryFileJsonTests`, ten facts:
  - every kind, complexity shape and provenance round-trips;
  - the file is canonical;
  - a version mismatch, a tampered file, a schema violation, a wrong-kind file and non-JSON text each
    answer their typed error.

## Tests

The arc-runner's gate engine is the gate authority. These are local observations:

- `build`: exit 0, `0 Warning(s)`, `0 Error(s)` (`.artifacts/0030-build-final.log`).
- `constructor-unit-tests`, whole suite: 771/771 passed (761 before plus the 10 `EntryFileJsonTests`
  facts) (`.artifacts/0030-constructor-unit-tests-whole.log`).
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 5 project(s)`, 0 known findings
  (`.artifacts/0030-elevation-local.log`).
- The six s04 invariants: each grep finds nothing (`.artifacts/0030-invariants.log`). The new untracked
  files were grepped directly too.
- EOL: every changed or new file is LF only, with no BOM.

```yaml
gates:
  constructor_unit_tests: 771
```

## Architecture

- **The file is flat and the record keeps the two apart.**
  - The file holds the content's keys with `contentHash` beside them.
  - `LibraryEntryEnvelope` keeps `content` and `contentHash` separate, so the hash covers exactly the
    content.
  - The writer adds the key, and the reader removes it before binding. Both name it by the envelope's
    field (`nameof`), so it cannot drift.
- **Every version passes the header check; only version 1 gets the body check.** Each kind schema always
  checks `schema` = `{ kind: <tag>, version: <positive int> }`. Everything else sits under
  `if schema.version == 1 then …`. A future version's file therefore passes the schema, and the reader
  rejects it as `UnsupportedSchemaVersionErr` before anything binds. There are no up-converters and no
  fallback readers.
- **The schema pins the envelope and the serializer pins the physics.** The schemas pin the envelope, the
  provenance shapes and each payload's case tag and record keys. The physics inside a payload is bound by
  `CanonicalJson.options`, so malformed physics is `MalformedJsonErr`.
- **Schema loading is ordered.** The common schema is loaded once and forced before each kind's schema is
  built, because JsonSchema.Net resolves a cross-document `$ref` by the registered `$id`. Each kind's
  schema is also loaded once, because `FromText` registers an `$id` only once.
- **`LibraryFileText` is the text S2 and S4 will hold byte for byte.** S2 stores it as the database
  payload and S4 as the kit snapshot. This step does no file IO: it renders and parses text.

## Deferred

Nothing. Every requirement of the slice landed this round:
- the two error cases and their records;
- the seven schemas and their content items;
- `schemaFile`, the lazies and `validateEntry`;
- `LibraryFileText`, `sealed`, `fileText` and `parseEntry`;
- the ten facts (a)–(j).

The manifest's own file and hash are step 31's by the slice plan.

## Gotchas

- **`sealed` is an F#-reserved identifier.** It is declared and called as ``` ``sealed`` ```, because a
  plain `sealed` raises `warning FS0046` everywhere it appears.
- **The schemas mirror FSharp.SystemTextJson's AdjacentTag union form** (`{ "Case", "Fields": [...] }`,
  fieldless cases as names, options as value-or-`null`). Changing the union encoding of
  `CanonicalJson.options` would break them, which is intended, and would also break the frozen-hash fact.
- **`git grep` invariants skip untracked files** until the supervisor stages them. This round grepped the
  new files directly.
- **A single-project build of Storage recompiles the vendored MathNet** and shows its two `SYSLIB0051`
  warnings. The solution build the gate runs shows `0 Warning(s)`.

## Changelog

- 2026-11-05: step 0030 landed the library-entry files: two `StorageError` cases, seven per-kind and
  common JSON Schemas, `SchemaValidation.validateEntry`, and `EntryFileJson` (`sealed`, `fileText`,
  `parseEntry`) with 10 `EntryFileJsonTests` facts. Constructor suite 771/771, ELEV clean.
