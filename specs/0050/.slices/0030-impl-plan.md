# Step 0030 — IMPLEMENT — impl-plan

## Approach

One library entry per file. The writer seals a `LibraryEntryContent` with its content hash, then renders it
as indented canonical JSON with `contentHash` beside the content's keys. The reader parses the text, validates
it against the kind's JSON Schema, checks the schema version, binds the content and checks the hash, in that
order. Each failure is a typed `StorageError` (001 §B; .spec-md §5).

1. `Errors.fs`: `SchemaVersionMismatchInfo` and `ContentHashMismatchInfo` records go above `StorageError`.
   `StorageError` gains `UnsupportedSchemaVersionErr` and `ContentHashMismatchErr`, and its doc list names
   both. No match on `StorageError` is exhaustive, so no consumer changes.
2. Seven draft 2020-12 schemas go under `schema/`. `library-common.schema.json` holds the shared `$defs`:
   GUID, positive version, content hash and the four provenance shapes. One schema per kind references it by
   `$id`. Each checks `schema` (kind tag const, positive int version) unconditionally, and checks everything
   else only under `if schema.version == 1`. Each is a `Content` item copied `PreserveNewest` beside the
   existing schema items.
3. `SchemaValidation.fs`: `LibraryEntryKind.schemaFile`, one lazy for the common schema, one lazy per kind
   that forces the common one first, and `validateEntry` over `validateWith`.
4. New `EntryFileJson.fs` (`module EntryFileJson`) after `CanonicalJson.fs`:
   - `LibraryFileText`;
   - `LibraryEntryContent.sealed`;
   - `LibraryEntryEnvelope.fileText`;
   - `LibraryFileText.parseEntry`.
5. New `EntryFileJsonTests.fs` holds the ten facts (a)–(j). It is appended to the test compile list after
   `LibraryEntryFilesTests.fs`.

## Files

- `OpticalConstructor.Storage/Errors.fs`, `SchemaValidation.fs`, `EntryFileJson.fs` (new),
  `OpticalConstructor.Storage.fsproj`, `schema/library-*.schema.json` (7 new)
- `OpticalConstructor.Tests/EntryFileJsonTests.fs` (new), `OpticalConstructor.Tests.fsproj`

## Risks

- **The real JSON shapes of FSharp.SystemTextJson.** Union encoding (`Case` / `Fields`) and option nulls
  decide what the schemas must accept. Mitigation: dump each kind's real node before writing the schemas.
- **JsonSchema.Net 9 resolves `$ref` at build time.** Mitigation: force the common schema's lazy before any
  entry schema is built.
- **Mueller matrix equality after a round trip.** MathNet storage equality is checked by the
  polarizer fact.
- **ELEV.** `LibraryFileText` is a single-case union over `string` with `.value` and members. The parse and
  render bodies sit at the IO seam: members on our types, so no free function takes a bare primitive.
