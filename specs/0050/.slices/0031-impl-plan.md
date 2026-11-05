# Step 0031 — IMPLEMENT — impl-plan

## Approach

The catalogue manifest gets its file form, the same way step 30 gave entries theirs (001 §A, §B; .spec-md §5).
The step also proves that every code seed has a file form, ahead of S1.7 deleting the seeds.

1. `schema/library-catalogue.schema.json` has its own `$id` and references `library-common.schema.json`.
   `schemaVersion` is a positive integer, checked unconditionally, and everything else sits under
   `if schemaVersion == 1`, so another version reaches the typed version check. It is a `Content` item copied
   `PreserveNewest`, beside step 30's.
2. `SchemaValidation.fs` gains the schema's file, its lazy and `validateCatalogueManifest` over `validateWith`.
   "Load the common schema first" moves to one `FileName` member that both the kinds and the manifest use.
3. `EntryFileJson.fs` gains `CatalogueManifest.fileText`, `CatalogueManifest.manifestHash` and
   `LibraryFileText.parseCatalogueManifest`. The parse, the throw-to-`MalformedJsonErr` mapping and the
   version comparison in `parseEntry` move to private helpers that both readers call, so nothing is copied.
4. `EntryFileJsonTests.fs` gains the five facts (a)–(e). `assertSchemaInvalid` becomes generic and the JSON
   editing helper is split so the manifest fixtures reuse it.

## Files

- `OpticalConstructor.Storage/EntryFileJson.fs`, `SchemaValidation.fs`, `OpticalConstructor.Storage.fsproj`,
  `schema/library-catalogue.schema.json` (new)
- `OpticalConstructor.Tests/EntryFileJsonTests.fs`

## Risks

- **A seed may not survive the round trip.** Fact (d) is the first time every seed goes through a file. A
  value that does not round-trip, a protection that disagrees with `BuiltInProvenance`, or a name that clashes
  under fact (e) would be a real finding outside this step's authorized edits.
- **JsonSchema.Net resolves `$ref` when a schema is built.** The common schema must be forced first, which the
  shared `FileName` member guarantees.
- **The JSON shape of the manifest.** Its rows carry the int `kind` tag through `ClosedSetTagConverter`, and
  the single-case unions unwrap. The tests pin the keys.
