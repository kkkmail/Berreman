# Step 0030 — IMPLEMENT — impl-log

## Progress

- [x] Dumped the real canonical JSON of every entry kind and provenance case before writing the schemas
  (`.artifacts/0030-json-shapes.log`).
- [x] `Errors.fs`: `SchemaVersionMismatchInfo`, `ContentHashMismatchInfo`, and the `StorageError` cases
  `UnsupportedSchemaVersionErr` and `ContentHashMismatchErr`; the doc list names both.
- [x] The seven `schema/library-*.schema.json` files and their `Content` items (`PreserveNewest`, no
  new comment).
- [x] `SchemaValidation.fs`: `LibraryEntryKind.schemaFile`, the common lazy, one lazy per kind and
  `validateEntry` over `validateWith`.
- [x] `EntryFileJson.fs` (`module EntryFileJson`, compiled directly after `CanonicalJson.fs`):
  `LibraryFileText`, `LibraryEntryContent.sealed`, `LibraryEntryEnvelope.fileText` and
  `LibraryFileText.parseEntry`.
- [x] `EntryFileJsonTests.fs` (`module EntryFileJsonTests`, appended after `LibraryEntryFilesTests.fs`):
  facts (a)–(j).
- [x] Probed the schemas with 22 hand-edited documents (`.artifacts/0030-schema-probe.log`).
- [x] Fixed the one ELEV003P the first gate run found (see Decisions).
- [x] Local runs: build, the new class, the whole constructor suite, elevation, the six s04 invariants
  and the EOL check.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/Errors.fs`
  - `open OpticalConstructor.Domain.LibraryEntryFiles`.
  - `SchemaVersionMismatchInfo = { fileExtension : FileExtension; found : FileSchemaVersion; expected : FileSchemaVersion }`.
  - `ContentHashMismatchInfo = { entryId : LibraryEntryId; recorded : ContentHash; computed : ContentHash }`.
  - `StorageError` gains `UnsupportedSchemaVersionErr of SchemaVersionMismatchInfo` and
    `ContentHashMismatchErr of ContentHashMismatchInfo`.
  - No match on `StorageError` was exhaustive: the build is green under `--warnaserror+:25`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/SchemaValidation.fs`
  - `libraryCommonSchemaFile` and `libraryCommonSchema` (lazy) load `schema/library-common.schema.json`.
  - `type LibraryEntryKind with member this.schemaFile : FileName` answers
    `schema/library-<kind>.schema.json`. It also holds `member private this.tryLoadEntrySchema ()`,
    which forces the common lazy first.
  - There are six private per-kind lazies over `tryLoadEntrySchema`.
  - `validateEntry (kind : LibraryEntryKind) (element : JsonElement) : Result<unit, StorageError>` picks
    the kind's lazy and calls `validateWith`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/EntryFileJson.fs` (new)
  - `LibraryFileText of string` with `.value`.
  - `LibraryFileText.parseEntry (kind)` works in the order the slice gives:
    1. parse (`MalformedJsonErr`);
    2. `validateEntry` (`SchemaValidationErr`);
    3. `schema.version` read as a `FileSchemaVersion` and compared with `kind.currentSchemaVersion`
       (`UnsupportedSchemaVersionErr`);
    4. the recorded `contentHash` read as a `ContentHash`;
    5. the content bound from `JsonObject.Create root` with `contentHash` removed (`MalformedJsonErr`);
    6. `contentHashOf content` compared with the recorded hash (`ContentHashMismatchErr`).
    Every throwing .NET call is inside a `try` that maps to `MalformedJsonErr`.
  - ``LibraryEntryContent.``sealed`` `` is `contentHashOf this` paired with the content.
  - `LibraryEntryEnvelope.fileText` is `nodeOf content` with `contentHash` set to `nodeOf contentHash`,
    rendered as `indentedCanonicalText` plus one `\n`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`
  - `<Compile Include="EntryFileJson.fs" />` directly after `CanonicalJson.fs`.
  - Seven `Content` items after the groups schema item.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/library-common.schema.json` (new)
  - `$id` `https://berreman/schema/library-common.json`.
  - `$defs`: `guid` (canonical lower-case), `positiveVersion`, `contentHash` (`^[0-9a-f]{64}$`), the four
    provenance shapes, and `provenance` (`oneOf` the four).
- `schema/library-{material,sample,source,polarizer,detector,category}.schema.json` (new), draft 2020-12,
  each `$id` `https://berreman/schema/library-<kind>.json`.
  - **Unconditional:** `schema` is `{ kind: <tag const>, version: positiveVersion }`, with no other key.
  - **Under `if schema.version == 1` (`then`):**
    - every key is required and no other is allowed;
    - `id` is a GUID and `version` is positive;
    - `name` is a non-empty string;
    - `categoryOpt` is a GUID for a material and `null` otherwise;
    - `descriptionOpt` is a string for a sample, a string or `null` for a material, and `null`
      otherwise;
    - `payload` is `{ Case: <case const>, Fields: [ <one item> ] }`. For a record kind the item is an
      object with exactly its record's keys. For detector and category, whose field is a fieldless union,
      the item is the enum of that union's case names;
    - `provenance` is the common `provenance`, and `contentHash` the common pattern.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/EntryFileJsonTests.fs` (new): facts (a)–(j).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one
  `<Compile>` line after `LibraryEntryFilesTests.fs`.

## Decisions

- **`sealed` keeps its name, with backticks.** `sealed` is an identifier F# reserves. A plain
  `member this.sealed` raises `warning FS0046` at the declaration and at every use, which would break
  the zero-warning rule. The double-backticked ``` ``sealed`` ``` raises none, so the slice's name is kept
  exactly. Callers write ``` content.``sealed`` ```. The doc comment says why.
- **The hash key is `nameof` of the envelope's field, not a string constant.** The first ELEV run
  reported `ELEV003P EntryFileJson.contentHashKey`, a private `string` binding.
  - The writer now uses `nameof this.contentHash`.
  - The reader uses a local `nameof Unchecked.defaultof<LibraryEntryEnvelope>.contentHash`.
  - The file's flat key is therefore the envelope's field name by construction, not a re-transcribed
    literal.
- **The version and the recorded hash are read as their elevated types.** They are deserialized through
  `options` as `FileSchemaVersion` and `ContentHash`, so no bare `int` or `string` is unwrapped and
  re-wrapped. A version beyond `Int32` passes the schema but fails this read as `MalformedJsonErr`.
- **`$ref` timing.** JsonSchema.Net 9 builds a schema at `FromText`. Each kind's lazy therefore forces
  the common lazy before it loads its own file, so the common `$id` is registered before any entry schema
  is built. Before evaluation is not late enough. A missing common file is `SchemaMissingErr` for every
  kind.
- **The payload is checked as far as the slice says, and no deeper.** It is the case tag plus the
  record's keys, with `additionalProperties: false` on the record as on the envelope. The values inside
  the record are type-checked only (`object`, `string`, `object|null`). The physics is bound by the
  serializer, so malformed physics is `MalformedJsonErr`. The detector and category payloads have no
  record, so their single field is checked against its case-name enum.
- **The GUID pattern is lower-case only.** That is what System.Text.Json writes, and the canonical form
  `ElementValueId.tryToLibraryEntryId` accepts.
- **The fixtures follow the slice, with one choice of my own.** The user source in (c) is a `FixedLine`,
  so both `SourceTuning` shapes, and both branches of the schema's `tuning` type, are written and read.
  The constant magnetic aspect in (a) is a `GyromagneticMu` (axis `AlongX`), and the dispersive one is
  `defaultPolderDispersion`.
- **Scratch scripts are deleted; their output is kept.** The two `.fsx` probes were removed after they
  ran. Their logs stay in `.artifacts/`.

## Testing state

Local observations only. The arc-runner's gate engine is the gate authority.

- `build` (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`): exit 0,
  `0 Warning(s)`, `0 Error(s)` (`.artifacts/0030-build-final.log`).
- `EntryFileJsonTests` alone: 10/10 passed (`.artifacts/0030-entry-file-json-tests.log`).
- `constructor-unit-tests`, the whole suite (the gate's command): `Passed!`, 771 total, 771 succeeded,
  0 failed, 0 skipped. That is 761 before plus the 10 new facts
  (`.artifacts/0030-constructor-unit-tests-whole.log`).
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 5 project(s)`, with 0 known findings
  (`.artifacts/0030-elevation-local.log`). The first run found the one ELEV003P above, which is fixed.
  Storage's suppressed (ELEV004) count is 40, the same as step 29's.
- The six s04 invariants each report `INVARIANT HOLDS ... found nothing`
  (`.artifacts/0030-invariants.log`). Because `git grep` skips untracked files, the new `.fs` and
  schema files were also grepped directly for every invariant pattern: nothing found.
- EOL: every changed or new file is LF only, with no BOM and a final LF. `git diff --numstat` equals
  `--ignore-cr-at-eol`.
- Schema probe (`.artifacts/0030-schema-probe.log`):
  - **Accepted:** the valid document, the built-in and pulled provenances, and the version-2 file with a
    junk body and an extra key.
  - **Refused:** a bad GUID, an upper-case GUID, an upper-case hash, an extra key, a null `categoryOpt`,
    an empty name, version 0, `schema.version` 0, the wrong `schema.kind`, an extra key in `schema`, no
    `schema`, an unknown provenance, two built-in fields, a bad kit GUID, a pulled provenance without its
    citation, the wrong payload case, a payload without `eps`, two payload fields, and an empty object.

## Artifacts

- `specs/0050/.artifacts/0030-json-shapes.log`: the canonical JSON of every kind, as the schemas'
  reference.
- `specs/0050/.artifacts/0030-schema-probe.log`
- `specs/0050/.artifacts/0030-build.log` and `0030-build-final.log`
- `specs/0050/.artifacts/0030-entry-file-json-tests.log`
- `specs/0050/.artifacts/0030-constructor-unit-tests-whole.log`
- `specs/0050/.artifacts/0030-elevation-local.log`
- `specs/0050/.artifacts/0030-invariants.log`

## Gotchas

- **`sealed` must stay backticked.** See Decisions. A future rename should prefer a word F# does not
  reserve.
- **A single-project build recompiles the vendored MathNet.** Building `OpticalConstructor.Storage.fsproj`
  alone recompiled MathNet once and showed its two `SYSLIB0051` warnings, as steps 20, 23 and 24 did. The
  solution builds (`0030-build.log`, `0030-build-final.log`) show `0 Warning(s)`.
- **The serializer's union form is `{ "Case", "Fields": [...] }`.** FSharp.SystemTextJson uses
  AdjacentTag, so a case with fields is written that way. A fieldless case is its name (`UserProvenance`,
  `ThinFilm`), and an option is its value or `null`. The schemas pin exactly this form. A change to
  `CanonicalJson.options`' union encoding would break them, which is intended, and would also break the
  frozen-hash fact.
- **Chained `JsonNode` indexing followed by a method call does not parse.** `o["a"]["b"].AsArray()` is
  FS0597. The tests use only `x["a"]["b"] <- v`, which compiles; `o.["a"].["b"].AsArray()` is the safe
  form.
