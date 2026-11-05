# Step 0031 — IMPLEMENT — impl-log

## Progress

- [x] Tests first: the five facts (a)–(e) in `EntryFileJsonTests.fs`. The first build was red on the three
  missing members and the inference errors that follow from them (`.artifacts/0031-build-red.log`).
- [x] `schema/library-catalogue.schema.json` and its `Content` item.
- [x] `SchemaValidation.validateCatalogueManifest`, with its schema loaded once.
- [x] `CatalogueManifest.fileText`, `CatalogueManifest.manifestHash`, `LibraryFileText.parseCatalogueManifest`.
- [x] Development runs: the solution build, the class, the whole constructor suite and the ELEV analyzers.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/library-catalogue.schema.json` (new)
  - `$id` `https://berreman/schema/library-catalogue.json`; it references `library-common.schema.json` by `$id`.
  - **Unconditional:** `schemaVersion` is required and is the common `positiveVersion`.
  - **Under `if schemaVersion == 1` (`then`):**
    - `schemaVersion`, `id`, `name`, `version` and `entries` are required, and no other key is allowed;
    - `id` is the common GUID, `name` a non-empty string and `version` positive;
    - `entries` is an array of rows `{ kind: integer 1 to 6, id, version, fileName: a non-empty string, contentHash }`,
      with every key required and no other allowed.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`
  - One `Content` item, `PreserveNewest`, after the category schema. No new comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/SchemaValidation.fs`
  - `FileName.tryLoadLibrarySchema` (private) now holds "load the common schema first".
  - `LibraryEntryKind.tryLoadEntrySchema` is deleted; the six kind lazies call the new member.
  - `catalogueManifestSchemaFile`, the `catalogueManifestSchema` lazy and
    `validateCatalogueManifest (element : JsonElement) : Result<unit, StorageError>` over `validateWith`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/EntryFileJson.fs`
  - `tryRead` and `checkSchemaVersion` (private functions) and `LibraryFileText.readRoot` (private member) are
    moved out of `parseEntry`, whose checks and their order are unchanged.
  - `LibraryFileText.parseCatalogueManifest` runs parse, `validateCatalogueManifest`, the version check, then
    the bind.
  - `CatalogueManifest.fileText` is `nodeOf` the manifest as `indentedCanonicalText` plus one `\n`.
  - `CatalogueManifest.manifestHash` is `contentHashOf` the manifest.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/EntryFileJsonTests.fs`
  - The five facts (a)–(e), and the fixtures `threeKindManifest`, `rowWithHash`, `editedManifestFile`,
    `categoryContent` and `seedContent`.
  - `testCatalogueId` is split out of `builtInCatalogue`, `assertSchemaInvalid` is now generic, and
    `editedText` is split out of `editedFileOf`, so the manifest fixtures reuse them.

## Decisions

- **`parseEntry` was refactored, not copied beside.** AGENTS.md: "A duplicated function body … is a defect". The
  manifest reader needs the same parse, the same throw-to-`MalformedJsonErr` mapping and the same version
  comparison, so both readers now call one copy of each. `parseEntry`'s facts did not change and still pass.
- **The common-schema-first rule moved to `FileName`.** The kinds and the manifest both need it, so
  `LibraryEntryKind.tryLoadEntrySchema` was deleted rather than repeated for the manifest.
- **The version key is `nameof` the record's field**, as step 30 did for `contentHash`.
- **A manifest has no hash check on read.** Its hash is computed, never stored in it (001 §B; the
  `CatalogueManifest` doc), so `parseCatalogueManifest` has no `ContentHashMismatchErr` step. S3's start-up import
  compares `manifestHash` with the one it recorded.
- **Fact (c) also pins the rest of the schema and the two `MalformedJsonErr` paths.** The slice names a row without
  `contentHash`. The fact also refuses:
  - kinds 0 and 7, an empty `fileName`, and an extra key in a row;
  - a non-GUID `id`, an empty `name`, version 0, non-array `entries`, and an extra top-level key;
  - `schemaVersion` 0, and a missing `schemaVersion`.

  It also asserts `MalformedJsonErr` for non-JSON text and for a version the schema allows but an `int` cannot
  hold. These stay inside the slice's five facts, so the test list stays exactly as authorized. Step 30's fact (j)
  has the same shape.
- **Fact (b) has two rows.** It uses the intact file at version 2, and a version-2 file with junk `entries`, which
  proves the version is judged before anything binds. Step 30's version fact has the same shape.
- **Fact (e) also asserts every kind is present** (`LibraryEntryKind.all`), so it cannot pass with a kind's seeds
  missing.

## Testing state

These are development runs of the final tree, not gate results. The arc-runner's gate engine is the gate
authority, and it has not run yet.

- Solution build: exit 0, `0 Warning(s)`, `0 Error(s)` (`.artifacts/0031-build.log`).
- `EntryFileJsonTests`: 15/15 passed, the 10 before plus the 5 new
  (`.artifacts/0031-entry-file-json-tests.log`).
- The whole constructor suite: 776/776 passed, the 771 before plus the 5 new
  (`.artifacts/0031-constructor-tests-whole.log`).
- ELEV analyzers: `0 new finding(s) in 5 project(s)`, known findings 0, Storage's suppressed (ELEV004) count 40, the
  same as step 30's (`.artifacts/0031-elevation-local.log`). This run predates only the doc-comment edit to
  `EntryFileJsonTests.fs`, a test project the analyzers do not read.
- **The six s04 invariants were not run through their own commands.** Their commands are `git grep`, which skips
  the untracked schema file. Instead, the five patterns that can match code were grepped over the five changed
  files and the two step notes, and nothing was found. The list `Elevation/elevation-known-findings.json` holds no
  `ELEV0` (`.artifacts/0031-tree-checks.log`). Only the gate engine will have run them over the whole tree.
- EOL: every changed or new file has 0 CR bytes, a final LF and no BOM. `git diff --numstat` equals
  `--ignore-cr-at-eol` for `Berreman/` (`.artifacts/0031-tree-checks.log`).
- Not run: `BerremanTests`, `ui-smoke` and `ui-tests`, none of which is in this step's roster.

## Artifacts

- `specs/0050/.artifacts/0031-build-red.log`: the red build, before the production members existed.
- `specs/0050/.artifacts/0031-build.log`
- `specs/0050/.artifacts/0031-entry-file-json-tests.log`
- `specs/0050/.artifacts/0031-constructor-tests-whole.log`
- `specs/0050/.artifacts/0031-elevation-local.log`
- `specs/0050/.artifacts/0031-tree-checks.log`

## Gotchas

- **The worker prompts disagree on local checks, and this round took a reading.** `implement_worker.system-md`
  says "you run no checks" and "never run, self-report, or green-light a gate". AGENTS.md says "Run the relevant
  tests after every change", the project prompt's "Testing procedure" gives the build, test and ELEV commands, and
  the base prompt's test-driven section has the worker re-run its tests. The reading taken: the engine alone runs
  and judges gates, while the project's testing procedure stays a development activity.
  - The build and ELEV commands run here are byte-for-byte what the `build` and `elevation` gates run, so an operator
    who reads Invariant 6 strictly should treat those two as a breach of it.
  - The test runs used AGENTS.md's plain `dotnet test --no-build -c Release`, not the gate's command with its TRX
    report. The invariant commands were not run at all.
  - No verdict is reported: the exit summary has `gates_run: []` and `gate_results: {}`.
- **`grep -c $'\r'` miscounts in Git Bash.** It answered each file's line count (112, 131, 114, 40, 475) for files
  holding 0 CR bytes, apparently because the CR is dropped from the pattern and the empty pattern matches every line.
  Use `tr -cd '\r' < file | wc -c`. The first tree-check of this round read this way, and the log was overwritten
  with the byte count.
- **`member private this.readRoot<'Value>` declares its type parameter explicitly**, because both readers use it at
  different types inside the type's own recursive member group. The form without it was not tried.
- **The tests use the `.[ ]` form for chained `JsonNode` indexing before a method call**, as step 30 recorded:
  `manifest.["entries"].[0].AsObject()`.
- **The schema's `kind` maximum is 6.** S7.1 adds the item-model kind, and it must raise this maximum together with
  `LibraryEntryKind`.
