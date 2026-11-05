# Step 0031 — IMPLEMENT — state of the world

## Where we are

This is spec 0050, section S1.4: "Library entry files: envelope, provenance, canonical JSON, content hash,
schemas". Step 30 landed the entry files. This step lands the catalogue manifest's file, `.catalogue-json`, and
the hash S3's start-up import will compare (001 §B "Import on every start"). It also proves that every code seed
has a file form, ahead of S1.7 deleting the seeds. This step does no file IO: it renders and parses text.

## What's working

- Added `schema/library-catalogue.schema.json` and `SchemaValidation.validateCatalogueManifest`, loaded once.
- Added `CatalogueManifest.fileText` and `CatalogueManifest.manifestHash`.
- Added `LibraryFileText.parseCatalogueManifest`, checking parse, schema, schema version, then binding.
- Moved the parse, the error mapping and the version check out of `parseEntry`, so both readers share them.
- Proved every built-in material, category, sample, source, detector and polarizer round-trips to its seed.
- Proved the built-ins form one catalogue, with unique file names per kind and no id and version repeated.

## Tests

The arc-runner's gate engine is the gate authority and has not run yet. These are development observations of the
final tree:

- Solution build: exit 0, `0 Warning(s)`, `0 Error(s)` (`.artifacts/0031-build.log`).
- `EntryFileJsonTests`: 15/15 passed (`.artifacts/0031-entry-file-json-tests.log`).
- The whole constructor suite: 776/776 passed, the 771 before plus the 5 new
  (`.artifacts/0031-constructor-tests-whole.log`).
- ELEV analyzers: 0 new findings in 5 projects, 0 known findings (`.artifacts/0031-elevation-local.log`).
- The six s04 invariants were not run through their own commands. Their patterns were grepped over the changed
  files, and nothing was found (`.artifacts/0031-tree-checks.log`).

```yaml
gates:
  constructor_unit_tests: 776
```

## Architecture

- **A manifest has no hash check on read.** Its hash is computed, never stored in it, so `manifestHash` is what S3
  compares with the one it recorded. An entry's hash, by contrast, is stored in its file and checked on read.
- **One schema law across library files.** The manifest follows the entries: the version is checked for every
  version and the body only for version 1, so another version reaches `UnsupportedSchemaVersionErr` before anything
  binds. There are no up-converters and no fallback readers.
- **The manifest binds whole.** The file is the record's canonical JSON, with no key added or removed, unlike an
  entry file, which adds `contentHash` beside the content's keys.
- **One copy of each read step.** `tryRead`, `checkSchemaVersion` and `LibraryFileText.readRoot` serve both readers.
  "Load the common schema first" lives once, in `FileName.tryLoadLibrarySchema`.

## Deferred

Nothing. Every requirement of the slice landed this round:
- the schema and its content item;
- `validateCatalogueManifest`;
- the three members;
- the five facts (a)–(e).

## Gotchas

- **The local checks were a reading of conflicting prompts.** "You run no checks" (`implement_worker.system-md`)
  conflicts with "Run the relevant tests after every change" (AGENTS.md) and the project prompt's testing procedure.
  This round ran the build, the tests and the ELEV script as development feedback, and reports no gate result. The
  build and ELEV commands are byte-for-byte the gates' own. See the impl-log's Gotchas.
- **`grep -c $'\r'` miscounts in Git Bash:** it answered the line count for files with no CR. Count CR bytes with
  `tr -cd '\r' < file | wc -c`.
- **The schema's `kind` maximum is 6.** S7.1's item-model kind must raise it together with `LibraryEntryKind`.
- **`git grep` invariants skip untracked files** until the supervisor stages them, so the new schema was grepped
  directly.

## Changelog

- 2026-11-05: step 0031 landed the catalogue manifest's file: its JSON Schema, `validateCatalogueManifest`,
  `CatalogueManifest.fileText`, `CatalogueManifest.manifestHash` and `LibraryFileText.parseCatalogueManifest`, with
  5 new `EntryFileJsonTests` facts proving every built-in has a file form. Constructor suite 776/776, ELEV clean.
