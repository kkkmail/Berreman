# Step 0026 — IMPLEMENT — state of the world

## Where we are

Spec 0050, section S1.4: "Library entry files: envelope, provenance, canonical JSON, content hash,
schemas". Steps 24 and 25 gave `OpticalConstructor.Domain/LibraryEntryFiles.fs` the entry
vocabulary, content and envelope. This step adds the catalogue manifest model in the same file. A
`.catalogue-json` holds the catalogue's name, id and version, plus one row per entry with its hash
(001 §A "Catalogue folder layout"; .spec-md §5). Its pure per-kind uniqueness check is the one that
export runs.

Later steps build on it:
- step 31 adds the manifest's JSON schema, text and hash;
- step 32 adds `CatalogueData` and `CatalogueProxy` over `tryCreate`;
- step 33 adds the folder store, which reads `CatalogueManifest.fileName`.

## What's working

- Added `CatalogueManifestRow` (kind, id, version, file name, content hash) and
  `LibraryEntryEnvelope.manifestRow`.
- Added `CatalogueManifest` (schema version, id, name, version, rows) with `currentSchemaVersion`
  (1), `extension` (`.catalogue-json`) and `fileName` (named by its extension alone).
- Added `CatalogueManifest.tryCreate`. It refuses one id + version twice
  (`DuplicateEntryVersionErr`) and two entries of one kind whose file names are equal ignoring case
  (`DuplicateEntryFileNameErr`). It sorts the rows by kind tag, then ordinally by file name.
- Added `LibraryEntryContent.describe`, so each refusal names both entries.
- Added three `LibraryEntryFilesTests` facts; the constructor suite is 754/754.

## Tests

The arc-runner's gate engine is the gate authority. These are local observations:

- `build`: exit 0, `0 Warning(s)`, `0 Error(s)` (`.artifacts/0026-build.log`).
- `constructor-unit-tests`, whole suite: 754 total, 754 succeeded, 0 failed, 0 skipped. That is 751
  plus the three new facts, each discovered under its slice id
  (`.artifacts/0026-constructor-unit-tests-whole.log`).
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 8 project(s)` (Domain plus its dependents), with
  0 known findings (`.artifacts/0026-elevation-local.log`).
- The six s04 invariants: each grep finds nothing.

```yaml
gates:
  constructor_unit_tests: 754
```

## Architecture

- **The row comes from the envelope.** `manifestRow` reads the content's kind, id, version and
  `fileName`, plus the envelope's `contentHash`, so no row is hand-assembled. `CatalogueManifestRow`
  is declared before the envelope, which makes `manifestRow` an intrinsic member.
- **The manifest's text does not depend on arrival order.** The rows are sorted by kind tag, then
  by `String.CompareOrdinal` on the file name. After the uniqueness check that order is total,
  because no two rows of one kind share a name, even ignoring case.
- **Uniqueness follows the Windows rule.** File names compare with
  `StringComparison.OrdinalIgnoreCase`, and only within one kind: entries of different kinds have
  different extensions, so they never clash. The `FileStem` rule makes `Si:O` and `Si_O` one file.
- **The manifest stores no hash of itself.** Storage computes it (step 31), and S3's start-up
  import records it.
- `.catalogue-json` is written once, in `CatalogueManifest.extension`. `fileName` derives from it
  through `FileStem.ofText ""`, the rule for a file that exists once per folder (001 §A).

## Deferred

Nothing. Every requirement of the slice landed this round: (1) the row and `manifestRow`, (2) the
manifest, its three static members and `tryCreate` with both refusals, and tests (a)–(c). The
manifest's JSON, schema and hash belong to step 31, by the spec's own plan.

## Gotchas

- When two envelopes share an id and a version, `DuplicateEntryVersionErr` wins over
  `DuplicateEntryFileNameErr`: it is the more specific diagnosis of a duplicated entry.
- One id at two versions with different file names is accepted. The slice refuses only one id +
  version twice.
- `CatalogueManifestRow` adds the record labels `kind`, `id`, `version`, `fileName` and
  `contentHash`, and `CatalogueManifest` adds `id`, `name` and `version`. Annotate record
  expressions in any file that opens `LibraryEntryFiles`.
- ELEV010P fires on a PRIVATE static member when its signature names one of our types beside a
  bare primitive (`LibraryEntryEnvelope -> string`). Put the render on the rendered type and keep
  comparators as lambdas.
- A full rebuild that recompiles the vendored MathNet shows its two SYSLIB0051 warnings. They are
  pre-existing and were recorded by earlier rounds.

## Changelog

- 2026-11-05: step 0026 added `CatalogueManifestRow`, `LibraryEntryEnvelope.manifestRow` and
  `CatalogueManifest` (`currentSchemaVersion`, `extension`, `fileName`, `tryCreate`), the two
  duplicate refusals in `LibraryEntryError`, `LibraryEntryContent.describe`, and three
  `LibraryEntryFilesTests` facts (754/754).
