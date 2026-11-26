# Step 0024 — IMPLEMENT — state of the world

## Where we are

Spec 0050, section S1.4: "Library entry files: envelope, provenance, canonical JSON, content hash,
schemas". Steps 21–23 gave the app one canonical serializer options value that carries every number
an entry holds. This step adds the Domain vocabulary of the entry files, in a new
`OpticalConstructor.Domain/LibraryEntryFiles.fs` compiled right after `ElementId.fs` (001 §A, §B;
.spec-md §3.11, §5). It covers the entry kinds and their extensions, the schema version, the
kind-independent entry id, the catalogue and kit ids, the provenance and the file-name rule. Step 25's
decoders and step 27's content hash build on these types.

## What's working

- Added `LibraryEntryKind`: six kinds with int wire tags 1–6, `tryOfWire`, `extension` (`.material-json` … `.category-json`), `currentSchemaVersion` and `all`
- Added `LibraryEntryId` (an entry's frozen GUID), with `toLibraryEntryId` on `MaterialId`, `SampleId` and `CategoryId`, and `ElementValueId.tryToLibraryEntryId` for preset ids
- Added `EntryProvenance` (built-in / user / kit import / source pull, with source, record, version, SPDX licence and citation); `protection` and `categoryOrigin` derive from it
- Added `CatalogueId`, `CatalogueName`, `KitId`, `ContentHash`, `FileSchemaVersion` and `EntrySchema`
- Added `FileStem.ofText`: only `: * ? " < > | / \` become `_`. `withExtension` and `EntryName.fileStem` build an entry's file name
- Added `LibraryEntryFilesTests` with the four facts the slice names

## Tests

The arc-runner's gate engine is the gate authority. These are local observations:

- `build`: exit 0, `0 Error(s)`. The two warnings are the vendored `MathNetNumerics/Numerics`
  `SYSLIB0051`, the same as step 0023's; none comes from our code (`.artifacts/0024-build.log`).
- `constructor-unit-tests`, whole suite: 746 total, 746 succeeded, 0 failed, 0 skipped. That is 742
  plus the 4 new `LibraryEntryFilesTests` facts, all discovered under their slice ids
  (`.artifacts/0024-constructor-unit-tests-whole.log`).
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 8 project(s)`, over Domain and its dependents.
  Known findings: 0 (`.artifacts/0024-elevation-local.log`).
- The six s04 invariants: each grep finds nothing, the new untracked files included.

```yaml
gates:
  constructor_unit_tests: 746
```

## Architecture

- **One id type for every kind.** `LibraryEntryId` is the GUID an entry file carries whatever its
  kind. Each kind's own id converts to it through a member on the source type. No conversion goes
  back (ELEV006): building a `MaterialId` from a file is the file seam's job (step 25).
- **Provenance decides protection.** `EntryProvenance.protection` (`EntryProtection`) and
  `categoryOrigin` (`CategoryOrigin`) answer the built-in / user split from the provenance. Each
  matches every case by name, so a new provenance case must choose its protection.
- **Preset ids parse strictly.** `ElementValueId.tryToLibraryEntryId` accepts only the canonical
  `D` GUID form, so each GUID has exactly one id text.
- **`KitId` lives in `LibraryEntryFiles`** because provenance needs it now. S4.1's `Kit` module
  reuses it.
- **The name rule is total and minimal** (.spec-md §3.11). It has no reserved-name, length or
  trimming rule. `withExtension` concatenates instead of calling STL's
  `FileName.addExtension`, which throws on a dotless extension.

## Deferred

Nothing. Every requirement of the slice, (1)–(7) and tests (a)–(d), landed this round. The
item-model kind is S7.1's by the slice's own text, so it is not part of this step.

## Gotchas

- `git grep`, which the s04 invariant gates use, skips untracked files. Grep a new file with
  `--untracked` before it is committed.
- A second `KitId` (for example in S4.1's `Kit.fs`) collides under ELEV014. Reuse
  `LibraryEntryFiles.KitId`.
- STL's `FileName.addExtension` evaluates `failwith this.value` on its no-dot branch, so it throws.
  `FileStem.withExtension` does not use it.
- `specs/0050/.manifest.state.json` shows as modified in the working tree. It is the supervisor's
  file, and this worker did not touch it.

## Changelog

- 2026-11-05 — Step 0024 (IMPLEMENT): new `OpticalConstructor.Domain/LibraryEntryFiles.fs` adds the
  entry kinds and extensions, schema version, `LibraryEntryId` and its conversions, catalogue and
  kit ids, provenance with derived protection, `ContentHash` and the `FileStem` name rule. New
  `LibraryEntryFilesTests` adds four facts; the constructor suite is 746/746.
