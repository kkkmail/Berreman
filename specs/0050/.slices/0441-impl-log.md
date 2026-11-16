# Step 0441 — implementation log

## Progress

- Read the protocol, the project prompt, rulings R0001–R0005 (`.manual/030`, `031`, `033`, `035`, `036`) and the
  slice. None of the rulings touches this step's files; all are preserved.
- Wrote the impl-plan.
- **Domain** (`LibraryEntryFiles.fs`):
  - `SourcePullInfo` gains `recordHash : ContentHash` directly after `record`. Its one-line doc cites spec 0050
    S13.1: the SHA-256 of the record's text as the source's snapshot held it when this version was pulled.
  - `ContentHash` was declared about 120 lines below `SourcePullInfo` in the same file, so it moved, unchanged and
    with its docs, to just above `SourcePullInfo`.
  - No provenance case, member or other field changed. `attributionOpt`, `protection` and `categoryOrigin` read no
    hash.
- **Schema** (`library-common.schema.json`):
  - The `sourcePullProvenance` object gains the required key `recordHash`, `$ref` `#/$defs/contentHash`
    (`^[0-9a-f]{64}$`). Its `Fields` description names the hash and cites S13.1.
  - The entry schemas stay at version 1, as at step 405.
- **The library database is unchanged.** `LibraryDatabaseStore.fs` and `LibrarySchemaVersion.current` (1) are
  untouched. Its payload is the file text, so it carries the hash through the file, and so does a kit's snapshot.
- **The three `SourcePullInfo` constructions set the field**; no assertion changed:
  - `BuiltinCatalogue.licensedMaterialContent` binds its record id once and hashes that id's text (`test/<name>`)
    through `ContentHash.ofBytes`.
  - `LibraryEntryFilesTests` and `EntryFileJsonTests` each hash a test-local record text.
- **Added the fact** `a pulled provenance keeps its record's hash through an entry file, and a file without it is
  refused` to `EntryFileJsonTests`:
  - The provenance is `pulledFromSource` with `recordHash` = `ContentHash.ofBytes` of UTF-8 `abc`. The fact asserts
    that this is `ba7816bf…15ad`.
  - Silica under that provenance round-trips content → `sealed` → `fileText` → `parseEntry`. The envelope reads back
    equal, and the decoded provenance holds the hash.
  - In the same file text, the provenance's `recordHash` is first asserted to hold the hash, then removed. The
    result answers `SchemaValidationErr` through the existing `assertSchemaInvalid`.
  - The fact sits after `assertSchemaInvalid`, so it can reuse that helper and `editedText`.
- `every provenance round-trips through an entry file` and `a pulled entry and its forks carry its licence's
  attribution, and every other provenance none` are unchanged and pass.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryEntryFiles.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/library-common.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/BuiltinCatalogue.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryEntryFilesTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/EntryFileJsonTests.fs`

Every file is within the step's authorized edits.

## Testing state

These are development runs, evidence only, each with its gate's own command and selection.

- **build:** exit 0, 85 s against 600 s. The only warnings are the exempt NU1902 (12) and the vendored MathNet's
  pre-existing SYSLIB0051 (4).
- **elevation** (ELEV004 excluded, all 13 projects, no comparison base): `ELEV-GATE: OK - 0 new finding(s)`, 237 s
  against 600 s.
- **constructor-unit-tests** (`all_discovered`, the whole suite as the gate selects it): `Passed!`, 1762 of 1762,
  105 s against 180 s.
  - The count rises by 1 from 1761, checkpoint 425's baseline (no later checkpoint records the key).
  - TRX: the new fact passed in 0.27 s. `every provenance round-trips through an entry file`, `a pulled entry and its
    forks carry its licence's attribution, and every other provenance none` and `a built-in provenance is protected
    and every other provenance is user-managed` passed.
- **The twenty-one s43 invariants:** each decoded `git grep` finds nothing (exit 1 for all 21).
- **Line endings:** `git diff --numstat` equals `--ignore-cr-at-eol`; no CR in the diff.
- No development run exceeded its gate's effective deadline.

## Artifacts

- `specs/0050/.artifacts/0441-dev-build.log`
- `specs/0050/.artifacts/0441-dev-constructor-unit-tests.log` and `0441-dev-constructor-unit-tests.trx`
- `specs/0050/.artifacts/0441-dev-elevation.log`
- `specs/0050/.artifacts/0441-dev-invariants-b64.txt`

## Gotchas

- **`ContentHash` had to move.** It is declared in the same file as `SourcePullInfo` but further down, so the new
  field could not reference it in place. The move keeps its body and docs byte for byte.
- **A `$ref` with a sibling `description`** has no precedent in the schemas. The property is a plain `$ref`, like
  `originalId`, and the description is on the `Fields` array.
- **Nothing on disk holds a pulled provenance.** `git grep` outside `specs/` finds `SourcePullInfo` /
  `sourcePullProvenance` only in the domain, the schema, the database tag and the three test constructions. Requiring
  the key therefore refuses no committed kit, catalogue or `data/` file.
- **The opens in `EntryFileJsonTests`.** `open Softellect.Sys.Primitives` (for `FileByteContent`) is placed before
  the Domain opens, as in `LibraryEntryFilesTests`, so a Domain name still wins any clash.
