# Step 0463 — implementation log

## Progress

- Read the task, the arc-runner and IMPLEMENT protocols, the project prompt, AGENTS.md, rulings R0001–R0005 (steps
  0427–0436; this step touches none of their files or behaviours), the slice and its gate snapshot.
- Read `CatalogueImport.fs`, `CatalogueImportStore.fs`, `MaterialImport.fs` (step 462's `GlassCatalogueText`), the
  step-444 pull (`MaterialSourcePulls.fs`) and the contract test file.
- Found that `MaterialImport.fs` compiles after `CatalogueImportStore.fs` in `OpticalConstructor.Storage.fsproj`, so
  `create` cannot reach `GlassCatalogueText`. Moved the one compile line up; see Gotchas, deviation 1.
- Implemented the Domain record, the context field and the drop path, then the Storage reader, then the three
  contract facts.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/CatalogueImport.fs`
  - `GlassCatalogueRead = { file; fileHash; glasses }`, declared before `CatalogueImportContext`.
  - `CatalogueImportContext.readGlassCatalogue : FileName -> Result<GlassCatalogueRead, CatalogueImportError>`.
  - `decideDroppedFiles` step (a) now reads each file into a list of `(file, envelope)` pairs:
    - a `.agf` file (ordinal, ignoring case) is read through `readGlassCatalogue`;
    - each glass is sealed in file order as `{ LibraryEntryContent.ofMaterialEntry VersionNumber.first (FileImportProvenance …) glass with id = this.newEntryId () }`;
    - a read or seal error is one `DroppedFileRefusedErr` naming the file;
    - an entry file is one pair, read exactly as before.
  - Steps (b)–(d) are unchanged over the concatenated pairs. The member's doc gains one line naming glass catalogues.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CatalogueImportStore.fs`
  - `create` (signature and `IoBoundary` mark unchanged) fills `readGlassCatalogue`:
    - `File.ReadAllBytes` sits in a `try`, and an exception becomes `DroppedFileRefusedErr "<file>: <message>"`;
    - the read carries `file.leafName`, `ContentHash.ofBytes` and `(GlassCatalogueText.ofBytes bytes).importGlasses`;
    - an `ImportError` becomes `DroppedFileRefusedErr "<file>: <describe>"`.
  - Opens `OpticalConstructor.Storage.MaterialImport`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj` (outside the effects list;
  deviation 1): the `MaterialImport.fs` compile line moved from after `Sidecar.fs` to before `CatalogueImportStore.fs`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CatalogueImportInMemoryTests.fs`
  - `CatalogueImportContractData` gains:
    - `glassCatalogueName` and `glassNames`;
    - `glassEntryIds`, six test-local GUIDs `31801b0<n>-444f-43c8-8440-000000000463`;
    - `mintingInOrder`;
    - `copiedGlassCatalogue`, which copies `fixtures/test-glasses.agf` into `freshImportFolder ()`.
  - `CatalogueImportContract` gains the three facts, so each runs in `CatalogueImportInMemoryTests` and
    `CatalogueImportSqliteTests`.

## Testing state

All runs are local development runs; the supervisor's gate engine is the authority.

- **build:** green, 84 s against a 600 s deadline, 0 errors. Warnings are the exempt `NU1902` and the vendored MathNet
  `SYSLIB0051` pair (on the start tree since step 0001). There are none from our code.
- **constructor-unit-tests:** whole suite (its `all_discovered` selection), 1837 of 1837, 109 s against a 180 s
  deadline. That is 1831 + 6, and the TRX lists all six new identities as `Passed`.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 226 s against a 600 s deadline.
- **kit-acceptance** (trait `Category=kit-acceptance`, class `KitAcceptanceTests`): `Passed!`, 14 succeeded and 1
  skipped (the xUnit-Explicit five-seed ensemble), 1414 s against a 3600 s deadline.
  - Both committed-kit equality facts pass.
  - AT-1 and AT-2's expectations and tolerances are unchanged.
- **invariant-s44-\* (21):** all find nothing.
- LF check: `git diff --numstat` equals `--ignore-cr-at-eol`, and no CR is in any edited file.

## Artifacts

- `specs/0050/.artifacts/0463-build.log`
- `specs/0050/.artifacts/0463-constructor-unit-tests.log`
- `specs/0050/.artifacts/0463-elevation.log`
- `specs/0050/.artifacts/0463-kit-acceptance.log`

## Gotchas

- **Deviation 1: compile order, outside the effects list.**
  - `GlassCatalogueText` (step 0462) is in `MaterialImport.fs`, which compiled after `CatalogueImportStore.fs`, so the
    brief's `create` could not reach it.
  - The `MaterialImport.fs` compile line moved up to just before `CatalogueImportStore.fs`. `MaterialImport.fs`
    opens no Storage module, so nothing else moved.
  - It is compile-only, as at steps 0313 and 0460, and the `.fsproj` gains no comment.
  - The alternatives were rejected:
    - filling the field in `AppContext` would leave `create` without the real reader the facts require;
    - moving the reader's code would edit `MaterialImport.fs`, which is also unlisted, and duplicate ownership.
- **Record-label resolution.** `GlassCatalogueRead` and `EntryFileImportInfo` share `file` / `fileHash`. Every new
  construction is annotated. Downstream constructions with a `file` label are annotated or carry a label only one
  type has, and the build confirms it.
- **`observeStartupImport` fixes `newEntryId`** to `forkEntryId`. The glass facts override it with
  `mintingInOrder glassEntryIds` after observing.
- **Interpretation:** "after `importFrom (CatalogueProxy.createBuiltIn ())`" is read as the contract's existing
  import-and-apply of the Built-in catalogue. `importFrom` alone writes nothing, and each glass references the
  built-in Glass category, which must be held for step (c)'s reference validation.
- The refusal fact's missing file is `missing-glasses.AGF`, which covers the case-insensitive extension.
