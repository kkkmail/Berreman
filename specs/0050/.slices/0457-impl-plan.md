# Step 0457 — impl plan (Henke compound adapter, spec 0050 S13.2, obligation S132-HENKE-ADAPTER)

## Approach

1. **`OpticalConstructor.Storage/MaterialImport.fs`**:
   - `HenkeScatteringText of string` with `.value`, beside `PtbOcdbText`;
   - `type HenkeScatteringText with member this.factors : Result<AtomicScatteringFactors, ImportError>`, after the PTB
     reader. It reuses `RiiPage.linesOf` for the lines and `AtomicScatteringFactors.tryCreate` (step 456) for the one
     validation:
     - the first non-empty line must start with `E(eV)`;
     - each further non-empty line is three invariant-culture numbers, split on tabs and spaces;
     - a row whose f1 is −9999 is left out.

     A refused header, a refused row and a refused table are `MalformedSourceFileErr`. No row left is `NoDataErr`. Any
     .NET exception is caught and answered `MalformedSourceFileErr`.
2. **`OpticalConstructor.Storage/MaterialSourceStore.fs`**:
   - `KitFileText.toHenkeScatteringText`, a private conversion member beside `toPtbOcdbText`;
   - a private `SourceSnapshotDownload` record (URL, file name, check);
   - on `SourceSnapshotLayout`:
     - its leftover clearing becomes one private member, which both updates share;
     - a new `updateFileByFile` member downloads each file in order into `<folder>.tmp` through the one HTTP seam. It
       reads the cancellation before each file and again after the download, checks each file, and reports progress
       after each. Once all are fetched it calls `installFrom`. Every exit except a finished install deletes the staged
       folder.
   - `type MaterialSourceProxy with static member createHenkeCompounds`:
     - its constants are the name, folder `cxro-henke`, `LicenseRef-CXRO` and the base URL, each written once;
     - its download list is the 92 elements, each `<symbol lower>.nff`, checked through `HenkeScatteringText.factors`;
     - `searchRecords` goes through `CompoundSpec.tryOfQuery` and checks that each formula element's file is held;
     - `fetchRecord` goes through `CompoundSpec.tryOfRecord`. It reads each element's bytes, parses them through
       `factors`, builds the table with `indexTableWith` and the entry with `isotropicEntry`. It answers the hash of the
       record's UTF-8 bytes followed by the element files' bytes, plus CXRO's citation.
3. **`OpticalConstructor.Domain/MaterialSources.fs`**: doc-only edits. The proxy doc names `createHenkeCompounds`, and
   `searchRecords` gains one line.
4. **Fixtures:** new `fixtures/si.nff` and `fixtures/o.nff`, in CXRO's format with step 456's test tables (LF, tabs).
   Each gets a `Content` item after `ptb-Co.txt`'s, with no comment.
5. **`MaterialSourceFixture`** gains `henkeFile`, `writeHenkeSnapshot` and `servingDownloader`, each with a one-line doc.
6. **Tests:**
   - `MaterialImportTests` gains one fact;
   - `MaterialSourceStoreTests` gains five facts, the census's six identities in all.

## Risks

- **ELEV010 on members and private members:** no new member or module function may mix a primitive with our types.
  Every new signature is fully elevated, and the primitives stay in local lets.
- **The density's text.** `CompoundSpec`'s G6 `densityText` is private in Domain, and `HenkeCompounds.fs` is outside
  this step's edits. The description and citation therefore render the density in G6 (invariant) in Storage, as the UI
  renders numbers. Recorded as a decision.
- **The fetch fact's version `2026-11-04`.** `writeHenkeSnapshot` stamps 2026-11-01, so the fetch fact first runs an
  update with `servingDownloader henkeFile` and the 2026-11-04 clock, then fetches. That update serves the fixture's own
  `si.nff` and `o.nff` bytes, so "the two fixture files" holds.
