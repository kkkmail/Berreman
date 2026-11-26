# Step 0454 — impl plan (IMPLEMENT, S132-PTB-READER)

## Approach

This step adds a PTB OCDB reader to `OpticalConstructor.Storage/MaterialImport.fs`, the module that already reads every
other optical-constants file. Step 455's adapter will call it and parse nothing itself.

1. `ImportError` gains `MalformedSourceFileErr of reason : string`. Its `describe` renders it as
   `the source file is malformed: <reason>`.
2. The entry assembly in `entryFromTabulated` moves into
   `type TabulatedNkTable with member this.isotropicEntry name category description`. That is one isotropic dispersive
   segment over `sampleRange`, with the axis `TabulatedNK this`, no magnetic or active part, and a minted `MaterialId`.
   - `entryFromTabulated` keeps its sort, its `tryCreate` and its `NoDataErr` mapping, then calls `isotropicEntry`.
   - It passes the description it wrote before, so the refractiveindex.info and CSV entries keep every field.
3. `PtbOcdbText of string` (with `.value`) is declared beside the other text types. A `type PtbOcdbText with` extension
   carries `headlineOpt` and `importMaterial`.
   - A private `PtbOcdbLines` type (the record's lines, split by the existing `RiiPage.linesOf`) reads the parts of
     the text:
     - the comment texts;
     - the column header;
     - the sigma level;
     - the numbered data lines.
   - `importMaterial` checks, in this order:
     - the required columns;
     - that the bound columns are all present or all absent;
     - that bounds come with a sigma level;
     - each row's value count and numbers;
     - that there is at least one row (`NoDataErr`).
   - Then it sorts the rows by wavelength, carrying the uncertainties with them, and builds the table with `tryCreate`
     and, when there are bounds, `withUncertainty`. Every `TabulatedTableError` becomes `MalformedSourceFileErr`.
   - The entry is `table.isotropicEntry (EntryName "Imported (PTB OCDB)") CategoryIds.semiconductor
     (EntryDescription "Imported tabulated n,k (PTB OCDB).")`.
   - The body runs inside `try … with` and answers `MalformedSourceFileErr`.
4. Fixture `OpticalConstructor.Tests/fixtures/ptb-Co.txt`: the seven header lines of PTB's `Co.txt` and three of its
   rows, tab-separated. It is checked against the live file (`.artifacts/0454-ptb-source/Co.txt`). It is a `Content`
   item, `PreserveNewest`, placed right after `sample-nk.csv`, with no comment.
5. Tests in `MaterialImportTests`:
   - two new facts;
   - `every import error renders its condition and its payload` widened by one row.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/fixtures/ptb-Co.txt` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialImportTests.fs`

## Risks

- **ELEV010 on a member that mixes our types with primitives** (step 0453's lesson). The public members' signatures
  use only elevated types. The private helper members speak only primitives.
- **ELEV019 (unique extension member on one receiver)**: no other `isotropicEntry` exists among our assemblies.
- **ELEV016 (unique DU case names)**: no `MalformedSourceFileErr` exists in Berreman or STL.
- **FS0025**: `ImportError` has no complete match outside `MaterialImport.fs`; `describe` gains its case.
- **LF endings**, the fixture's tabs included.
