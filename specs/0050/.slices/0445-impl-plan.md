# Step 0445 — impl plan

## Approach

refractiveindex.info's two file kinds, its catalog and its pages, are read by the one importer module,
`OpticalConstructor.Storage/MaterialImport.fs`, line by line, without a YAML package (spec 0050 S13.1).

1. `ImportError.describe` is an intrinsic member on the union. It renders each case's condition and its payload in
   the brief's four texts.
2. The private `RiiPage` gains the YAML line rules, so the page readers and the catalog reader share them:
   - `linesOf` splits a text into lines. `importMaterial` already split this way, so its one inline split moves
     here and its readings are unchanged (AGENTS.md "Reuse, never copy");
   - `valueAfter` gives the trimmed text after a key;
   - `unquoted` trims a value and removes one pair of enclosing `"` or `'`;
   - `citationOpt` reads `REFERENCES:` as an inline value or as a `|` block.
3. `RefractiveIndexInfoCatalogText of string` with `.value`. Its extension member `hits` folds the catalog's lines
   through a private `RiiCatalogReading` record, which holds:
   - the open book's name;
   - the open page's name and record (`RiiCatalogPage`);
   - the item opened last (`RiiCatalogItem`, three cases);
   - the hits of the pages closed so far.

   A page is closed when the next `- PAGE:`, `- BOOK:` or `- SHELF:` opens, or when the file ends. If it has a
   `data:`, it becomes one `MaterialSourceHit`. No hits gives `NoDataErr`, and a throw gives `MalformedYamlErr`.
4. `RefractiveIndexInfoText.referencesOpt` is `RiiPage.citationOpt` over the page's lines.
5. The fixture `fixtures/catalog-nk.yml` gets a `Content` / `PreserveNewest` entry beside `Si.yml`'s.
6. `MaterialImportTests` gains the census's three facts.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialImportTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/fixtures/catalog-nk.yml` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- **ELEV010 and ELEV010P check member signatures, private ones included.** A member that takes a `string` therefore
  returns only primitives, and one that answers our types takes none. A function local to a member body is not
  checked; the existing `resonant` and `tryFloat` are written that way.
- **Opening `LibraryEntryFiles` brings more records with an `id` and a `name` field into scope.** Each existing
  `MaterialEntry` construction already has its expected type from an annotation. The build will confirm it.
- **The fixture names contain `–` (U+2013) and `µ` (U+00B5).** The fixture and the test literals must use the same
  code points, so check the bytes after writing.
