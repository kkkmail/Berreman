# Step 0462 — impl plan

## Approach

This step adds a Zemax AGF glass-catalogue reader to `OpticalConstructor.Storage/MaterialImport.fs`. It reuses the
refractiveindex.info formula lowering: each AGF equation is rewritten, term for term, as the refractiveindex.info
formula of the same equation, then lowered by the existing `lowerFormulaAxis`. The entry is assembled by
`entryOfAxis`, so no second formula engine exists.

1. **`AgfFormulaNumber`** (`of int`, `.value`, `.family`). Its extension `riiFormOf` maps a glass's coefficients
   C1 … Cn, absent ones zero, to the refractiveindex.info page:
   - 1, 10 and 12 become formula 3; 2, 6, 11 and 9 formula 2; 3 formula 7; 4 formula 1; 5 formula 5; 7 and 8
     formula 4.
   - Any other number is `MalformedSourceFileErr`.
2. **`GlassCatalogueText`** (`of string`, `.value`, `ofBytes`).
   - A BOM picks the encoding. Without one, a zero in the first two bytes means UTF-16 LE; otherwise UTF-8.
   - `importGlasses` splits the lines through `RiiPage.linesOf`, groups `NM` blocks, and reads `NM` / `CD` / `LD`.
     It then calls `riiFormOf`, `lowerFormulaAxis` and `entryOfAxis`, named by the glass and described
     `Imported <family> glass (AGF formula <n>).`
   - Any glass that does not read or lower makes the whole text `MalformedSourceFileErr`, naming every such glass,
     one per line. No glass gives `NoDataErr`. Exceptions are caught as `MalformedSourceFileErr`.
3. **Reuse refactors inside `MaterialImport.fs`:**
   - `entryOfAxis` takes the entry's name and description, and the RII page passes its old ones unchanged.
   - The µm-bounds interval and the zero nominal interval come out of `RiiPage.rangeInterval` into two private
     bindings, so the page and the `LD` line share them.
4. **Fixture** `fixtures/test-glasses.agf` holds `TEST-S1`, `TEST-SCHOTT` and `TEST-HERZ`. It is a `Content` item,
   `PreserveNewest`, beside `ptb-Co.txt`.
5. **Three facts** in `MaterialImportTests`, exactly the names the effects declare.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialImportTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/fixtures/test-glasses.agf` (new)

## Risks

- **ELEV009 / ELEV010 on the how_to's literal signatures.** Both rules check members.
  - `riiFormOf (c : float array) : Result<RiiFormulaNumber * float array, ImportError>` carries a bare `float` on a
    Result happy path, beside elevated types.
  - `ofBytes (bytes : byte[]) : GlassCatalogueText` puts `byte[]` beside an elevated type, and `ofBytes` is not one
    of the analyzer's elevation-pattern names.
  - The tree is ELEV-clean and `elevation` is in the roster. So `riiFormOf` takes and returns the existing
    `[<NumericCarrier>]` `FormulaCoefficients`, made public, and `ofBytes` takes STL's `FileByteContent`, as
    `ContentHash.ofBytes` does. Making `FormulaCoefficients` public moves its pairing member onto the private
    `CoefficientPair` (`pairsOf`), because a public type cannot expose a private one.
- **Precision.** The 1e-12 tolerance against hand-written equations rests on the algebra being exact term for term.
  The only rearrangements are Sellmeier's `B·λ²/(λ²−C) = B + B·C/(λ²−C)` and formula 9's `1 + (C1 − 1)`. Both stay
  within about 1e-15.
- **kit-acceptance** is long, about 1440 s at 0460. It runs in the background after the build.
