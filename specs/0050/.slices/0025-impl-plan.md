# Step 0025 — IMPLEMENT — impl plan

## Approach

Extend `OpticalConstructor.Domain/LibraryEntryFiles.fs` after step 24's types with the entry
content model (001 §B; .spec-md §5):

1. The payload: `SamplePayloadData`, `SourcePayloadData`, `PolarizerPayloadData` and the
   six-case `LibraryEntryPayload` with `kind`. It also gets a `currentSchema` member, so every
   builder sets `schema` the same way: the payload's kind at the kind's current version.
2. `LibraryEntryContent` (the file without its hash) with `fileName`. `LibraryEntryEnvelope` pairs
   the content with its `ContentHash`.
3. `LibraryEntryError`: four `of reason : string` cases ending in `Err`.
4. `CategoryName.toEntryName`, a type extension, so the category builder never re-wraps `.value`.
5. The builders, as static members of `LibraryEntryContent`:
   - `ofMaterialEntry` / `ofSample` answer the content;
   - the four preset and category builders answer `Result`. They refuse a non-GUID preset id
     (`EntryIdNotGuidErr`), and a protection or origin that disagrees with the provenance
     (`ProvenanceMismatchErr`).
6. The decoders, as members of `LibraryEntryContent`, each marked
   `[<ElevationBoundary(nameof StorageBoundary, …)>]`. Each builds the kind's own id from the
   entry GUID. A payload of another kind answers `EntryKindMismatchErr`; a material without a
   category, or a sample without a description, answers `MissingEntryFieldErr`.

`LibraryEntryFilesTests` gets the five facts (a)–(e), named word for word as in the slice.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryEntryFiles.fs` (edit)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryEntryFilesTests.fs` (edit)

The fsproj files need no change: both files are already compiled.

## Risks

- **Whether a property-level `[<ElevationBoundary>]` is seen.** The ELEV001 boundary must be
  recognised on property members; the analyzer reads `m.Attributes` of the member. The local ELEV
  run checks this.
- **Exact inverse for presets.** A preset id in non-canonical GUID text (upper case) would come back
  lower-cased. `ElementValueId.tryToLibraryEntryId` is tightened to the canonical text its doc
  comment already promises, so builder ∘ decoder is exact for every accepted id.
- **New names.** Every new type, case and extension-member name was grepped across Berreman and
  STL (ELEV014/016/019); none exists.
