# Step 0020 — CLOSE_RED — impl plan

Closes RED_0004 (001 §B "Identity is GUID + version + hash", `.spec-md` §3.3), declared by step 19.

## Approach

1. `OpticalConstructor.Domain/ElementId.fs`, `Library.PresetIds`: replace the text of each of the 33
   members with a GUID minted once now (`[guid]::NewGuid()`), written `ElementValueId "<guid>"` in the
   lowercase hyphenated form `string guid` prints. Check that the 33 are distinct and collide with no
   `MaterialIds`, `CategoryIds` or `SampleIds` literal. From this step the literals are frozen.
2. `newSourceId` mints `ElementValueId (string (Guid.NewGuid ()))`; its doc says the id is a plain
   GUID string, as a sample's is (`SampleId.toElementValueId`).
3. `LibraryEntry.entryId`'s doc says every entry id is the string of a GUID: a sample's `SampleId`,
   a preset's `PresetIds` member, a user source's minted id. `liveLibrary` keeps its code; its
   `Guid.TryParse` probe now parses a preset id, finds no sample (`Ok None`) and falls through to the
   presets.
4. `OpticalConstructor.Tests/SourceLibraryTests.fs` gains
   `the live Library resolves a seeded preset, a user source and a sample by their GUID ids`.
5. No product ordering changes. Survey for facts that relied on the old ids' alphabetical order
   (Map / Set / sort keyed by `ElementValueId`), and fix such a fact order-independently if one
   appears.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SourceLibraryTests.fs`

## Risks

- A product path that tells a sample from a preset by "does the id parse as a GUID". Surveyed:
  only `liveLibrary`'s probe does, and it falls through on `Ok None`. `ConstructorUiIdOfDomain`'s
  `Guid.TryParse` calls read category / material codes only.
- Ordering keyed by entry id (a `Set`/`Map` over `ElementValueId` that is enumerated, a sort by id).
  Surveyed product sorts: they sort by label (stable), not by id. Confirm by running the suites.
- The slice's grep must find nothing over `Berreman/*.fs` afterwards; today only `ElementId.fs`
  hits.
