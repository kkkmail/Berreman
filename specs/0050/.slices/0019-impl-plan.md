# Step 0019 — ADD_RED — impl plan

## Approach

Declare RED_0004 (001 §B "Identity is GUID + version + hash", `.spec-md` §3.3): every seeded
Library entry id, and every minted source id, is the canonical string of a GUID. Test sources
only; no production file changes.

1. `OpticalConstructor.Tests/SourceLibraryTests.fs`, beside the seeded-sources facts:
   - a private helper `canonicalGuidOf : ElementValueId -> Result<Guid, string>` that matches
     `Guid.TryParse` (never `.IsSome`) and accepts only when the parsed GUID's `string` (the
     lowercase hyphenated form `SampleId.toElementValueId` writes) equals the id; the `Error`
     carries the reason, naming the offending id;
   - `every seeded Library entry id is the canonical string of a GUID, and no two are equal`:
     every `seedEntries` id passes the helper, and no id repeats. Fails today on the presets'
     text ids (`src-600`, `det-intensity`, `pol-lp`, …);
   - `newSourceId mints the canonical string of a fresh GUID`: two calls each pass the helper and
     differ. Fails today on the `src-` prefix (`ElementId.fs:650`, the slice's `:594` drifted).
2. `OpticalConstructor.Ui.Tests/LightSourceEditorTests.fs`, `Add source opens the editor on a
   freshly minted source id`: name, setup, the update call and `Assert.Equal(1, calls.Count)`
   stay; `Assert.StartsWith("source-add:src-", calls.[0])` becomes "starts with `source-add:` and
   the rest is the canonical string of a GUID". Fails today on `src-<guid>`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SourceLibraryTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LightSourceEditorTests.fs`

## Risks

- The canonical-GUID check is written once per test project (a helper in the constructor suite,
  inline in the one UI fact). The authorized effects allow only the two `.fs` files, so no shared
  linked test source can be added, and the Domain has no such check to call (ADD_RED forbids a
  production change).
- `open System` could shadow names in an existing test file; `System.Guid` is written qualified.
- Local runs are observations made to record the red, as the ADD_RED obligation and steps 1, 4
  and 6 did; the gate engine stays the gate authority.
