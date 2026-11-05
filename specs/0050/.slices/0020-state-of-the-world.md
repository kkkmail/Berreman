# Step 0020 — CLOSE_RED — state of the world

## Where we are

Spec 0050, section S1.3: "Every library entry is identified by a frozen GUID". Step 19 declared
RED_0004 (001 §B "Identity is GUID + version + hash", `.spec-md` §3.3): every seeded Library entry
id, and every source id `newSourceId` mints, must be the canonical string of a GUID. This step
closes it. The 33 source, detector and polarizer presets in `Library.PresetIds` now carry GUIDs
minted once, and `newSourceId` mints a plain GUID. Samples already had GUIDs, so every Library
entry id is now a GUID string. From this step the preset literals are frozen, and S1.5 exports
them byte-for-byte.

## What's working

- Gave each of the 33 seeded source, detector and polarizer presets a frozen GUID id (`PresetIds`)
- `newSourceId` mints a plain GUID string, as a sample's id is
- Documented that every `LibraryEntry.entryId` is the string of a GUID
- Added the fact `the live Library resolves a seeded preset, a user source and a sample by their GUID ids`
- RED_0004 is green: step 19's three facts now pass

## Tests

The arc-runner's gate engine is the gate authority. These are local observations:

- `build`: exit 0, 0 errors. The only warnings are the two vendored MathNet SYSLIB0051 warnings,
  which were already on the start tree.
- `constructor-unit-tests` (whole suite): Passed!, 739 of 739. That includes step 19's reds
  `every seeded Library entry id is the canonical string of a GUID, and no two are equal` and
  `newSourceId mints the canonical string of a fresh GUID`, plus the new live-Library fact.
- `ui-tests` (the 46 selected classes): Passed!, 560 of 560. That includes
  `Add source opens the editor on a freshly minted source id`.
- `ui-smoke`: Passed!, 208 of 208.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 8 project(s)`.
- The five s03 invariants each find nothing. So does the slice's old-preset-id grep, which S1.4
  will run as its invariant.

```yaml
gates:
  constructor_unit_tests: 739
  ui_smoke_tests:         208
  ui_tests:               560
```

## Architecture

- An entry id is still an `ElementValueId` over a string, but every id is now the canonical
  (lowercase, hyphenated) string of a GUID. That covers samples (`SampleId.toElementValueId`),
  presets (`PresetIds`) and user sources (`newSourceId`).
- `liveLibrary` keeps its code. Its `Guid.TryParse` sample probe now parses a preset id too. It
  finds no sample there (`SampleStore.tryGetSample` answers `Ok None`) and falls through to the
  presets.
- No product ordering changed. The Library lists keep their declaration order, and the window
  trees sort by label, not by id.

## Deferred

Nothing in this step's scope.

## Gotchas

- The 33 `PresetIds` GUIDs are frozen; never re-mint them. They are recorded in member order in
  `.artifacts/0020-minted-guids.txt`, and none equals a `MaterialIds`, `CategoryIds` or
  `SampleIds` literal.
- Test-local user-source ids (`src-user-1`, `src-mine`, …) stay text. The stores take any id, and
  the slice's grep does not match them.
- `newSourceId ()` and a fresh `Guid.NewGuid ()` string look alike. A fact that needs "an id that
  names nothing" uses the latter, so it does not suggest a source.

## Changelog

- 2026-11-05 — Step 0020: closed RED_0004. The 33 seeded presets carry frozen GUID ids,
  `newSourceId` mints a plain GUID, and a new fact shows that the live Library resolves a preset, a
  user source and a sample by GUID.
