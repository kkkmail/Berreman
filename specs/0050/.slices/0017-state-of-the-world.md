# Step 0017 — state of the world

## Where we are

Spec 0050, section S1.3: "Every library entry is identified by a frozen GUID".

- **Step 15** gave every seeded preset one home, `Library.PresetIds` (`ElementId.fs:66`). It kept
  the ids' old text.
- **Step 16** switched the constructor suite to those members.
- **This step** switches the Library window's UI suite,
  `OpticalConstructor.Ui.Tests/LibraryWindowTests.fs`.

The test file no longer spells any seeded preset id. A later step that gives the presets
their frozen GUIDs therefore leaves every Library-window fact unchanged.

## What's working

- LibraryWindowTests names every seeded preset through `PresetIds`, with no preset id text left.
- Tree-node, entry-code and branch-leaf ids are built from the members.
- Select-bay rows are keyed by `ConstructorUiId.libraryEntry PresetIds.detectorIntensity.uiKey`.
- Recorder texts and filtered-id lists come from `PresetIds.<m>.value`, and the two sorted
  lists use `List.sort`.
- Every fact keeps its assertion, so ui-tests (LibraryWindowTests, 39) and ui-smoke (208) keep
  their counts.

## Tests

No gate was run: under Invariant 6 the gate engine is the only gate authority. The round ran
local construction checks, which are observations, not gate results:

- **Build**: exit 0, 0 errors.
- **Selected ui-tests (`OpticalConstructor.Ui.Tests.LibraryWindowTests`)**: Passed!, 39 of 39.
- **ui-smoke**: Passed!, 208 of 208, equal to the checkpoint.
- **ELEV**: `ELEV-GATE: OK`. The test project is out of scope.
- **The five s03 invariants and the slice's grep**: each finds nothing.

The details are in `0017-impl-log.md` (Testing state).

```yaml
gates:
  ui_smoke_tests:         208
  ui_tests:                39
```

## Architecture

- **No new shape.** The test file reads the `PresetIds` members as the domain already exposes
  them.
- **The Select bay's leaf-row id goes through `ElementValueId.uiKey`**
  (`ConstructorUiIdOfDomain.fs:28`), the key the bay itself gives its rows. The test no longer
  re-wraps a string in `ConstructorUiKey`.

## Deferred

Nothing. Every clause of the slice, (1) to (4), landed this round.

## Gotchas

- **A sorted expected list must be sorted itself.** The two lists compared with
  `filteredEntryIds … |> List.sort` are written `List.sort [ …value; …value ]`. Once the ids
  become GUIDs, the members' text may no longer sort in the order they are written.
- **`:1372` and `:1383` carry the `ui-smoke` trait.** The ui-tests selection does not run them;
  the ui-smoke gate does.
- **Hard-coded preset ids remain elsewhere.** Nine other UI test files still spell preset ids,
  among them `ScrollExtentTests`, `WorkbenchInfoAndCaptureTests`, `LightSourceEditorTests` and
  `FsxAcceptanceTests` (`git grep -l -E '"(src|det|pol)-[a-z0-9]'` over the project). This
  step's authorized effects cover `LibraryWindowTests.fs` alone.

## Changelog

- 2026-11-05 — Step 0017: LibraryWindowTests switched from preset id literals to
  `Library.PresetIds` (67 lines), with counts unchanged.
