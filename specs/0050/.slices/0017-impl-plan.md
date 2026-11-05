# Step 0017 — IMPLEMENT — impl plan

## Approach

Step 15 gave every seeded preset one home, `Library.PresetIds` (`ElementId.fs:66`), and its ids
still hold their step-15 text. This step switches `OpticalConstructor.Ui.Tests/LibraryWindowTests.fs`
from string literals to those members. Since the text is unchanged, every fact keeps its
assertion and its result.

Step 16 did the same for the constructor suite, and this step repeats its method: a script that
counts every pattern before it replaces it (`.artifacts/0017-scripts/preset-ids.py`). The script
does the literal edits in four passes, following the slice's four clauses:

1. **Ids as values.** Each `ElementValueId "src-600"` / `"pol-lp"` / `"pol-cp-left"` /
   `"det-intensity"` / `"det-ellipsometer"` becomes its member. Where the literal was
   parenthesised, the parentheses go with it. This covers its uses inside
   `ConstructorUiId.libraryEntryNode`, `.libraryEntryCode`, `.libraryBranchLeaf`,
   `SelectLibraryEntry`, `SelectLibraryEntryNode` and `Some …`.
2. **Text compared with `filteredEntryIds` or the `onSelected` recorder.**
   - Lines 437, 447 and 623 use `PresetIds.<m>.value`.
   - Line 1080 uses `[ PresetIds.polarizerLp.value ]`.
   - Lines 1102, 1106 and 1151 use `"selected:" + PresetIds.<m>.value`.
   - The two sorted lists (`:1072-1074`, `:1143-1145`) use `List.sort [ …value; …value ]`.
3. **The id-format facts** build their expected text from the member: `:251` and `:369-371`.
4. **The Select-bay rows** (`:1372`, `:1383`) use
   `ConstructorUiId.libraryEntry PresetIds.detectorIntensity.uiKey`. This is the same form
   `:1361` already uses for `glassFilm600EntryId.uiKey`.

The file already opens `OpticalConstructor.Domain.Library` (which provides `PresetIds`) and
`OpticalConstructor.Ui` (whose `[<AutoOpen>] ConstructorUiIdOfDomain` provides `uiKey`). No
`open` is added.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LibraryWindowTests.fs`: the only
  authorized edit.

## Risks

- **Parse precedence.** `Some (ElementValueId "x").libraryEntryCode` becomes
  `Some PresetIds.m.libraryEntryCode`. A long identifier is one atom, so the parse is the same
  `Some (….libraryEntryCode)`.
- **Line 1372 and line 1383 are `ui-smoke` facts.** The `ui-smoke` gate runs them. They render
  the same id string, because `uiKey` is `ConstructorUiKey this.value`, so they keep their
  result.
- **No fact is added or removed**, so the `ui-tests` and `ui-smoke` counts cannot move.
