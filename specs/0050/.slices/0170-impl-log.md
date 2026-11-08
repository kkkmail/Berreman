# Step 0170 — IMPLEMENT — impl-log

## Progress

- [x] Read the protocol (base and `IMPLEMENT` delta), the project prompt, the slice, its gate snapshot, manual 013,
  `KitHistory.fs`, `KitHistoryTests.fs`, `Kit.fs` (`KitName`, `KitFolder`), and step 169's log and SoW.
- [x] `KitHistory.fs`: `KitHistoryFilter`, `entriesMatching`, `KitHistoryRow`, `rowsMatching`.
- [x] `KitHistoryTests.fs`: the three facts.
- [x] LF check: neither edited file contains a CR. `git diff --numstat` and `--ignore-cr-at-eol` agree (+34 / +39).
- [x] State of the world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitHistory.fs`
  - **New type, between `KitHistoryEntry` and `KitHistory`:** `KitHistoryFilter = KitHistoryFilter of string`.
    - It has `member this.value : string` (the `KitName` accessor form) and
      `static member none : KitHistoryFilter = KitHistoryFilter ""`.
    - Its one-line doc cites spec 0050 S5.2.
  - **New intrinsic member on `KitHistory`:**
    `member this.entriesMatching (filter : KitHistoryFilter) : KitHistoryEntry list`.
    - It is declared after `displayOrder` and filters `this.displayOrder`, so the order is always `displayOrder`'s.
    - An entry is kept when `entry.name.value` or `entry.folder.value.value` contains `filter.value.Trim()`, compared
      with `StringComparison.OrdinalIgnoreCase`.
  - **New record, after `KitFolderPresence`:**
    `KitHistoryRow = { entry : KitHistoryEntry; presence : KitFolderPresence }`.
  - **New intrinsic extension, after `KitHistoryRow`:**
    `type KitHistory with member this.rowsMatching (filter : KitHistoryFilter) (probe : KitFolder -> KitFolderPresence) : KitHistoryRow list`.
    - It maps `this.entriesMatching filter` through an annotated local
      `rowOf (entry : KitHistoryEntry) : KitHistoryRow = { entry = entry; presence = probe entry.folder }`.
    - It needs the extension form because `KitHistoryRow` and `KitFolderPresence` are declared after `KitHistory`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryTests.fs`: three facts over the existing `mixed`
  history, named exactly as the slice gives them. They sit before the error-render fact.
  - `entriesMatching keeps the kits whose name or folder holds the filter, ignoring case`:

    | Filter | Answers | Matched on |
    |---|---|---|
    | `"kit A"` | `[ kitA ]` | the name |
    | `"/C"` | `[ kitC ]` | the folder |
    | `"  KITS/  "` | all five, in display order | |
    | `"zz"` | nothing | |

  - `a blank filter keeps the whole display order`:
    - `KitHistoryFilter.none` and `KitHistoryFilter "   "` both answer `d`, `b`, `c`, `a`, `e`.
    - It also pins `KitHistoryFilter.none = KitHistoryFilter ""`, as the slice defines it.
    - It also checks that the empty history answers no entries.
  - `rowsMatching pairs each listed kit with its folder's presence`:
    - The probe is local and spelled out as the slice gives it: `KitFolderMissing` for `C:/kits/b` and `C:/kits/e`,
      `KitFolderPresent` for every other folder.
    - `rowsMatching KitHistoryFilter.none` answers `d` present, `b` missing, `c` present, `a` present and `e` missing.
    - `rowsMatching (KitHistoryFilter "Kit b")` answers `b` missing alone.

## Decisions

- **Blank filter.** No special case was added. `"".Contains` is true for every string under every comparison, so a
  filter that trims to `""` keeps every entry. The doc comment says this, so a reader does not look for a
  missing `IsNullOrWhiteSpace` branch.
- **The row is built through an annotated local.** `entry` is also a field label of records in `CatalogueImport`,
  `MaterialStore` and `RayModel`. Annotating `rowOf`'s return type makes the record construction type-directed, so
  it resolves to `KitHistoryRow` whatever is in scope. The test's `rowOf` is annotated the same way.
- **The test probe is spelled out, not `KitHistoryProxy.createInMemory`.** The slice describes the probe as "answering
  `KitFolderMissing` for b and e and `KitFolderPresent` for every other folder". The in-memory double answers the
  reverse way (it names the present folders), so the slice's wording is used literally.
- **What was kept out, as the slice says:** no sort option, no word splitting, no cache and no `bool` in any
  signature or record.
- **Not reused: `Facets.TextQuery`.** The slice names the `KitHistoryFilter` contract symbol explicitly, and
  `TextQuery` belongs to the facet engine's constraint model. The one shared idiom, `Contains(…, OrdinalIgnoreCase)`,
  is a BCL call, not a duplicated body.

## Testing state

- Under invariant 6 this `IMPLEMENT` role runs no checks, so no gate was run: no `dotnet build`, no `dotnet test`,
  no analyzer, no invariant grep.
  - Because nothing compiled locally, every expression copies a form that already compiles in the tree:
    - the `.value` accessor of `KitName`;
    - the `Contains(…, StringComparison.OrdinalIgnoreCase)` call of `Facets.textFilterDef`;
    - the leading-`||` continuation of `KitFolders.fs`;
    - the intrinsic `type … with` extension of `KitHistoryProxy`.
  - The arc-runner still has to run build, elevation, constructor-unit-tests (all discovered) and the fourteen s20
    invariants.
- **Expected count.** `KitHistoryTests` now has 14 facts, 3 of them new, so constructor-unit-tests should rise by 3.
  - The latest recorded checkpoint is 168, at 1,163.
  - Step 169's net change was +3, so 1,166 is expected after it. This step would then give 1,169.
  - None of this was measured here.
- **Uniqueness.** `git grep` finds no other `KitHistoryFilter`, `KitHistoryRow`, `entriesMatching` or `rowsMatching`
  in `Berreman/`, and no `KitHistoryFilter` or `KitHistoryRow` in `C:\GitHub\Softellect`. That covers ELEV014, 016
  and 019.
- **Section invariants.** None of the fourteen s20 grep words appear in either edit.

## Artifacts

None. No logs were captured, because no command whose output is evidence was run.
