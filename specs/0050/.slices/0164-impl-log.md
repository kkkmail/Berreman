# Step 0164 — implementation log

## Progress

- Read the system prompts, the project prompt, the slice, manual 013 and the gate snapshot.
  Checkpoint 162 (step 0163) in `.checkpoints-json` is `constructor_unit_tests: 1138` and
  `ui_smoke_tests: 224`.
- Read the types the slice opens:
  - `Kit.fs`: `KitFolder of FolderName`, `KitName`, `ClockTime of DateTime` (no attributes, so
    structural equality and comparison hold).
  - `LibraryEntryFiles.fs`: `KitId of Guid`, `FileSchemaVersion of int`.
  - `WorkbenchSettings.fs`: `KitHistoryLimit`, with an int `value`.
  - The MRU rule in `Storage/RecentFiles.fs:bump`: `String.Equals(…, StringComparison.OrdinalIgnoreCase)`
    over `FileName.value`.
- Name census (`git grep -w`): `KitStar`, `StarredKit`, `UnstarredKit`, `KitHistoryEntry` and
  `KitHistory` appear nowhere under Berreman or in STL's `*.fs` (read-only grep). The members
  are intrinsic, so ELEV019 does not apply.
- Read STL's analyzer source (read-only) for the private comparison:
  - `NoPrimitiveBesideElevated.fs` and `ElevationAnalyzer.fs` show that ELEV010 checks private
    members under ELEV010P, and that `bool` / `int` are in the primitive table (`System.Boolean`,
    `System.Int32`).
  - A private `KitFolder -> KitHistoryEntry -> bool` comparison would therefore be an ELEV010P
    finding.
  - `value` and `tryOfWire` are elevation-pattern members (`ElevationAnalyzer.fs:662`), so
    `KitStar.value : int` and `tryOfWire (tag : int) : KitStar option` are sanctioned, as
    `LibraryEntryKind`'s are today.
  - ELEV012 reads only member and module-level signatures, never lambdas or a local `let`.
- Wrote `KitHistory.fs`, its Domain `<Compile>` line, `KitHistoryTests.fs` and its test
  `<Compile>` line (details below).
- Line endings: `file` reports plain LF text for all four product files and both slice notes.
  `git diff --numstat` equals `--ignore-cr-at-eol` (1/0 for each fsproj).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitHistory.fs` (new):
  `module KitHistory` in `namespace OpticalConstructor.Domain`. It opens `System`,
  `LibraryEntryFiles`, `Kit` and `WorkbenchSettings`, and declares, in order:
  - `KitStar = StarredKit | UnstarredKit`:
    - `member this.value : int` answers 1 and 2;
    - `static member tryOfWire (tag : int) : KitStar option` answers `None` for any other tag.
  - `KitHistoryEntry = { folder : KitFolder; kitId : KitId; name : KitName; lastOpened : ClockTime; star : KitStar }`.
  - `KitHistory = { schemaVersion : FileSchemaVersion; entries : KitHistoryEntry list }`:
    - `static member currentSchemaVersion = FileSchemaVersion 1`;
    - `static member empty` holds that version and no entry;
    - `member private this.changeEntry (folder : KitFolder) (change : KitHistoryEntry -> KitHistoryEntry option) : KitHistory`
      is the one folder comparison, ordinal ignoring case over `entry.folder.value.value`. It maps
      the matching entry through `change`, where `None` drops it, and keeps every other entry and
      the order;
    - `star` / `unstar` are `changeEntry folder (fun entry -> Some { entry with star = … })`;
    - `forget` is `changeEntry folder (fun _ -> None)`;
    - `evict (limit : KitHistoryLimit)` indexes the entries and keeps the unstarred ones. It sorts
      them by `(lastOpened, -index)`: earliest first, and the later stored entry first among equal
      times. It truncates that order to `max 0 (entries.Length - limit.value)` and filters those
      indices out, so the survivors keep their stored order and a starred entry is never a
      candidate;
    - `displayOrder` is the starred group, then the unstarred group, each
      `List.sortByDescending _.lastOpened`. The sort is FSharp.Core's stable sort, so equal times
      keep their stored order.
  - Every doc is one to three lines and cites spec 0050 S5.1; `forget` also cites S5.2. No cache,
    `bool` flag, sort option or other member was added. Star, unstar and forget never evict.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`:
  one line, `<Compile Include="KitHistory.fs" />`, directly after `KitFolders.fs`, with no
  comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryTests.fs` (new):
  `module KitHistoryTests` in `namespace OpticalConstructor.Tests` (`ADD_TEST OpticalConstructor.Tests.KitHistoryTests`).
  - Fixtures: `entryOf kit id minutes star` builds folder `C:/kits/<kit>`, a fixed GUID
    `KitId`, `Kit <kit>` and a time `minutes` after `DateTime(2026, 11, 3, 12, 0, 0, Utc)`.
    `kitA`..`kitE` are the slice's a (unstarred, 12:05), b (starred, 12:01),
    c (unstarred, 12:09), d (starred, 12:07) and e (unstarred, 12:05).
    `mixed = historyOf [ kitA; kitB; kitC; kitD; kitE ]`.
  - Six `[<Fact>]`s, named exactly as the census:
    1. `a kit star round-trips its wire tag`: the tags are 1 and 2, `tryOfWire` inverts them,
       and 0 and 3 answer `None`.
    2. `starred kits are listed first, each group newest open first`: `mixed.displayOrder` is
       d, b, c, a, e, and the stored order is unchanged. It also checks that
       `currentSchemaVersion` is 1 and that `empty` holds it with no entry and an empty display
       order.
    3. `star and unstar change only the named kit, matching its folder in any case`: `star` of
       `C:/KITS/A` equals `mixed` with only a starred. `unstar` of `C:/kits/a` on that equals
       `mixed`, and `star` of `C:/kits/zz` equals `mixed`.
    4. `forget removes only the named kit, starred or not`: `C:/Kits/B` leaves a, c, d, e, then
       `C:/kits/c` leaves a, d, e. `C:/kits/zz` equals `mixed`.
    5. `evict drops the oldest unstarred kits down to the limit`: limit 4 equals a, b, c, d (only
       e dropped), limit 3 equals b, c, d, and limit 5 equals `mixed`.
    6. `evict never drops a starred kit, even over the limit`: [b; c; d] at limit 1 equals
       [b; d], which is two entries. [b; d] at limit 1 is unchanged.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`:
  one line, `<Compile Include="KitHistoryTests.fs" />`, appended after `KitOpeningTests.fs` at
  the end of the compile list, with no comment.

## Testing state

As Invariant 6 requires, this IMPLEMENT worker ran no gates: no `dotnet build`, no
`dotnet test`, no analyzer and no invariant. The gate engine runs build, elevation,
constructor-unit-tests and the thirteen s19 invariants after exit. The only commands run were
read-only (reads, `git grep`, `git diff --numstat`, `file`) plus the no-op LF normalisation.

Expected: constructor-unit-tests +6 (six new facts, none changed or removed), from checkpoint
162's 1,138 to 1,144.

## Artifacts

None. The step produced no run artifacts.
