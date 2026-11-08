# Step 0170 — IMPLEMENT — impl-plan

## Approach

The start screen needs the kit history in its display order, narrowed by a filter box, with each row showing
whether its folder is on disk (spec 0050 S5.2). This step adds the pure half of that to
`OpticalConstructor.Domain/KitHistory.fs`. The screen comes later (step 176).

1. `KitHistoryFilter = KitHistoryFilter of string`, declared between `KitHistoryEntry` and `KitHistory`, with
   `member this.value` and `static member none = KitHistoryFilter ""`.
2. `KitHistory.entriesMatching (filter : KitHistoryFilter) : KitHistoryEntry list`, an intrinsic member:
   - start from `displayOrder` and keep the entries whose `name.value` or `folder.value.value` contains the
     filter's trimmed text, compared with `StringComparison.OrdinalIgnoreCase`;
   - a blank or white-space filter trims to `""`, which every string contains, so every entry is kept;
   - the order is always `displayOrder`'s.
3. `KitHistoryRow = { entry : KitHistoryEntry; presence : KitFolderPresence }`, declared after `KitFolderPresence`.
4. `type KitHistory with member this.rowsMatching (filter) (probe : KitFolder -> KitFolderPresence)`, declared after
   `KitHistoryRow`: it maps `entriesMatching filter` to rows, pairing each entry with `probe entry.folder`. The
   history stays pure, because the caller supplies the probe (step 176 passes `KitHistoryProxy.probeFolder`).

Three facts are added to `KitHistoryTests`, named exactly as the slice gives them, over the existing `mixed` history.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitHistory.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryTests.fs`

## Risks

- **Record-label inference.** `entry` is also a field of records in `CatalogueImport`, `MaterialStore` and
  `RayModel`. The row is therefore built only where its type is already known, through an annotated local
  `rowOf : … : KitHistoryRow`.
- **No local build** (invariant 6). Every expression copies a form that already compiles in the tree:
  - `Contains(…, StringComparison.OrdinalIgnoreCase)`, as `Facets.textFilterDef` uses it;
  - the single-case DU with a `.value` accessor, as `KitName` has it;
  - the intrinsic `type … with` extension, as `KitHistoryProxy` has it.
- **ELEV.** Every new name is unique in the tree:
  - `KitHistoryFilter` is both a type and its case;
  - `KitHistoryRow` is a type;
  - `entriesMatching` and `rowsMatching` are members.

  Every signature that speaks a primitive is a member on its own type. No `bool` appears in a signature or a
  record.
