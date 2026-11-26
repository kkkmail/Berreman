# Step 0164 — implementation plan

The step adds the kit history as pure Domain data: its star, its eviction and its display order
(001 §F "History" and "Storage"; .spec-md §3.17; S5.1).

## Approach

1. **Domain.** Add `OpticalConstructor.Domain/KitHistory.fs`: `module KitHistory` in
   `namespace OpticalConstructor.Domain`. It is compiled directly after `KitFolders.fs`, with one
   `<Compile>` line and no comment. It opens `Kit`, `LibraryEntryFiles` and `WorkbenchSettings`.
   - `KitStar = StarredKit | UnstarredKit`. `value : int` answers 1 or 2, and
     `tryOfWire (tag : int) : KitStar option` reads the tag back. Both are elevation-pattern
     members.
   - `KitHistoryEntry = { folder; kitId; name; lastOpened; star }`, every field elevated.
   - `KitHistory = { schemaVersion; entries }`, with `currentSchemaVersion = FileSchemaVersion 1`
     and `empty`.
   - The folder comparison is written once, as a private member: `changeEntry`.
     - It compares with `StringComparison.OrdinalIgnoreCase` over the folder text, as
       `RecentFiles.bump` does.
     - It applies `KitHistoryEntry -> KitHistoryEntry option` to the matching entry, where `None`
       drops the entry.
     - `star`, `unstar` and `forget` are one line each over it.
     - Its signature names no primitive, so no `bool` sits beside an elevated type. ELEV010
       checks private members too, under ELEV010P, as step 0163 attempt 02 measured.
   - `evict (limit : KitHistoryLimit)` keeps every starred entry and drops unstarred ones until
     the history is at or under the limit, or no unstarred entry is left.
     - The unstarred entries are indexed and sorted by `(lastOpened, -index)`: the earliest open
       goes first, and among equal times the later stored entry goes first.
     - The excess, floored at 0, is dropped from the front of that order.
     - The survivors keep their stored order.
   - `displayOrder` lists the starred entries, then the unstarred ones. Each group is sorted by
     `List.sortByDescending _.lastOpened`, which is stable, so equal times keep their stored order.
2. **Tests.** Add `OpticalConstructor.Tests/KitHistoryTests.fs`: `module KitHistoryTests` in
   `namespace OpticalConstructor.Tests`, appended at the end of the compile list with no comment.
   It holds the six facts the slice names, verbatim, over the slice's `mixed` fixture: kit `x`
   is `C:/kits/x`, with a fixed GUID, the name `Kit x`, and minutes after
   2026-11-03 12:00 UTC.

## Risks

- ELEV010P on a private `bool` comparison. Avoided by the option-answering `changeEntry`.
- Record-label ambiguity: `name` and `folder` are common field names. Every fixture entry and
  history is built by a function or value with a type annotation.
- No gate is run by this worker (Invariant 6), so the code is written against the existing
  patterns: `LibraryEntryKind.tryOfWire`, `WorkbenchSettings`, `KitTests`.
