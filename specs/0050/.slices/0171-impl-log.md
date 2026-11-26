# Step 0171 — IMPLEMENT — impl-log

## Progress

- [x] Read the protocol (base and `IMPLEMENT` delta), the project prompt, the slice and its gate snapshot, manual
  013, `KitHistory.fs`, `KitHistoryStore.fs`, `KitHistoryTests.fs`, `KitHistoryProxyTests.fs`, and step 170's log
  and SoW. The project prompt's operator note is empty.
- [x] `KitHistory.fs`: `relocate`, `changeHistory`.
- [x] `KitHistoryTests.fs`: two relocate facts.
- [x] `KitHistoryProxyTests.fs`: the contract fact and the folder fact.
- [x] LF check: Python finds 0 CR bytes in all three files. `git ls-files --eol` shows `i/lf w/lf` for each, and
  `git diff --numstat` agrees with `--ignore-cr-at-eol` (+29, +24, +23).
- [x] State of the world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitHistory.fs`
  - **New intrinsic member on `KitHistory`, after `forget`:**
    `member this.relocate (missing : KitFolder) (found : KitFolder) : KitHistory`. Its doc is three lines citing
    spec 0050 S5.2.
    - It indexes `entries` and picks the first entry matching `missing` through `KitHistory.tryMatchFolder`, which
      stays the one private comparison.
    - None found: it answers `this`.
    - Found: at that index the entry is `{ moved with folder = found }`. Every other index whose entry matches
      `found` is dropped, and every other entry is kept in place.
  - **New member in the existing `type KitHistoryProxy with` block, after `createInMemory`:**
    `member this.changeHistory (change : KitHistory -> KitHistory) : Result<KitHistory, KitHistoryError>`. Its doc
    is two lines citing spec 0050 S5.2.
    - It is `let changed = change (this.readHistory ())`, then
      `this.writeHistory changed |> Result.map (fun () -> changed)`.
    - No retry, cache, lock or proxy field.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryTests.fs`: two facts after the `forget` fact, over
  `mixed`.
  - `relocate moves a kit's entry to the found folder and keeps its star, name, time and place`:
    - `mixed.relocate (folderOf "C:/KITS/B") (folderOf "D:/moved/b")` is called.
    - The second entry has folder `D:/moved/b`, `kitB`'s id, `StarredKit`, `KitName "Kit b"` and last open
      2026-11-03 12:01 UTC.
    - The whole history equals `[ kitA; { kitB with folder = D:/moved/b }; kitC; kitD; kitE ]`.
  - `relocate onto a folder the history holds keeps one entry for that folder`:
    - `C:/kits/a` → `C:/kits/c` answers `[ { kitA with folder = C:/kits/c }; kitB; kitD; kitE ]`.
    - Exactly one entry is at `C:/kits/c`, and there are four entries in all.
    - `C:/kits/zz` → `C:/kits/c` answers `mixed` unchanged.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryProxyTests.fs`:
  - **`KitHistoryProxyContract`** gains `changing the history writes the change and answers what was written`.
    - It writes `twoEntries`, then calls `changeHistory (fun h -> h.unstar (KitFolder (FolderName "C:/kits/a")))`.
    - The answer is `Ok` of `twoEntries` with `kitA` unstarred, and `readHistory ()` answers the same.
    - `KitHistoryProxyTests` and `KitHistoryProxyFolderTests` both inherit it.
  - **`KitHistoryProxyFolderTests`** gains `a change the preferences folder cannot take is KitHistoryWriteErr, never
    a throw`.
    - It writes known bytes to a FILE at `<root>/Preferences`, then calls
      `changeHistory (fun h -> h.star (KitFolder (FolderName "C:/kits/a")))`.
    - It expects `Error (KitHistoryWriteErr reason)` with `reason` containing `preferences.value`, and the file's
      bytes unchanged.

## Decisions

- **The moved entry is identified by its index, not by re-matching.** If `missing` and `found` name the same folder
  (for example only the case differs), "drop every entry of `found`" would otherwise also drop the moved entry. With
  the index check, the moved entry always survives in its place.
- **The `C:/kits/zz` case relocates onto the held `C:/kits/c`.** The slice says only "relocating `C:/kits/zz`
  answers `mixed` unchanged", so the target was mine to choose. A held folder is the stronger check: without a kit in
  `missing`, nothing is dropped even when `found` is listed.
- **How the folder test fails.** With a file at `Preferences`, `File.Exists "<Preferences>/kit-history.json"` is
  false, so `readHistory ()` answers the empty history; starring it changes nothing. `writeHistory` then fails in
  `createPreferences` with `the preferences folder '<path>': …`, the mechanism the existing write test already
  covers.
- **`changeHistory` sits in the existing `type KitHistoryProxy with` block.** That block is an intrinsic
  augmentation in the type's own module, so the member is visible wherever the type is, with no extra `open`.
  `changeHistory` is unique on its receiver: `git grep` finds no other `changeHistory` or `relocate` under
  `Berreman/`.
- **Reuse.** `relocate` reuses `tryMatchFolder`. `changeHistory` composes the proxy's own `readHistory` and
  `writeHistory`. Nothing is copied.

## Testing state

- Under invariant 6 this `IMPLEMENT` role runs no checks, so no gate was run: no `dotnet build`, no `dotnet test`,
  no analyzer, no invariant grep.
  - Because nothing compiled locally, every form copies one already compiling in `KitHistory.fs`:
    - `List.indexed` with tuple-pattern lambdas, as `evict` does;
    - `List.choose`, as `changeEntry` does;
    - `Result.map`, as `KitHistoryStore` does;
    - a `match` over a two-element tuple.
  - The test lambdas are annotated `(history : KitHistory)`.
  - The arc-runner still has to run build, elevation, constructor-unit-tests (all discovered) and the fourteen s20
    invariants.
- **Expected count.** constructor-unit-tests rises by 5:
  - 2 in `KitHistoryTests`;
  - 1 contract fact, run by 2 derived classes;
  - 1 in `KitHistoryProxyFolderTests`.

  The last recorded checkpoint is 169, at 1,166. Step 170 added 3, so 1,174 is expected. Nothing was measured.

## Artifacts

None.
