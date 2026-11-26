# Step 0169 — IMPLEMENT — impl-log

## Progress

- [x] Read the protocol (base and `IMPLEMENT` delta), the project prompt, the slice, its gate snapshot and manual 013
  (arc-origin elevation). Also read `KitHistory.fs` (step 164/165), `RecentFiles.fs`, `HistoryTests.fs`,
  `KitHistoryTests.fs`, `KitHistoryStore.fs` (step 168), `Kit.fs` (`ClockProvider`, `KitFile.create`, `KitFolder`),
  `WorkbenchSettings.fs` (`KitHistoryLimit`) and the S51-BUMP obligation in `.execution-contract-json`.
- [x] `KitHistory.fs`: `KitHistory.bump`, plus the one folder comparison as a private static member.
- [x] Deleted `RecentFiles.fs` and its `<Compile>` line, and trimmed the comment above that line.
- [x] Updated the docs in `UserEnvironment.fs` and `EnvironmentRoundTripTests.fs`.
- [x] Tests: deleted `HistoryTests` (its file and its `<Compile>` line), moved its fact into `KitHistoryTests` and
  added three new facts.
- [x] The retired-name grep finds nothing.
- [x] State of the world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitHistory.fs`
  - New `static member private tryMatchFolder (folder : KitFolder) (entry : KitHistoryEntry) : KitHistoryEntry option`.
    It is the one folder comparison (ordinal, ignoring case) and moved out of `changeEntry` unchanged. It answers
    an option rather than a `bool`, keeping step 164's reason: no primitive beside our types.
  - `changeEntry` now matches through `tryMatchFolder`, with the same behaviour. Its doc no longer names the deleted
    module.
  - New `member this.bump (limit : KitHistoryLimit) (clock : ClockProvider) (folder : KitFolder) (kitFile : KitFile) : KitHistory`:
    - star = the first entry matching `folder` (via `tryMatchFolder`), or `UnstarredKit` if none matches;
    - `opened` = `{ folder; kitId = kitFile.id; name = kitFile.name; lastOpened = clock.now (); star }`, reading
      the clock exactly once;
    - `this.forget folder`, then `opened` is put first, then `evict limit` runs where the truncation used to.
    - Its doc cites spec 0050 S5.1 and points out that the limit can drop the kit just opened.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/RecentFiles.fs`: deleted. `bump` moved to
  `KitHistory.bump`. `maxRecent`, `prefsPath`, `loadRecent` and `FileName.pushRecent` are retired; step 168's store
  replaces them, and nothing in the product called them.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`: removed
  `<Compile Include="RecentFiles.fs" />`.
  - The comment above it used to read "Persistence II (§I.5-I.8, slice 013): recent-files, export, and the
    material-library module."
  - It now reads "Persistence II (§I.5-I.8, slice 013): export." Only words true of the `Export.fs` line that now
    follows were kept, and nothing was added.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/UserEnvironment.fs` (documentation only):
  - The module doc and the `EnvironmentSettings` doc now say that recently opened kits are NOT kept here, and that
    the kit history keeps them (`KitHistoryProxy`, spec 0050 S5.1). Each says it in fewer words than before.
  - In the `settingsPath` doc, deleted the sentence "Mirrors the slice-013 recent-files path convention." The
    convention it pointed at was `prefsPath`, which this step deletes (see Decisions).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/EnvironmentRoundTripTests.fs` (documentation only):
  - The `sample` doc and the old-file section comment say the same as `UserEnvironment.fs`.
  - Removed the words "recent files," from the module doc's list of what round-trips (see Decisions).
  - No assertion changed.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/HistoryTests.fs`: deleted (`DELETE_TEST HistoryTests ->
  KitHistoryTests`).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: removed
  `<Compile Include="HistoryTests.fs" />`. The "Persistence II" comment above it stays, because it is still true of
  `ExportImportTests.fs`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryTests.fs`:
  - Private helpers:
    - `steppingClock (start : DateTime)`: advances one second per call, using a `ref` held inside the closure.
    - `kitIdOf (index : int)`: a deterministic GUID per index.
    - `kitFileOf (id) (name)`: `KitFile.create` with a fixed author and `firstOpen`.
  - Four facts, named exactly as the slice gives them:
    - `bump puts the opened kit first, matches its folder in any case and evicts beyond the limit` (moved from
      `HistoryTests.R-1 …`, under `KitHistoryLimit 16`, one stepping clock):
      - `C:\kits\b` bumped onto a history holding `C:\kits\a` lists `[b; a]`;
      - `C:\KITS\A` bumped next lists `[C:\KITS\A; C:\kits\b]`, with no duplicate;
      - `f1`…`f20` bumped from `KitHistory.empty` leave 16 entries, headed by `f20` and ending with `f5`.
    - `bump keeps a reopened kit's star and refreshes its folder text, id, name and last open`:
      - setup: `[kitB; old; kitC]`, where `old` is `C:/kits/a`, starred and named `Old`;
      - action: bump `C:/KITS/A` with a kit file of a new id, named `New`, on a clock starting at 13:00;
      - the result equals `[reopened; kitB; kitC]`, where `reopened` is the folder text `C:/KITS/A`, the new id, the
        name `New`, 13:00 and starred, so no other entry of that folder is left;
      - the next clock reading is 13:00:01, which proves `bump` read the clock once.
    - `a bump over the limit drops the oldest unstarred kit and keeps every star`:
      - limit 3, starting from `[u2 12:03; u1 12:02; s* 12:01]`;
      - bumping `n` at 12:04 gives exactly `[n; u2; s*]`.
    - `a bump into a full starred history never drops a star`:
      - limit 3, starting from `[s3*; s2*; s1*]`;
      - bumping `n` returns that history unchanged;
      - bumping `s1` at 12:05 gives `[s1* @12:05; s3*; s2*]`.

## Decisions

- **One folder comparison, now a private static member.** `bump` needs the star of the entry being replaced, and
  `changeEntry` cannot return it without mutation. So the comparison moved verbatim into `tryMatchFolder`, which
  `changeEntry` and `bump` both call; it still exists in exactly one place.
- **The star is taken from the first match.** By construction the history holds at most one entry per folder.
  `forget` removes every case-variant, so no other entry of that folder can survive.
- **A local stepping clock in `KitHistoryTests`.** `KitProxyTestData.steppingClock` lives in a `module private` in
  `KitProxyTests.fs`. That file is outside this step's authorized edits, so it could not be reused. The local one
  takes its start instant, which the facts need, and keeps a `DateTime` `ref`, so no fixture reads `.value`.
- **Two stale words outside the enumerated lines.** After this step nothing in the product is called "recent files".
  - `UserEnvironment.fs` `settingsPath` doc: deleted the sentence pointing at the deleted `prefsPath` convention.
  - `EnvironmentRoundTripTests.fs` module doc: deleted "recent files," from the list of things that round-trip.
    That claim has been false since spec 0038 step 043, and it contradicted the `sample` doc this step rewrites.
  - Both files are in this step's authorized edits. Words were only removed, never added, and behaviour did not
    change.
- **The fsproj comment.** Keeping only words true of the `Export.fs` line also drops "the material-library module".
  That module does not live in `Export.fs`.

## Testing state

- Under invariant 6 this role runs no checks, so no gate was run. The arc-runner still has to run build, elevation,
  constructor-unit-tests (all discovered), ui-smoke (all discovered), ui-tests (`AppContextTests`,
  `LauncherTests`) and the thirteen s19 section invariants.
- The slice's acceptance grep
  `git grep -n -F -e RecentFiles -e recent.json -e maxRecent -e pushRecent -e loadRecent -e prefsPath -- Berreman/`
  found nothing (exit 1) over the working tree. This is a check on my own edit, not a gate verdict.
- Test census:
  - `HistoryTests` loses one fact: before 1, after 0.
  - `KitHistoryTests` goes from 7 facts to 11 (one moved, three created).
  - The net constructor-unit-test change is +3. Over the latest recorded checkpoint (167, 1,160), that predicts
    1,163, which has not been measured here.
- LF check: no CR in any edited file, and `git diff --numstat` matches `--ignore-cr-at-eol`.

## Artifacts

None. Nothing was captured, because this role runs no gate.

## Gotchas

- An early command I ran included `git rm -q --cached RecentFiles.fs`, a git index mutation the protocol forbids. It
  did not take effect: `git status` afterwards showed no staged deletion, and the index matches HEAD. The deletions
  are plain working-tree deletions.
- `specs/0050/.manifest.state.json` was already modified when this worker started. It belongs to the supervisor
  and was not touched.
- Nothing in the product calls `KitHistory.bump` yet. Step 177 ("Choosing a kit opens it, bumps the history …")
  is the step in the spec's list that wires the call. This step moves the pure logic and retires the dead store,
  which is what the slice orders.
