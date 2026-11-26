# Where we are

Step 0171 is an `IMPLEMENT` step in spec 0050 S5.2, the kit start screen. Section S5.1 built the pure kit history:
its order, star, forget, evict and `bump`, plus the proxy, the folder probe and the JSON store. Step 170 added the
filtered rows. This step adds the two verbs the start screen still needs:
- `relocate`, Locate…'s answer for a missing folder;
- `KitHistoryProxy.changeHistory`, which writes the read-change-write sequence once.

Step 176's star, unstar, forget and locate and step 177's bump will all run through `changeHistory`. Verification
belongs to the arc-runner after this act-only worker exits.

# What's working

- Added `KitHistory.relocate missing found`. The kit in `missing` takes `found` as its folder and keeps its id, name, last open, star and place, and any other entry of `found` is dropped. A history without `missing` is unchanged.
- Added `KitHistoryProxy.changeHistory change`. It reads the stored history, applies `change`, writes the result, and answers it or the write's error.
- Added two `KitHistoryTests` relocate facts, and the `changeHistory` contract fact, which the in-memory and folder stores both run.
- Added the folder fact: a file where the preferences folder belongs gives `KitHistoryWriteErr` naming it, and the file's bytes are kept.

# Tests

No gates were run: under invariant 6 this role does not run checks. The arc-runner still has to run:
- build and elevation;
- constructor-unit-tests, all discovered;
- the fourteen s20 section invariants.

The new facts are named exactly as the slice gives them:
- `KitHistoryTests.relocate moves a kit's entry to the found folder and keeps its star, name, time and place`;
- `KitHistoryTests.relocate onto a folder the history holds keeps one entry for that folder`;
- `KitHistoryProxyTests.changing the history writes the change and answers what was written`;
- `KitHistoryProxyFolderTests.changing the history writes the change and answers what was written`;
- `KitHistoryProxyFolderTests.a change the preferences folder cannot take is KitHistoryWriteErr, never a throw`.

The count should rise by 5. The latest recorded checkpoint is 169, at 1,166. Step 170 added 3, so this step should
give 1,174. Nothing was measured here. The YAML below repeats the recorded checkpoint, not a result of this round.

```yaml
gates:
  constructor_unit_tests: 1166
```

# Architecture

- **`relocate` is an intrinsic member beside `forget`.** It is pure and touches no disk; the row's presence is
  probed when the screen lists it again (step 176).
  - It matches folders only through the private `tryMatchFolder`.
  - The moved entry is identified by its index, so it survives even when `missing` and `found` name the same folder.
- **`changeHistory` is a member in the existing intrinsic `type KitHistoryProxy with` block, after
  `createInMemory`.**
  - It composes the proxy's own `readHistory` and `writeHistory`; no proxy field, retry, cache or lock was added.
  - It is unique on its receiver beside `createInMemory` and Storage's `create` (ELEV019).

# Deferred

Nothing in this slice is deferred. The start screen's Star, Unstar, Remove from history and Locate… actions are
step 176's, and the open-time bump through `changeHistory` is step 177's.

# Gotchas

- **`changeHistory` with a broken store.** When the history file cannot be read, `readHistory` answers the empty
  history, so a change is then applied to empty and that result is written. This is the store's existing
  "never fails on read" contract (step 165), not new behaviour.
- **`grep -c $'\r'` in Git Bash miscounts CRs on these files.** Check line endings with
  `git ls-files --eol` or a byte count. All three edited files are LF only.
- **`specs/0050/.manifest.state.json` belongs to the supervisor.** It was already modified when this worker
  started, and it was not touched.

# Changelog

- 2026-11-08: Added `KitHistory.relocate` and `KitHistoryProxy.changeHistory`, with two `KitHistoryTests` facts, a
  contract fact run by both proxy test classes and a folder-store failure fact. Verification is left to the
  arc-runner.
