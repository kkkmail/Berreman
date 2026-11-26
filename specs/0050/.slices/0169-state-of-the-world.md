# Where we are

Step 0169 is the `IMPLEMENT` step of spec 0050 S5.1 that retires the 16-entry `recent.json` store. Earlier steps
built the kit history: the pure history (164), the proxy seam and its double (165), the canonical JSON and schema
(166–167), and the folder store over the per-user preferences (168). This step moves the pure MRU update into the
history as `KitHistory.bump`, deletes the old Storage module and its test, and points the docs at the kit history.
Step 177 wires opening a kit to `bump`. Verification belongs to the arc-runner after this act-only worker exits.

# What's working

- Moved the MRU update into `KitHistory.bump`: the opened kit goes first with its star kept and its folder text, id, name and time refreshed.
- `bump` matches the folder in any case, reads the clock once, then evicts over `KitHistoryLimit`, dropping the oldest unstarred kit and never a star.
- Deleted the recent-files Storage module (`bump`, `maxRecent`, `prefsPath`, `loadRecent`, `pushRecent`) and its compile entry.
- Replaced `HistoryTests` with four `KitHistoryTests` facts: one moved, three new.
- Pointed the environment docs at the kit history (`KitHistoryProxy`, spec 0050 S5.1); no behaviour or assertion changed.

# Tests

No gates were run: under invariant 6 this role does not run checks. The arc-runner still has to run:
- build and elevation;
- constructor-unit-tests and ui-smoke, all discovered;
- ui-tests over `AppContextTests` and `LauncherTests`;
- the thirteen s19 section invariants.

Constructor-unit-test identities change as follows:
- `OpticalConstructor.Tests.HistoryTests` is removed: one fact, moved to its successor.
- `OpticalConstructor.Tests.KitHistoryTests` gains the four facts named in the slice:
  - `bump puts the opened kit first, matches its folder in any case and evicts beyond the limit` (moved);
  - `bump keeps a reopened kit's star and refreshes its folder text, id, name and last open`;
  - `a bump over the limit drops the oldest unstarred kit and keeps every star`;
  - `a bump into a full starred history never drops a star`.

The net change is +3. That predicts 1,163 over the latest recorded checkpoint (167, 1,160), but nothing was
measured here. The YAML below repeats that recorded checkpoint, not a result of this round.

The slice's acceptance grep
(`git grep -n -F -e RecentFiles -e recent.json -e maxRecent -e pushRecent -e loadRecent -e prefsPath -- Berreman/`)
finds nothing over the working tree.

```yaml
gates:
  constructor_unit_tests: 1160
```

# Architecture

- `KitHistory.bump` is an intrinsic member taking only our types (`KitHistoryLimit`, `ClockProvider`,
  `KitFolder`, `KitFile`). It composes the existing `forget` and `evict`, so there is no second eviction or
  removal rule.
- The one case-insensitive folder comparison moved verbatim out of `changeEntry` into the private static
  `tryMatchFolder`, which answers `KitHistoryEntry option`. Both `changeEntry` and `bump` use it, so the folder
  identity is still defined once.
- Nothing stored changes: `recent.json` was never migrated, and `kit-history.json` (step 168) starts empty, so
  there is nothing to migrate.

# Deferred

Nothing in this slice is deferred. Step 177 calls `KitHistory.bump` when a kit is opened; the spec's step list
gives that step the wiring.

# Gotchas

- The limit is the history's ceiling, so a bump into a history whose other kits are all starred, and already at
  the limit, drops the kit just opened. `a bump into a full starred history never drops a star` pins this
  behaviour; do not "fix" it.
- `KitHistoryTests` has its own `steppingClock`, because `KitProxyTestData` is a `module private` in a file outside
  this step's edits. A future step that shares the helper should move it, not copy it again.
- Beyond the enumerated doc lines, two stale phrases about recent files were removed: the `settingsPath` doc in
  `UserEnvironment.fs` and the module doc of `EnvironmentRoundTripTests.fs`. Words were only removed.
- `specs/0050/.manifest.state.json` was already modified when this worker started. It belongs to the supervisor
  and was not touched.

# Changelog

- 2026-11-08: Moved the MRU bump into `KitHistory.bump`, deleted the recent-files store and `HistoryTests`, added
  the four `KitHistoryTests` bump facts, pointed the environment docs at the kit history, and left verification to
  the arc-runner.
