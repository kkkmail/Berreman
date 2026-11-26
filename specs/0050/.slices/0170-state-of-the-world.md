# Where we are

Step 0170 is an `IMPLEMENT` step in spec 0050 S5.2, the kit start screen. Section S5.1 built the pure kit history:
its display order (164), the proxy and folder probe (165), the JSON store (166–168), and `bump` (169). This step adds
the pure half of the start screen's history list:
- a filter typed into the filter box;
- the display order narrowed by that filter;
- one row per listed kit, carrying whether its folder is on disk.

Step 176 lists these rows on the screen, using `KitHistoryProxy.probeFolder` as the probe. Verification belongs to
the arc-runner after this act-only worker exits.

# What's working

- Added `KitHistoryFilter`, the start screen's filter-box text, with `.value` and `KitHistoryFilter.none` (the empty box).
- Added `KitHistory.entriesMatching`: the display order narrowed to the kits whose name or folder holds the trimmed filter, ignoring case. A blank filter keeps every kit.
- Added `KitHistoryRow` and `KitHistory.rowsMatching`, which pair each matching kit with a caller-supplied folder probe, so the history stays pure.
- Added three `KitHistoryTests` facts covering name and folder matches, the blank filter, and row presence.

# Tests

No gates were run: under invariant 6 this role does not run checks. The arc-runner still has to run:
- build and elevation;
- constructor-unit-tests, all discovered;
- the fourteen s20 section invariants.

`OpticalConstructor.Tests.KitHistoryTests` gains three facts, named exactly as the slice gives them:
- `entriesMatching keeps the kits whose name or folder holds the filter, ignoring case`;
- `a blank filter keeps the whole display order`;
- `rowsMatching pairs each listed kit with its folder's presence`.

The count should rise by 3. The latest recorded checkpoint is 168, at 1,163; step 169 added a net 3, so this step
should give 1,169. Nothing was measured here. The YAML below repeats the recorded checkpoint, not a result of this
round.

```yaml
gates:
  constructor_unit_tests: 1163
```

# Architecture

- `entriesMatching` is an intrinsic member that filters `displayOrder`. The display order is therefore still defined
  once, and the filter cannot reorder anything.
- A blank or white-space filter trims to `""`. Every string contains `""`, so the blank case needs no branch of its
  own.
- `rowsMatching` is an intrinsic `type KitHistory with` extension, because `KitHistoryRow` and `KitFolderPresence`
  are declared after `KitHistory`. It takes the probe as a function, so the domain does no IO.
- There is no new `bool`, sort option, word splitting or cache. Every primitive is spoken by a member of its own
  type.

# Deferred

Nothing in this slice is deferred. The screen that lists the rows, with greyed missing folders and the filter box,
is step 176's.

# Gotchas

- **`entry` is a shared record label.** Records in `CatalogueImport`, `MaterialStore` and `RayModel` also have an
  `entry` field. Build a `KitHistoryRow` where its type is known (an annotated binding), as `rowsMatching` and the
  test do.
- **The test probe is not the in-memory double.** It is spelled out locally, because the slice phrases it as
  "missing for b and e, present for every other folder". `KitHistoryProxy.createInMemory` takes the present folders
  instead.
- **`KitHistoryFilter` is deliberately separate from `Facets.TextQuery`.** The slice names it as a contract symbol.
  Do not merge them without a ruling.
- **`specs/0050/.manifest.state.json` belongs to the supervisor.** It was already modified when this worker started,
  and it was not touched.

# Changelog

- 2026-11-08: Added `KitHistoryFilter`, `KitHistory.entriesMatching`, `KitHistoryRow` and `KitHistory.rowsMatching`,
  with three `KitHistoryTests` facts, and left verification to the arc-runner.
