# Where we are

Step 0164 continues S5.1, the kit history over the per-user preferences.
- Step 162 added `KitHistoryLimit`, and step 163 carried it in `WorkbenchSettings` and read it
  from `appsettings.json`.
- This step adds the history itself as pure Domain data: the star, eviction and display order of
  001 §F "History" and "Storage" and .spec-md §3.17.
- `KitHistory.bump` and the injected-clock `lastOpened` arrive in step 169. That step moves the
  MRU rule.

# What's working

- Add `OpticalConstructor.Domain/KitHistory.fs` with `KitStar` (`StarredKit` / `UnstarredKit`,
  int wire tag 1 / 2, `tryOfWire`), `KitHistoryEntry` and `KitHistory` (schema version 1, `empty`)
- Star, unstar and forget a kit by its folder, matched ordinal-ignoring-case as the MRU list
  matches paths, through one private comparison member
- `evict` drops the oldest unstarred kits (the later of equal times first) down to the limit and
  never drops a starred one; `displayOrder` lists stars first, each group newest open first
- Add `KitHistoryTests` with the six facts the slice names

# Tests

As Invariant 6 requires, this worker ran no gates: `gates_run: []`, `gate_results: {}`. The gate
engine runs build, elevation, constructor-unit-tests and the thirteen s19 invariants.

Expected effects:

- constructor-unit-tests: +6, all new facts with none changed or removed, from checkpoint 162's
  1,138 to 1,144.
- ui-smoke is not in this step's roster and is unchanged at 224.

```yaml
gates:
  constructor_unit_tests: 1144
  ui_smoke_tests: 224
```

# Architecture

- `KitHistory` is a record with members, like `KitFile`, and lives in `module KitHistory`, as
  `WorkbenchSettings` does. Every operation answers a new history and changes nothing else. The
  only state is `entries`, kept newest open first.
- There is exactly one folder comparison: `member private this.changeEntry`. It maps the
  matching entry through `KitHistoryEntry -> KitHistoryEntry option`, and `None` drops the entry.
  `star`, `unstar` and `forget` are one line each over it.
  - The signature names no primitive, so a predicate's `bool` never sits beside `KitFolder` or
    `KitHistoryEntry`. ELEV010 checks private signatures too, under ELEV010P.
- `evict` picks its victims from the unstarred entries only, by `(lastOpened, -index)`. The
  survivors are filtered by index, so their stored order is kept by construction.
- `displayOrder` relies on FSharp.Core's documented stable `List.sortByDescending`.

# Deferred

Nothing in this step's scope. `bump` and the history's reader, writer and UI belong to later S5.1
/ S5.2 steps (169 onward), as the slice states.

# Gotchas

- The slice asks for the comparison as "a private member of `KitHistory`" with "no public member
  answering a `bool`". A private member answering `bool` would still be an ELEV010P finding, so the
  comparison is the option-answering `changeEntry`, not a predicate. Step 0163 attempt 02
  measured the same private-surface rule.
- `star` / `unstar` keep the STORED folder text, not the casing the caller passed: starring
  `C:/KITS/A` leaves the entry's folder as `C:/kits/a`.
- `evict` truncates to `max 0 (entries.Length - limit.value)`. A history already within the
  limit is returned equal, and one with more stars than the limit keeps every star.
- A `List.truncate` with a negative count answers `[]`. The `max 0` keeps that from mattering.
- `StarredKit.value` on a bare case works in the tests, as `SourceEntryKind.value` does in
  `CanonicalJsonTests`.

# Changelog

- 2026-11-08: Step 0164 added the kit history model: `KitStar`, `KitHistoryEntry` and
  `KitHistory`, with star / unstar / forget by folder, `evict` and `displayOrder`, and the six
  `KitHistoryTests` facts.
