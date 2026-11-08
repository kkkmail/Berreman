# Where we are

Step 0175 is an `IMPLEMENT` step in spec 0050 S5.2, the kit start screen and the Launcher. The app scope now composes
the kit history once, over the configured preferences folder (the S5.1 mapper's interpretation 12; .spec-md §0.2,
§3.9). The start screen (steps 176 onward) reads and bumps it through `AppContext.kitHistory`, and builds no proxy of
its own. Verification belongs to the arc-runner after this act-only worker exits.

# What's working

- Added `AppContext.kitHistory : KitHistoryProxy`, the record's last field.
- The private `compose` sets it to `KitHistoryProxy.create locations.preferencesFolder`, so `create` and
  `recreateLibrary` both carry it, and building it touches nothing.
- Added the `AppContextTests` fact `create composes the kit history over the configured preferences folder`:
  - it reads the empty history;
  - it writes one entry with `Ok ()`;
  - `kit-history.json` then lies in the configured preferences folder;
  - a new store over that folder reads the entry back equal.

# Tests

No gates were run: under invariant 6 this role does not run checks. The arc-runner still has to run:
- build;
- elevation;
- constructor-unit-tests, all discovered;
- ui-smoke, all discovered;
- ui-tests, `OpticalConstructor.Ui.Tests.AppContextTests`;
- the fourteen s20 section invariants.

The new fact is
`OpticalConstructor.Ui.Tests.AppContextTests.create composes the kit history over the configured preferences folder`.
The ui-tests count over `AppContextTests` should rise by 1. No constructor or ui-smoke test changed.

Nothing was measured here. The YAML repeats the latest recorded checkpoints (step 173: constructor 1,175, ui-smoke 224),
not a result of this round.

```yaml
gates:
  constructor_unit_tests: 1175
  ui_smoke_tests: 224
```

# Architecture

- **One composition.** The kit history is composed once per app scope, in `AppContext.compose`, the same place as every
  other proxy. Its folder comes from `DataLocations.preferencesFolder`, which settings resolve (§3.9).
- **No new fields.** No option, cache or limit was added. The eviction cap stays `settings.kitHistoryLimit`, applied by
  the caller of `KitHistory.bump`.
- **Test scopes.** Every test scope carries the history through `TestAppScope` unchanged, over its `Preferences` folder
  under the test output.

# Deferred

Nothing in this slice is deferred. The start screen that reads `kitHistory` belongs to its own later steps.

# Gotchas

- **The record doc line.** It now names the kit history among what the scope shares, so the type's contract matches its
  fields.
- **The extra assertion.** The new fact also asserts that the preferences folder does not exist after `create` and the
  first read. This is the observable form of "building it touches nothing".
- **`specs/0050/.manifest.state.json` belongs to the supervisor.** It was modified before this worker started and was
  not touched.

# Changelog

- 2026-11-08: Composed the kit history in `AppContext` over the configured preferences folder (`AppContext.kitHistory`)
  and added one `AppContextTests` fact. Verification is left to the arc-runner.
