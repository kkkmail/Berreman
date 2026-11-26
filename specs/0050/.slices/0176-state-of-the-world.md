# Where we are

Step 0176 is an `IMPLEMENT` step in spec 0050 S5.2, the kit start screen and the Launcher. It adds the start screen's
state and its history messages as a pure Elmish update over injected seams (001 §F 'The history list', §P StarKit /
UnstarKit / ForgetKit). The model reads and writes the kit history composed in step 0175 only through
`KitStartContext.kitHistory`.

Step 177 adds the open messages, which read the rest of the context. Step 179 adds the window. Verification belongs to
the arc-runner after this act-only worker exits.

# What's working

- Added `OpticalConstructor.Ui.KitStartView`, which holds:
  - `KitStartContext`, the injected seams;
  - `KitStartOffer` and `KitRelocation`;
  - `KitStartModel` and `KitStartMsg` (StarKit, UnstarKit, ForgetKit, LocateKit, FilterKitHistory).
- `initKitStart` reads the history once and lists its rows in display order, each with its folder's presence.
- `updateKitStart` writes star, unstar, forget and locate through `changeHistory` and re-lists the rows. A refused write
  keeps the rows and sets the status. The filter re-lists the rows and writes nothing.
- Added `KitHistoryError.notSavedCaption`, the start screen's caption for a failed history write.
- Added `KitStartTests`, five facts covering the opening listing, star / unstar / forget, locate, the filter and a store
  that refuses writes.

# Tests

No gates were run: under invariant 6 this role does not run checks. The arc-runner still has to run:
- build;
- elevation;
- constructor-unit-tests, all discovered;
- ui-smoke, all discovered;
- ui-tests, `OpticalConstructor.Ui.Tests.AppContextTests` and `OpticalConstructor.Ui.Tests.LauncherTests`;
- the fourteen s20 section invariants.

The five new facts are `OpticalConstructor.Tests.KitStartTests.`:
- `the start screen opens on the history in display order, each row with its folder's presence`;
- `starring, unstarring and forgetting a kit write the history and re-list it`;
- `locating a missing kit re-points its row and keeps its star`;
- `the filter narrows the rows and writes nothing`;
- `a history the store cannot write is shown and the rows are kept`.

The constructor count should rise by 5. No ui-smoke or ui-tests test changed.

Nothing was measured here. The YAML repeats the latest recorded checkpoints (step 174: constructor 1,175, ui-smoke 224),
not a result of this round.

```yaml
gates:
  constructor_unit_tests: 1175
  ui_smoke_tests: 224
```

# Architecture

- **The `LibraryWindowView` shape.** A `[<ReferenceEquality>]` context of injected seams, a model, semantic messages and
  an update that reaches IO only through the context. Tests substitute the in-memory history proxy, or a copy of it
  whose write records or refuses.
- **One listing rule.** The rows are always `history.rowsMatching filter context.kitHistory.probeFolder`. They are
  computed in one private function and never cached apart from the history they come from.
- **One write path.** The four history verbs share one private function over `changeHistory`. It answers either the
  written history, re-listed with no status, or the unchanged model with the failure's caption.
- **The rest of the context.** `limit`, `clock`, `author`, `imports`, `picker` and `showKit` are composed now, but this
  step's messages read only `kitHistory`. Step 177's open messages read the rest.

# Deferred

Nothing in this slice is deferred. The slice keeps the open messages (step 177) and the view and window (step 179) out
of scope.

# Gotchas

- **A duplicated test helper.** `KitStartTests.steppingClock` repeats the private helper in `KitHistoryTests`.
  `KitHistoryTests.fs` was outside this step's authorized edits, so it could not be shared.
- **The filter leaves the status alone.** `FilterKitHistory` does not clear `status`, so a failed write stays visible
  while the user filters.
- **Remove from history has no undo.** The folder is untouched, and opening it again re-adds it (.spec-md §10.5).
- **`specs/0050/.manifest.state.json` belongs to the supervisor.** It was modified before this worker started and was
  not touched.

# Changelog

- 2026-11-08: Added the kit start screen's pure model and its history messages (`KitStartView`),
  `KitHistoryError.notSavedCaption` and five `KitStartTests` facts. Verification is left to the arc-runner.
