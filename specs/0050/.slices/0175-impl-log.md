# Step 0175 — impl log

## Progress

- [x] `OpticalConstructor.Ui/AppContext.fs`:
  - the record gains `kitHistory : KitHistoryProxy` as its last field. Its one-line doc reads: the kit history the
    start screen lists and bumps, in the per-user preferences (001 §F, spec 0050 S5.2);
  - the private `compose` (steps 89, 100) sets it to `KitHistoryProxy.create locations.preferencesFolder` (Storage's
    `KitHistoryStore`, step 168). So `create` and `recreateLibrary` both carry it, and building it touches nothing;
  - two opens, in the file's alphabetical order: `OpticalConstructor.Domain.KitHistory` (the proxy type) and
    `OpticalConstructor.Storage.KitHistoryStore` (its `create` extension);
  - the type's doc line now names the kit history among what the scope shares.

  No other field, option or cache is added. The cap stays `settings.kitHistoryLimit` (step 163).
- [x] `OpticalConstructor.Ui.Tests/AppContextTests.fs` gains
      `create composes the kit history over the configured preferences folder`. It runs over
      `TestAppScope.fileLocations owner` (step 88), composed by `AppContext.create`:
  - `context.kitHistory.readHistory ()` answers `KitHistory.empty`, and the preferences folder still does not exist,
    so composing and reading create nothing;
  - a one-entry history (a starred entry over `TestAppScope.freshKitFolder owner`) written through
    `context.kitHistory.writeHistory` answers `Ok ()`;
  - the locations' `preferencesFolder` then holds exactly `kit-history.json`;
  - a new `KitHistoryProxy.create locations.preferencesFolder` reads the history back equal.

  The test file opens `OpticalConstructor.Domain.KitHistory` and `OpticalConstructor.Storage.KitHistoryStore`.

`TestAppScope` is unchanged. Every test composition already gets the history through it, over `testLocations`'
`Preferences` folder under the test output (step 83).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs` (+6 / −1).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/AppContextTests.fs` (+28).

Both files are LF. `git diff --numstat` equals `git diff --numstat --ignore-cr-at-eol` for both.

## Testing state

No gate was run, because under invariant 6 the `IMPLEMENT` worker acts and runs no checks. The arc-runner's gate engine
runs the roster from `0175.gates`:
- build;
- elevation;
- constructor-unit-tests, all discovered (no constructor test changes);
- ui-smoke, all discovered;
- ui-tests, `OpticalConstructor.Ui.Tests.AppContextTests` (expected to rise by 1, the new fact);
- the fourteen s20 section invariants.

What was checked by reading the code, not by running it:
- `compose` holds the only full `AppContext` record literal. Every other site is a copy-and-update
  (`LauncherTests`, `WireUiFinalCompositionTests`), so the new field breaks no other construction.
- Nothing reflects over `AppContext`'s fields: there is no `GetRecordFields` or `typeof<AppContext>` in the
  constructor tree.
- Nothing else in the create path reads `preferencesFolder`. So the test's "folder not created before the first write"
  assertion depends only on `KitHistoryProxy.create` building lazily (its doc: building it touches nothing).
- Opening the `KitHistory` module adds record labels (`name`, `folder`, `entry`, …). Every existing record expression and
  field access in both files has a known type: annotations, pipelines or copy-and-update. `Kit.KitFile`'s `name` was
  already opened after `ExperimentExport`, and the series literal compiled with it.
- The fixture entry satisfies `kit-history.schema.json`: a non-empty folder, a GUID kit id, a non-blank name, and the
  star tag 1.

## Artifacts

None. Nothing was captured, because no check was run.

## Gotchas

- **The type doc line.** The slice gives only the field's doc. I also updated the record's own doc line ("Shares …
  exports, settings and the kit history across windows"), because otherwise the type's contract would leave out a
  field it now carries. This is inside the authorized file and adds no field, option or cache.
- **The extra assertion.** The test checks that the preferences folder is absent after `create` and the first
  `readHistory`. The slice states that building the history touches nothing, and this assertion is the observable proof
  of it.
- **`specs/0050/.manifest.state.json` belongs to the supervisor.** It was modified before this worker started and was
  not touched.
