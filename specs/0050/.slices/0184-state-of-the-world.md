# Step 0184 — state of the world

# Where we are

Spec 0050 section S5.3 builds the kit window (title, Kit menu, lock, reload and mode) and the Data locations window.
Step 183 made `ExplorerProxy.system` real. This `IMPLEMENT` step gives the kit window its title. An opened kit now
carries the folder it was opened from. The constructor model knows that folder, and a constructor window's title is
its model's `kitWindowTitle`, which names:

- the kit;
- `(read-only)` when the kit was opened read-only;
- the folder;
- the constructor.

Later S5.3 steps rely on the title following the model: the mode switch (step 186) and a reload's new name
(step 187).

# What's working

- `OpenedKit` gains `folder`, the folder the kit was opened from. `openKitAt` and `openExistingKitAt` set it.
- The constructor model gains `kitFolderOpt` (`None` for the scratch kit), `kitCaption` and `kitWindowTitle`, for
  example `Optics QZ (read-only) — C:/kits/Optics QZ — Optical Constructor — Main`.
- Main and Inverse constructor windows take their title from the model on every render. The literal titles are gone.
- Four constructor-unit facts are added (`KitOpeningTests`, `KitWindowModelTests`), plus the headless
  `KitWindowTests` fact.
- Repaired the attempt-01 build error: `KitWindowModelTests` now opens `Storage.CatalogueImportStore`, where
  `CatalogueImportContext.create` is defined.

# Tests

This worker ran no build, test, analyzer or invariant gate, in either attempt. The supervisor's attempt-01 `build`
measured one error, `FS0039` in `KitWindowModelTests.fs:28`, because `CatalogueImportStore` was not opened.
Attempt 02 repaired it. The `IMPLEMENT` protocol (Invariant 6) gives gate
execution to the gate engine, which runs:

- build and elevation;
- constructor-unit-tests, all discovered;
- ui-smoke, all discovered;
- ui-tests over `AppContextTests`, `InverseConstructorTests`, `KitWindowTests`, `LauncherTests`,
  `SceneSaveLoadTests` and `TableAndElementRotationTests`;
- the fourteen s21 invariants.

Checked without running a gate:

- every edited and new file is LF, and `git diff --check` is clean;
- no added line contains an s21 invariant's word;
- every API used was checked against its source or an existing use in the repo (impl-log, Testing state).

Test identities created, all declared by the step:

- `KitOpeningTests.an opened kit carries the folder it was opened from`
- `KitWindowModelTests.the window title names the kit, its folder and the constructor`
- `KitWindowModelTests.a kit opened read-only is marked in the title`
- `KitWindowModelTests.a scene that opened no folder is titled by its kit alone`
- `KitWindowTests.a constructor window is titled by its kit` (ui-tests, no trait)

None is removed or renamed.

The counts below are the latest checkpoint's (182), not measured this round. Constructor-unit-tests is expected to
measure 1195 (+4). Ui-smoke is unchanged. Ui-tests over the selected classes is expected to rise by 1.

```yaml
gates:
  constructor_unit_tests: 1191
  ui_smoke_tests: 227
```

# Architecture

- The opened folder lives on `OpenedKit`, the session's record of what the start screen opened. The model copies it
  in `loadKit`, the one place a constructor takes up an opened kit. A scene seeded without opening a folder keeps
  `kitFolderOpt = None`, so its title is its kit's name alone.
- The title text is pure model logic (`kitCaption` and `kitWindowTitle`, both `ControlCaption`), so it is tested
  without a window. The window's only part is one assignment in the view `ConstructorScene.mount` runs, so the title
  follows every model change without a cache or a separate message.
- The folder is shown as it was opened, never as its `canonical` key.

# Deferred

Nothing. Every requirement of the step landed this round.

# Gotchas

- A constructor window's `Title` is written by the Elmish view, so it is guaranteed only after the first render.
  Headless facts must show the window and run the dispatcher's jobs before they read it.
- `CatalogueImportContext.create` is a Storage type extension. Calling it requires
  `open OpticalConstructor.Storage.CatalogueImportStore`, and the Domain open alone is not enough.
- `TestAppScope.openedKitIn` names its kit after the GUID folder leaf. A title assertion over it must build the
  expected text from the opened kit's name and folder, never from a literal.

# Changelog

- 2026-11-08 — Step 0184: added `OpenedKit.folder`, the model's `kitFolderOpt`, `kitCaption` and `kitWindowTitle`.
  Main and Inverse windows now take their title from the model. Added four constructor-unit facts and one ui-tests
  fact (`KitWindowTests`).
- 2026-11-08 — Step 0184, attempt 02: repaired the `build` failure (`FS0039`) by opening
  `OpticalConstructor.Storage.CatalogueImportStore` in `KitWindowModelTests.fs`. No other change.
