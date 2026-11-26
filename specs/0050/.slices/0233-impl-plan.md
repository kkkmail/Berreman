# Step 0233 — impl plan

## Approach

Spec 0050, section S6, step 0233 (WIRE_UI), obligation `S6-KIT-SHELL-WIRE`. The constructor windows' kit bar gets
the real Send kit… and Copy experiments… seams. Until now it ran on `KitWindowShell.inert`, apart from the explorer
and the close. The seams already exist (steps 145, 150, 197–201, 225). This step only composes them.

1. **`OpticalConstructor.Ui/AppContext.fs`.**
   - Add the record field `locations : DataLocations`, with a one-line doc: the locations the app scope was composed
     over, where its windows' dialogs start.
   - `compose` sets it from its `locations` argument, so `create` and `recreateLibrary` both carry it.
   - Every other construction is a `{ … with … }` copy, so nothing else changes.
2. **`OpticalConstructor.App/Program.fs`, `ConstructorScene.mount`.**
   - It takes the `AppContext` after the window. The shell becomes `{ KitWindowShell.inert with explorer;
     requestClose; zipPicker; archive; archiveJobs; kitPicker }`:
     - `zipPicker = KitZipPicker.ofWindow window context.locations.exportsFolder`;
     - `archive = KitArchiveProxy.create ()`;
     - `archiveJobs = KitArchiveJobHost.onUiThread`, which dispatches `SendKitProgressed` / `SendKitEnded` through
       the dispatch that the init command stores in `hostDispatch`. It is read when the job reports, not when the
       shell is built;
     - `kitPicker = KitFolderPicker.ofWindow window context.locations.kitsFolder`.
   - `MainConstructorWindow` and `InverseConstructorWindow` pass their context. Each doc gains one line: the kit bar
     sends and copies through the real dialogs.
   - Add `open OpticalConstructor.Storage.KitArchiveStore` for `KitArchiveProxy.create`.
3. **Tests.**
   - `AppContextTests`, the pure contract section (gate `ui-tests`): `the app scope carries the locations it was
     composed over`.
   - `KitWindowTests`, trait `ui-smoke`: `a constructor window's kit bar offers Send kit, and Copy experiments only to
     a kit opened read-write`. It shows a real `MainConstructorWindow` over a read-write kit from
     `TestAppScope.openedKitIn`, then over the same folder opened again read-only. Nothing is pressed.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/AppContextTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`

The windows' public constructors are unchanged. The other effect files (`LauncherTests`,
`TableAndElementRotationTests`, the three `WireUi*CompositionTests`) are expected to need no edit, and they keep their
assertions.

## Risks

- **Real dialogs in a headless test.** A fact that pressed Send kit… or Copy experiments… on a real constructor window
  would now open the system dialog. No existing fact does: those presses use hosted models with test shells. The new
  fact presses nothing.
- **The dispatch reference.** The shell's closures must dereference `hostDispatch` when called. A value captured when
  the shell is built would still be `ignore`.
