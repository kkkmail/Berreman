# Step 0189 — impl plan (WIRE_UI)

## Approach

Compose the pieces steps 183–188 built into the two real constructor windows, in
`OpticalConstructor.App/Program.fs`, `ConstructorScene.mount`:

1. Run `Program.mkProgram` instead of `Program.mkSimple`. Init answers the seed with the real shell
   (`kitShell = { explorer = ExplorerProxy.system; requestClose = window.Close }`) and a `Cmd.ofEffect` that stores
   the Elmish dispatch, as `LibraryWindow` does. Update is `updateTableAndElementRotation` with `Cmd.none`.
2. The view keeps setting `window.Title <- model.kitWindowTitle.value` and answers
   `KitWindowView.kitWindowView model dispatch` instead of `mainView`, so the kit bar sits above the workbench.
3. `window.Activated.Add` dispatches `CheckKitChanges KitCheckOnActivation`, so every activation (the first one
   too) checks the kit. The first check finds nothing, because opening the kit recorded its stamps (step 131).

`mount` gets a one-to-three-line doc citing spec 0050 S5.3. `MainConstructorWindow` and `InverseConstructorWindow`
keep their constructors and their `Closed` release. Their docs say they run the kit window. No timer, file watcher
or polling is added.

The mount is a module function, so the captured dispatch is a `ref` cell: a closure cannot capture a
`let mutable` local. `LibraryWindow`'s `let mutable` is a class field.

## Tests (`KitWindowTests`, trait `ui-smoke`)

- `acceptance: activating a constructor window offers the kit files changed outside the app, and Reload reads them in`:
  - show a real `MainConstructorWindow`; it holds `kitBar` and no `kitChangesText`;
  - another writer (`KitProxy.create` over the kit folder) writes `E2`: an experiment committed in a scratch
    `initMain ()` model, given `id = ExperimentId 2`;
  - show and activate a second `Window`, then `Activate ()` the constructor window again;
  - `kitChangesText` names `E2.experiment-json`;
  - press `kitChangesReloadButton` at its centre: the offer goes;
  - press the Experiments tab: the collection holds `E2`'s row.
- `Close kit closes the window and releases the kit`: pressing `kitCloseButton` closes the window (`Closed` raised
  once), and the kit folder holds no `.oc/lock`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`: two facts.
- The other five test files in the effects are touched only if the wiring breaks one of their existing facts
  (the kit bar takes height above the page, and every activation now runs a check).

## Risks

- The kit bar takes vertical space above the page's `ScrollViewer`. A fact that presses a control near the bottom
  of the real window, or wheels over the table, may now need to bring it into view first.
- In headless Avalonia, `Show ()` raises `Activated`. A second window's activation does not necessarily deactivate
  the first, but `Activate ()` raises `Activated` again, which is what the acceptance fact needs.
- `CloseKit` calls `window.Close` from inside an Elmish update. The `Closed` release then runs in the same
  dispatch. Avalonia allows closing a window from its own input handler.
