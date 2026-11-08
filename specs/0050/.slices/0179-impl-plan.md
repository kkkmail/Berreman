# Step 0179 — impl plan

## Approach

The start screen's window, `KitStartWindow`, joins the step 176–178 model, update and view to the app scope, as
`LibraryWindow` joins `LibraryWindowView`:

1. `OpticalConstructor.Controls/UiIds.fs` — `kitStartWindow` (`"KitStartWindow"`) in the start-screen group, with a
   one-line doc.
2. New `OpticalConstructor.Ui/KitStartWindow.fs` (`namespace OpticalConstructor.Ui`), compiled directly after
   `AppContext.fs`, the list's last file (one `<Compile>` line, no comment). The type inherits `HostWindow`, titles
   itself by its mode, sets `Name`/`AutomationId` to `kitStartWindow`, asks for 720 × 560 and calls
   `WindowFit.keepOnScreen this`. It captures the Elmish dispatch from its init command and runs
   `initKitStart` / `updateKitStart` / `kitStartView` over a `KitStartContext` composed from the app scope (history,
   `settings.kitHistoryLimit`, `ClockProvider.system`, `KitAuthor Environment.UserName`, `catalogueImports`,
   `pickerFor this`). `showKit` is `opened.resolveAndShow context.catalogueImports (fun shown -> showConstructor mode
   shown; this.Close ()) (fun e -> dispatch (KitOpenFailed e))`.
3. `OpticalConstructor.Ui.Tests/ScrollExtentTests.fs` — `the kit start screen leaves everything reachable`:
   `assertReachable "KitStart"` over the window on a fresh app scope whose history holds forty in-memory entries
   (every fifth folder missing, every seventh kit starred).
4. `OpticalConstructor.Ui.Tests/KitStartWindowTests.fs` — two facts over a fresh app scope with a fresh in-memory
   history and a recording `showConstructor`: the title/id fact (no trait) and the ui-smoke acceptance (New kit on
   the Main and Inverse screens records the screen's mode with a read-write kit named for the folder, closes the
   screen and leaves the history's one entry that folder). Every recorded kit is closed at the fact's end and every
   fresh folder is deleted.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitStartWindow.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitStartWindowTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`

## Risks

- The title needs a render of `ConstructorMode`, which has none, and `TableAndElementRotationView.fs` is outside the
  step's edits: the window matches the mode inline (a private one-parameter function would be ELEV013P).
- `this.Close ()` runs inside the Elmish update (the hand-off is called from `kitStartOpened`), as `LibraryWindow`'s
  `requestClose` does; the loop's next render lands on a closed host, which FuncUI tolerates.
- The scroll law walks forty rows' corners at two sizes; the view's `ScrollViewer` and `MinWidthPanel` already scroll
  both ways.
