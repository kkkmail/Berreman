# Step 0188 — impl plan

## Approach

Step 0188 renders the kit window's kit bar. Steps 0184–0187 already put everything it shows into the model:
`kitCaption`, `kitFolderOpt`, `constructorMode`, `kitChanges`, `kitStatus`, and the messages `ShowKitInExplorer`,
`CheckKitChanges KitCheckOnRequest`, `CloseKit`, `SwitchConstructorMode`, `ReloadKitChanges` and
`KeepMyKitChanges`. This step adds only:

- ten ids;
- one view file;
- the removal of the duplicate status line;
- tests.

1. `OpticalConstructor.Controls/UiIds.fs`. A new group follows the kit start screen group:
   `// --- The kit window's kit bar (KitWindowView). ---`. It holds the ten ids, each with a one-line doc citing
   spec 0050 S5.3. `kitStatus`'s doc now says it is the kit bar's status line. The old doc also covered
   `experimentDataFiles`, so that id gets its own one-line doc, keeping what the old line said.
2. New `OpticalConstructor.Ui/KitWindowView.fs`, `module OpticalConstructor.Ui.KitWindowView`. It is compiled
   directly after `KitStartView.fs`, by one `<Compile>` line with no comment.
   - `kitWindowView` is a `DockPanel`: the kit bar docked at the top, `mainView model dispatch` as its fill.
   - The kit bar is a vertical `StackPanel` carrying `kitBar`, margin 8, 6, 8, 2. It holds, in order:
     - (a) the caption `DockPanel`: `Kit:` on the left, the wrapping caption as the fill;
     - (b) the Kit menu `WrapPanel`: Show in Explorer (only with a folder), Reload changes and Close kit as
       `MaterialsWindowView.verbButton`s, then the mode text and the switch verb;
     - (c) the offer, when `kitChanges` is not empty: a wrapping question over one horizontal row of two
       `actionButton`s, Reload (positive) and Keep mine (negative);
     - (d) the kit status, when `Some`, in `messageColor`.
   - Each control gets its id as an AutomationId, never as a `Name`. Every variable-membership child is keyed, so
     FuncUI recreates it instead of patching another control into it.
3. `OpticalConstructor.Ui/TableAndElementRotationView.fs`. `collectionBuilderView` drops its `statusRow`; its doc
   says the kit bar shows the status whichever bay is open. `KitWindowView` reaches the model only through
   `mainView` and messages, so this file does not absorb the bar's view (.spec-md §4).
4. Tests.
   - `KitWindowTests` gains the five declared facts. It mounts `kitWindowView` headless at 1000 × 700, as
     `ScrollExtentTests.constructorWindow` mounts a view. Each model is loaded from a folder kit
     (`openedAt` + `initMainWithKit` / `initInverseWithKit` + `loadKit`) with a recording `kitShell`.
   - `ScrollExtentTests` gains `the kit window: the kit bar and the page stay reachable`. It runs over
     `filledExperiments ()`, updated as a record so that the bar is at its tallest:
     - a read-only kit;
     - a test-local folder;
     - three changed files;
     - a status.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitWindowView.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`

## Risks

- **Removing the builder's status line.** Every UI test reads `kitStatus` from the model, except
  `LauncherTests`' read-only acceptance. That test asserts the `KitStatus` control is absent, which still holds
  (manual 014 keeps that expectation). Between this step and step 189, the real app windows still run
  `mainView` and show no status line. Step 189 composes `kitWindowView` into them.
- **The scroll-extent law.** The bar sits outside the page's `ScrollViewer`, so at 1000 × 540 the whole bar
  must fit. Everything in it wraps (the caption, the `WrapPanel`, the question), so its height stays well under
  the window's.
- **`actionButton` subscribes its press once per id.** Reload and Keep mine send constant messages, so a stale
  subscription is harmless. The mode switch is a `verbButton`, which refreshes its handler on every patch.
