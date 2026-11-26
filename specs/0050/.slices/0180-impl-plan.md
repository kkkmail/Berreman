# Step 0180 — impl plan (WIRE_UI)

## Approach

Re-wire the Launcher's Main and Inverse buttons to the kit start screen built in steps 176–179, and delete the
launcher's own kit opening (001 §F, §O; .spec-md §4).

1. `OpticalConstructor.App/Program.fs`
   - Add `module private KitConstructorWindows` after `InverseConstructorWindow`. Its one function,
     `showConstructorIn (context : AppContext) (mode : ConstructorMode) (opened : OpenedKit) : unit`, is step 154's mode
     match, moved out of the launcher.
   - `LauncherWindow` keeps its signature. Main shows `KitStartWindow(context, ForwardConstructor, pickerFor,
     KitConstructorWindows.showConstructorIn context)`, and Inverse does the same with `InverseConstructor`.
   - Delete the launcher's kit opening: `openConstructorKit`, the kit error / offer mutables, `clearKitError` and
     `showKitError`, and the Open read-only / Cancel row. The opens `Domain.Kit` / `KitFolders` / `Media` lose their
     last users here, so check each before removing it.
   - Materials, Library, the library error, Recreate and the pending-imports notice stay untouched.
   - Tighten the three docs: the launcher's Main / Inverse open the start screen, and each constructor window works in
     the kit the start screen opened.
2. `OpticalConstructor.Controls/UiIds.fs`: delete `launcherKitErrorText`, `launcherOpenKitReadOnlyButton` and
   `launcherCancelKitOpenButton`, with their docs.
3. `OpticalConstructor.Ui.Tests/LauncherTests.fs`
   - Add `windowsOpenedBy` (the `WindowOpenedEvent` record that `clickAndObserve` now uses too), `startScreenOf` (the one
     `KitStartWindow` a launcher click opens) and `pressAndObserve` (`clickBorder` on a start-screen box, recording what
     it opens).
   - Port the ten listed facts per the brief, and rename the error fact. The snapshot fixture opens its start screen
     once, so a cancelled dialog leaves the same screen open for the retry.
   - The Materials, Library, library-error, recreate and pending-import facts keep their code.
4. `OpticalConstructor.Ui.Tests/WireUiFinalCompositionTests.fs`: also show, assert and close
   `KitStartWindow(ctx, ForwardConstructor, …)` and `KitStartWindow(ctx, InverseConstructor, …)`. Each gets a picker of
   a fresh folder and an ignoring `showConstructor`.

## Risks

- The start screen's controls are Borders (`verbButton` / `actionButton`), not Buttons. `automationAttribute` sets only
  the AutomationId. So the read-only fact's Name, `AutomationProperties.Name` and `Button.Content` checks have to map
  onto the start screen's equivalents: the AutomationId, the box's text, and its margin in place of the spacing.
  `KitStartView.fs` is outside this step's edits, so the view stays as step 178 left it.
- The start screen closes on a successful hand-off during `MouseDown`. `clickBorder` already skips `MouseUp` on a hidden
  window.
- The snapshot fixture records every opened window, the start screen included, so its window counts move up by one.
