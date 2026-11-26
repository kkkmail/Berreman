# Step 0180 — impl log

## Progress

- [x] `Program.fs`: `KitConstructorWindows.showConstructorIn`. Main and Inverse each show one `KitStartWindow` of their
      mode. The launcher's kit opening is deleted whole.
- [x] `UiIds.fs`: `launcherKitErrorText`, `launcherOpenKitReadOnlyButton` and `launcherCancelKitOpenButton` retired with
      their docs.
- [x] `LauncherTests.fs`: the ten listed facts now go through the start screen, and the error fact is renamed. The
      Materials, Library, library-error, recreate and pending-import facts keep their code.
- [x] `WireUiFinalCompositionTests.fs`: `KitStartWindow` is shown, asserted and closed in both modes over the fact's one
      context.
- [x] `git grep -n -F -e launcherKitErrorText -e launcherOpenKitReadOnlyButton -e launcherCancelKitOpenButton -- Berreman/`
      finds nothing (exit 1). The ids' literal strings are gone from `Berreman/` too.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
  - Adds `module private KitConstructorWindows` after `InverseConstructorWindow`. Its one function is
    `showConstructorIn (context : AppContext) (mode : ConstructorMode) (opened : OpenedKit) : unit`, step 154's mode
    match moved out of the launcher, with a one-line doc.
  - Main's click is now `KitStartWindow(context, ForwardConstructor, pickerFor, KitConstructorWindows.showConstructorIn
    context).Show()`; Inverse's is the same with `InverseConstructor`. `LauncherWindow`'s signature is unchanged.
  - Deletes the kit error / offer mutables, `clearKitError`, `showKitError`, `openConstructorKit` and the Open read-only
    / Cancel row. Their calls of `pickThen`, `resolveAndShow`, `openFailedCaption` and `openElsewhereCaption` went with
    them, so the start screen is now their only caller.
  - Drops `open OpticalConstructor.Domain.Kit`. Nothing in the file names a type, case or function of that module any
    more (token scan of `Kit.fs`'s declarations against the file), and `KitError.describe` is intrinsic.
  - The `LauncherWindow`, `MainConstructorWindow` and `InverseConstructorWindow` docs now say that Main and Inverse open
    the start screen, and that a window works in the kit the start screen opened. The file header names the start
    screen too, because it said Main opens the constructor.
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`: removes the three launcher kit ids and their docs.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LauncherTests.fs`
  - Adds `windowsOpenedBy`, the `WindowOpenedEvent` record. `clickAndObserve` is now a call of it that filters out the
    launcher, so the subscription is not copied.
  - Adds `startScreenOf`, the one `KitStartWindow` a Main / Inverse click opens; `pressAndObserve`, `clickBorder` on a
    start-screen box recording what opens; and `closeStartScreen`.
  - The snapshot fixture gains `start`: it opens the start screen once, and every dialog comes from that screen's Open
    kit….
  - The facts are ported as below, and the module doc names the start screen.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WireUiFinalCompositionTests.fs`: the launcher fact also shows,
  asserts and closes `KitStartWindow(ctx, ForwardConstructor, …)` and `KitStartWindow(ctx, InverseConstructor, …)`.
  Each has a picker of a fresh folder and a `showConstructor` of `fun _ _ -> ()`. The fact's comment now lists the start
  screen.

## Decisions

- **The facts' mapping.** Each fact that clicked Main or Inverse to open a kit now takes the one `KitStartWindow` the
  click opens (`startScreenOf`, through `WindowOpenedEvent`). It then presses the start-screen Border the old fact
  reached through the launcher, at its centre, with `clickBorder`. The pressed boxes are:
  - `newKitButton` for the Main / Inverse scene facts and the cancelled pick;
  - `openKitButton` for the experiments, error, held-kit and snapshot facts;
  - `openKitReadOnlyButton` and `cancelKitStartOfferButton` for the held-kit offer.

  The launcher's picker reaches the start screen unchanged.
- **The start screen's lifetime is asserted wherever the brief names it:**
  - it closes once a constructor opens: the scene, experiments, read-only, error retry and snapshot Apply facts;
  - it stays open after a cancelled pick, a failed open, Cancel on the held-kit offer, and Cancel or a direct close of
    the import dialog.

  Each fact closes a still-open screen in its `finally` (`closeStartScreen`).
- **The held-kit acceptance keeps every check in substance.** The start screen's offer is step 178's view, and
  `KitStartView.fs` is outside this step's edits. That view sets only the AutomationId (`automationAttribute`), and its
  pair are `actionButton` Borders with no row spacing. So each launcher-specific check maps onto the start screen's
  equivalent:
  - `Name = id` → `AutomationProperties.GetAutomationId = id`;
  - `Button.Content = label` → the Border's `TextBlock` child's text;
  - `Spacing = 12` and `Right + 12 = Left` → a positive right margin on Open read-only, and `Right + Margin.Right =
    Left` (Cancel sits after the accepting box's margin, .spec-md §10.5, as step 178's view documents).

  The `AutomationProperties.Name` equalities on the offer text and the two boxes have no counterpart, because the start
  screen's view sets no accessible name there, so they are dropped. Every other assertion is kept as it was, against the
  start screen's controls:
  - the caption, the process id, the wrapping and the visibility;
  - the shared row, the one row under the text, the two children, the horizontal orientation and the equal tops;
  - the fills, the unchanged files, the picks, and the read-only save refusal.

  The "no kit controls left" checks still run over the launcher, with the start screen's ids. They now assert that the
  launcher hosts no kit question.
- **The renamed error fact** asserts the parts the brief names: `kitStartStatusText` names `E1.experiment-json`, no
  constructor window opens and no `.oc/lock` is left.
  - It also keeps the old visibility, wrapping, `StartsWith` and no-offer checks.
  - The old `AutomationProperties.Name` check is dropped, because the status sets no accessible name.
  - After the experiment file is deleted, a fresh Main start screen opens the kit, and the screen closes.
- **The snapshot fixture records every opened window, the start screen included,** so its counts move up by one: 1 → 2,
  2 → 3, 3 → 4, and the constructor is `opened[3]`. A cancelled dialog leaves the same start screen open for the retry.
  This is the brief's "Cancel on the dialog keeps the start screen open", and it is asserted after Cancel and after the
  direct close.
- **The kit history.** Every launcher fact's start screen uses the fixture's file-backed history under
  `TestAppScope/LauncherTests/Preferences`, inside the test output. This is the real composition, so a successful open
  bumps it there. No assertion reads it, and it writes nothing into a kit folder, so `assertFilesUnchanged` and the
  lock-bytes checks are unaffected.
- **The new module's doc** is one line, on `showConstructorIn`. The `private` module has no doc of its own.

## Testing state

No build, test, analyzer or invariant gate was run by this worker. The WIRE_UI protocol (Invariant 6) gives gate
execution to the arc-runner's gate engine alone, and no compilation, warning or pass result is claimed here. The gate
engine runs:

- build and elevation;
- constructor-unit-tests and ui-smoke, all discovered;
- ui-tests, selected to `OpticalConstructor.Ui.Tests.LauncherTests`;
- the fourteen s20 invariants.

What was checked without running a gate:

- the step's own `git grep` for the three retired ids finds nothing, nor do their literal strings;
- the four edited files are LF, byte-checked: no `\r\n` in any of them, and `git diff --numstat` equals
  `--ignore-cr-at-eol`;
- the start-screen titles in the facts are byte-identical to `KitStartWindow.fs`'s `Open a kit — Main` / `— Inverse`.

Test identities:

- no fact is added or removed;
- one ui-smoke fact is renamed, `a kit that cannot be opened is shown on the launcher and opens no window` → `a kit that
  cannot be opened is shown on the start screen and opens no window`, so the ui-smoke count stays the same;
- the other ten listed facts change expectation only.

## Artifacts

None this round: no gate or tool run produced a capture.

## Gotchas

- **The start screen closes inside its own press.** A successful hand-off runs `this.Close()` in the Elmish update during
  `MouseDown`. `clickBorder` already skips `MouseUp` on a hidden window, so the facts check `start.IsVisible` afterwards
  and close only a still-visible screen.
- **The start screen's boxes are Borders**, found by AutomationId only. `clickBorder`'s `matchesId` (Name or
  AutomationId) finds them; `tryFindButton` would not.
- **The snapshot fixture's `opened`** sees the start screen as its first window. A later count change there is an
  off-by-one against that screen, not a stray dialog.
- `specs/0050/.manifest.state.json` was already modified when the round started. It is the supervisor's, and this step
  leaves it alone.
