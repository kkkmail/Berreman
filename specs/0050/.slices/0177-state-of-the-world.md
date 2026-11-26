# Where we are

Step 0177 is an `IMPLEMENT` step in spec 0050 S5.2, the kit start screen and the Launcher. It adds the start screen's
open messages to the pure Elmish update from step 176 (001 §F New kit… / Open kit… / Make this a kit, §E open
read-only, §P CreateKit / OpenKit). Choosing a kit opens it through `KitOpening`, bumps the kit history and hands the
kit to the window through `showKit`. A question that an open raises becomes an offer.

Step 178 adds the view and its ids, and step 179 adds the window, which reports a failed resolution with
`KitOpenFailed`. Verification belongs to the arc-runner after this act-only worker exits.

# What's working

- Added `CreateKit`, `OpenKit`, `OpenKitReadOnly`, `DismissKitStartOffer` and `KitOpenFailed` to `KitStartMsg`.
- `updateKitStart` opens through `openKitAt` (read-write or read-only) or `openExistingKitAt`, and one private reader
  handles every answer:
  - every successful open bumps the history and hands the kit to `showKit` once;
  - a kit open elsewhere becomes `ReadOnlyKitOffer`, and a folder that is not a kit becomes `MakeKitOffer`;
  - any other failure shows `openFailedCaption` and hands nothing over.
- `DismissKitStartOffer` drops the offer. `KitOpenFailed` drops the offer and shows the failure.
- Added six `KitStartTests` facts:
  - New kit;
  - Open kit;
  - Make this a kit, and Cancel;
  - Open read-only;
  - an unopenable kit;
  - a failed resolution.

# Tests

No gates were run: under invariant 6 this role does not run checks. The arc-runner still has to run:
- build;
- elevation;
- constructor-unit-tests, all discovered;
- ui-smoke, all discovered;
- ui-tests, `OpticalConstructor.Ui.Tests.AppContextTests` and `OpticalConstructor.Ui.Tests.LauncherTests`;
- the fourteen s20 section invariants.

The six new facts are `OpticalConstructor.Tests.KitStartTests.`:
- `New kit initialises an empty folder, bumps the history and hands the kit over`;
- `Open kit opens an existing kit and bumps it above the others`;
- `Open kit on a folder that is not a kit offers Make this a kit, and accepting it keeps the folder's files`;
- `a kit open elsewhere offers Open read-only, which opens it and writes nothing to the kit`;
- `a kit that cannot be opened is shown and nothing is handed over or bumped`;
- `a failed resolution reported by the window is shown`.

The constructor count should rise by 6, from 1,180 to 1,186. No ui-smoke or ui-tests test changed.

Nothing was measured here. The YAML repeats the latest recorded checkpoint (`.checkpoints-json` 175: constructor
1,180, ui-smoke 224), not a result of this round.

```yaml
gates:
  constructor_unit_tests: 1180
  ui_smoke_tests: 224
```

# Architecture

- **One answer reader.** `kitStartOpened folder answer m` is the only place an open's `Result` is interpreted. The
  three open messages differ only in the `KitOpening` call that produces the answer.
- **One history write path.** The bump goes through step 176's `kitStartChanged`, the only `changeHistory` caller.
  So a refused bump keeps the history and rows, shows `notSavedCaption`, and still hands the kit over.
- **One failure shape.** `kitStartFailed` (no offer, `openFailedCaption`) serves both an open's unexpected error and
  the window's `KitOpenFailed`.
- **A read-only open bumps the history** like any other open, because the user chose the kit.
- **The update stays a pure function of its context.** Every effect goes through `KitStartContext`: the kit folders
  through `KitOpening`, the history through `kitHistory`, time through `clock`, and the hand-over through `showKit`.
  So the facts drive it with an in-memory history and a recording `showKit`.

# Deferred

Nothing in this slice is deferred. The view and its ids (step 178) and the window, with its resolution and the
`KitOpenFailed` report (step 179), are out of this slice's scope.

# Gotchas

- **A failed open never initialises a folder.**
  - `OpenKit` goes through `openExistingKitAt`, which never initialises.
  - Only `CreateKit` (New kit… and Make this a kit) initialises a folder, and initialising keeps every file the
    folder already holds (`KitFolderStore.initialiseKit`).
- **Errors from other opens are offers too.** A `CreateKit` of a kit that is open elsewhere becomes the read-only
  offer. An `OpenKitReadOnly` of a folder without a kit becomes Make this a kit.
- **`DismissKitStartOffer` leaves `status` alone.** It clears only the offer and writes nothing. Offers themselves
  clear the status.
- **Test fixture hygiene.** `withKitStartRoot` closes every kit `showKit` recorded before it deletes the root. Facts
  that hold a kit open themselves close it in a `finally`. A kit left open would keep `.oc/lock`.
- **Duplicated test helpers.** `KitStartTests.kitFolderBytes` restates the private `KitOpeningTests.folderBytes`.
  That file is outside this step's authorized edits.
- **`specs/0050/.manifest.state.json` belongs to the supervisor.** It was modified before this worker started and was
  not touched.

# Changelog

- 2026-11-08: Added the kit start screen's open messages to `KitStartView` (`CreateKit`, `OpenKit`, `OpenKitReadOnly`,
  `DismissKitStartOffer`, `KitOpenFailed`), with the shared answer reader that bumps the history and hands the kit
  over, and six `KitStartTests` facts. Verification is left to the arc-runner.
