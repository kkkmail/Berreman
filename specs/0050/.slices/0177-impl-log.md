# Step 0177 — impl log

## Progress

- [x] Read the protocol (`arc-runner.system-md`, `implement_worker.system-md`), the project prompt (its Operator note
  is empty), the slice, the `.gates` snapshot, and the step-176 SoW. Also read the code: the step-176
  `KitStartView.fs` / `KitStartTests.fs`, `KitOpening.fs` (`openKitAt`, `openExistingKitAt`), `KitHistory.fs` (`bump`,
  `changeHistory`), `Kit.fs` / `KitFolders.fs` (`KitError`, `KitContext.openKit` / `openOrInitialise`),
  `KitFolderStore.initialiseKit`, and the captions in `ConstructorUiIdOfDomain.fs`.
- [x] `KitStartView.fs`: the five open messages and the shared answer reader.
- [x] `KitStartTests.fs`: six facts plus their fixture.
- [x] Self-review: style, ELEV, name collisions, section invariants, LF endings.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitStartView.fs`
  - `KitStartMsg` gains five cases: `CreateKit of KitFolder`, `OpenKit of KitFolder`, `OpenKitReadOnly of KitFolder`,
    `DismissKitStartOffer` and `KitOpenFailed of KitLibraryError`. Each has a one-to-two-line doc citing spec 0050 S5.2.
  - New private `kitStartOpened (folder) (answer) (m)`, the one reader of every open's answer, matched in the slice's
    order:
    - `Ok` → bump through the existing `kitStartChanged`, then `showKit` once, then no offer;
    - `KitLockedErr holder` → `ReadOnlyKitOffer (folder, holder)`, no status;
    - `NotAKitFolderErr` → `MakeKitOffer folder`, no status;
    - any other error → `kitStartFailed`.
  - New private `kitStartFailed (error) (m)`: no offer, with status `error.openFailedCaption`. It is shared by the
    last open branch and `KitOpenFailed`.
  - `updateKitStart` answers the new messages:
    - `CreateKit` through `openKitAt … ReadWriteKitAccess …`;
    - `OpenKit` through `openExistingKitAt`;
    - `OpenKitReadOnly` through `openKitAt … ReadOnlyKitAccess …`;
    - `DismissKitStartOffer` clears the offer only.
  - The module doc and the `updateKitStart` doc now cover the open messages.
  - Added `open OpticalConstructor.Domain.KitLibrary`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitStartTests.fs`
  - New fixture helpers:
    - `historyOver`: an in-memory store seeded with a history over given present folders. The existing
      `storedHistory` now delegates to it.
    - `kitStartAuthor`, which `contextOver` now uses too.
    - `remembering`: wraps a clock and keeps its last answer.
    - `withKitStartRoot`: the root is `AppContext.BaseDirectory/KitStartTests/<guid N>`. At the end it closes every
      kit `showKit` recorded, then deletes the root, then fails on any close error.
    - `kitIn`, `closeHeld`, `heldKitAt`, `madeKitAt`, `olderEntryOf`, `entriesOf`, `kitFolderBytes` and
      `assertKitLayout`.
  - Six new facts, named exactly as the slice's test effects.
  - Added opens: `System.IO` and `OpticalConstructor.Domain.KitLibrary`.

## Decisions

- **Reuse the history write path.** The `Ok` branch bumps through step 176's `kitStartChanged`, the one
  `changeHistory` caller. On `Ok` it already answers the bumped history and rows with no status, and on `Error` it
  keeps both with `notSavedCaption`, which is exactly what the slice specifies for the bump. Only the offer reset and
  the `showKit` call are added on top. So the kit is handed over even when the bump is refused.
- **Order inside `Ok`:** bump first, then `showKit`, as the slice states ("bumps the history … then calls
  `m.context.showKit opened` once").
- **`DismissKitStartOffer` leaves `status` alone** (`{ m with offer = NoKitStartOffer }`). The slice asks only for
  no offer and writing nothing, and the step-176 filter also leaves the status alone.
- **Offers clear the status** ("answers `ReadOnlyKitOffer (folder, holder)` and no status"), so a stale failure
  caption does not sit beside a question.
- **Errors from `CreateKit` and `OpenKitReadOnly` go through the same reader.** A read-write `CreateKit` on a kit
  that is open elsewhere becomes the read-only offer. A read-only open of a folder without a kit becomes Make this a
  kit. Both follow from "one private function … reads every answer".
- **"Opened at the clock's last answer."** Fact 1 wraps the context's stepping clock in `remembering` and compares
  the history entry's `lastOpened` with the clock's last answer. It does not count how many times `openKitAt` reads
  the clock, which is an implementation detail.
- **The "a failed resolution" fact** builds its `MakeKitOffer` model by copy-and-update from `initKitStart`. The
  update is pure, and the message never touches disk.
- **Lock hygiene.**
  - Facts that hold a kit open themselves close it in a `finally` (`closeHeld owner`) or at once (`madeKitAt`).
  - The fixture closes the kits `showKit` recorded before it deletes the root.
- **Kits made by the fact** use `ClockProvider.system` through `openKitAt`, as `KitOpeningTests` does. The screen's
  own opens use the context's stepping clock.

## Testing state

No gate was run. Under invariant 6 this `IMPLEMENT` worker acts only, and the arc-runner's gate engine runs the
step's roster after exit:
- build;
- elevation;
- constructor-unit-tests, all discovered;
- ui-smoke, all discovered;
- ui-tests, `AppContextTests` and `LauncherTests`;
- the fourteen s20 invariants.

I compiled nothing, so I checked the code by hand:
- **Name collisions.** I grepped for the five new case names in the Berreman tree and in STL. There are none
  (ELEV016).
- **Other consumers.** `KitStartMsg` and `updateKitStart` have no consumer outside the two edited files, so no
  incomplete-match warning (FS0025) can appear elsewhere.
- **Section invariants.** The diff contains none of the fourteen s20 invariants' patterns.
- **Shadowing.** No opened module shadows `File`, `Path`, `Directory`, `SearchOption`, `UserName` or `MachineName`.
- **Parse hazards.** I rewrote `File.ReadAllText((…).value)`, `AddDays(-1.0)` and `Assert.Single<OpenedKit>(shown)`
  in parenthesised form.
- **Type annotation.** The close-error lambda's parameter is annotated, so `.describe` resolves.

Expected effect: constructor-unit-tests rises by 6, from the last recorded checkpoint of 1,180 to 1,186. No ui-smoke
or ui-tests test changed.

## Artifacts

None. The round produced no logs, captures or dumps.

## Gotchas

- **Duplicated test helpers.** `KitStartTests.kitFolderBytes` restates the private `KitOpeningTests.folderBytes`, and
  `KitStartTests.steppingClock` (step 176) restates `KitHistoryTests`'. Both originals are private to files outside
  this step's authorized edits (`KitStartView.fs`, `KitStartTests.fs`), so they could not be shared without exceeding
  scope.
- **The spec's "`openKitAt folder ReadOnlyKitAccess …`" passes `m.context.author`.** `openKitAt` ignores the author
  on a read-only open.
- **`specs/0050/.manifest.state.json`** was already modified, by the supervisor, before this worker started. It was
  not touched.
