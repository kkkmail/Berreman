# Step 0177 — impl plan

## Approach

The kit start screen (step 176, `OpticalConstructor.Ui/KitStartView.fs`) gains its open messages (spec 0050 S5.2;
001 §F New kit… / Open kit… / Make this a kit, §E open read-only, §P CreateKit / OpenKit).

1. `KitStartMsg` gains five cases, each with a one-to-three-line doc citing spec 0050 S5.2:
   `CreateKit of KitFolder`, `OpenKit of KitFolder`, `OpenKitReadOnly of KitFolder`, `DismissKitStartOffer` and
   `KitOpenFailed of KitLibraryError`.
2. `updateKitStart` answers them:
   - `CreateKit` through `openKitAt folder ReadWriteKitAccess clock author imports` (initialises a folder without a kit,
     keeping its files);
   - `OpenKit` through `openExistingKitAt folder clock imports` (never initialises);
   - `OpenKitReadOnly` through `openKitAt folder ReadOnlyKitAccess …` (writes nothing).
   One private function of the folder, the answer and the model, `kitStartOpened`, reads every answer:
   - `Ok` bumps the history through the existing private `kitStartChanged` (the one `changeHistory` path, which already
     answers the bumped history and rows with no status, or keeps them with `notSavedCaption`), then calls `showKit` once
     and clears the offer;
   - `KitLockedErr holder` answers `ReadOnlyKitOffer (folder, holder)`, no status;
   - `NotAKitFolderErr` answers `MakeKitOffer folder`, no status;
   - any other error answers no offer and `openFailedCaption`, through one private `kitStartFailed`, which
     `KitOpenFailed` also uses.
   `DismissKitStartOffer` answers no offer, writing nothing.
3. `KitStartTests` gains six facts over a fixture rooted at `AppContext.BaseDirectory/KitStartTests/<guid>`. The fixture
   records every kit `showKit` is given and closes them all at the fact's end, before it deletes the root. A
   remembering clock wraps the stepping clock, so the bump's reading is the clock's last answer.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitStartView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitStartTests.fs`

## Risks

- **ELEV016.** New DU case names must be unique. A grep of the tree found no `CreateKit`, `OpenKit`,
  `OpenKitReadOnly`, `DismissKitStartOffer` or `KitOpenFailed` case.
- **Lock hygiene.** A kit left open keeps `.oc/lock`, and `Directory.Delete` can then fail. The fixture closes every
  shown kit before deleting, and the read-only fact closes its own owner kit in a `finally`.
- **Clock arithmetic.** The number of clock reads inside `openKitAt` is an implementation detail. The test reads the
  last answer from the remembering clock and does not count steps.
