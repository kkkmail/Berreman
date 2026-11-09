# Step 0184 — IMPLEMENT — impl-plan

## Approach

The kit window's title names the kit, `(read-only)` when the kit was opened read-only, its folder and the
constructor (spec 0050 S5.3; `.manual/001 §O`).

1. `OpticalConstructor.Ui/KitOpening.fs`
   - `OpenedKit` gains `folder : KitFolder` as its first field.
   - The private `openedOver` takes the folder first and sets the field.
   - `openKitAt` and `openExistingKitAt` pass the folder they opened.
   - `withRebindings` keeps the folder through `{ this with … }`.
2. `OpticalConstructor.Ui/TableAndElementRotationView.fs`
   - `TableAndElementRotationModel` gains `kitFolderOpt : KitFolder option` after `kit`.
   - `initWith` sets it to `None`, and `loadKit` sets it to `Some opened.folder`.
   - The extension that holds `loadKit` gains `kitCaption` and `kitWindowTitle`, both `ControlCaption`.
3. `OpticalConstructor.App/Program.fs`
   - The view that `ConstructorScene.mount` runs sets `window.Title <- model.kitWindowTitle.value`, then answers
     `mainView model dispatch`.
   - Both constructor windows drop their literal titles, and their docs say that the title names the kit.
4. Tests
   - `KitOpeningTests` gains `an opened kit carries the folder it was opened from`.
   - New `KitWindowModelTests` (Tests) holds three facts.
   - New `KitWindowTests` (Ui.Tests) holds one fact.
   - Both new files go at the end of their compile lists, with no comment.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitOpening.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitOpeningTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`

## Risks

- `OpenedKit` is a record. Only `openedOver` constructs it; the repo-wide grep finds no other construction.
- `TableAndElementRotationModel` is constructed in full only by `initWith`; everything else uses `with`.
- The title is set inside the Elmish view, so it is first written on the initial render (`Program.run`). The UI fact
  shows the window and runs the dispatcher jobs before it reads `Title`.
- The folder text is the folder exactly as given (`folder.value.value`), not its canonical key, so the title shows
  the path the user picked.
