# Step 0176 — impl plan

## Approach

Add the kit start screen's pure Elmish state and its history messages (spec 0050 S5.2), shaped like
`LibraryWindowView.fs`: a `[<ReferenceEquality>]` context of injected seams, a model, messages and an
update that reaches IO only through the context.

- New `OpticalConstructor.Ui/KitStartView.fs` (`module OpticalConstructor.Ui.KitStartView`), compiled
  right after `TableAndElementRotationView.fs` (it reads `ConstructorMode`): `KitStartContext`,
  `KitStartOffer`, `KitRelocation`, `KitStartModel`, `KitStartMsg` (StarKit, UnstarKit, ForgetKit,
  LocateKit, FilterKitHistory), `initKitStart`, `updateKitStart`.
- The four history-writing messages go through `KitHistoryProxy.changeHistory` with the domain verbs
  `star` / `unstar` / `forget` / `relocate`; `Ok` re-lists the rows via `rowsMatching filter probeFolder`
  and clears the status; `Error e` keeps history and rows and sets `status = Some e.notSavedCaption`.
  `FilterKitHistory` only re-lists.
- `ConstructorUiIdOfDomain.fs`: `KitHistoryError.notSavedCaption : ControlCaption`, beside the other
  start-screen captions (`openFailedCaption`, `openElsewhereCaption`).
- New `OpticalConstructor.Tests/KitStartTests.fs` (`module KitStartTests`), appended last in the test
  project's compile list, with the five facts the slice names.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitStartView.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` (one Compile line)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs` (one member)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitStartTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (one Compile line)

## Risks

- ELEV: helpers must not be single-parameter free functions over our types (ELEV013/013P); the model
  re-listing is a member on `KitStartModel`. New names checked unique across `Berreman/` and STL.
- Record-label inference: the model is built with an explicit type annotation.
- LF line endings on every new/edited file.
