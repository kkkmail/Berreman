# Step 0173 — impl plan

## Approach

The launcher's `showKitError` (`OpticalConstructor.App/Program.fs`) builds its two kit-open texts from literals:

- `The kit is open elsewhere: {holder.describe}.` for `KitLibraryKitErr (KitLockedErr holder)` (step 155);
- `The kit could not be opened: {error.describe}` for every other `KitLibraryError` (step 154).

Each text moves to one home in `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`, as an extension member next to
`KitError.caption`:

- `KitLibraryError.openFailedCaption : ControlCaption`;
- `KitLockHolder.openElsewhereCaption : ControlCaption`.

Each member has a one-line doc citing spec 0050 S5.2. `showKitError` then picks one of the two captions and renders
its `.value` as the TextBlock's text and its automation name. Both literals leave `Program.fs`, and the launcher
shows the same words as before.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: two extension members. This needs
  `open OpticalConstructor.Domain.KitLibrary`, and `KitLockHolder` is qualified through
  `OpticalConstructor.Domain.Kit`, the same way `KitError` is there.
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`: `showKitError` renders the captions.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitOpeningTests.fs`: the fact
  `the open captions name the error and the lock holder`, plus `open OpticalConstructor.Ui` so the `[<AutoOpen>]`
  extensions are in scope.

## Risks

- **Name collisions.** ELEV019 requires extension-member names to be unique per receiver. No `openFailedCaption` or
  `openElsewhereCaption` exists anywhere, which grep confirms.
- **Fixture placement.** Step 129's holder fixture is `private` to `KitTests.fs`, which this step may not edit. The new
  fact therefore declares its own holder with the same values the slice names.
- **Unchanged assertions.** `LauncherTests`' facts of steps 154 to 156 assert the rendered text, which this step
  leaves unchanged. That file is not edited.
