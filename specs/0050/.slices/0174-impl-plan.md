# Step 0174 — impl plan

## Approach

Move two launcher-local bodies into `OpticalConstructor.Ui/KitOpening.fs`, so the coming start screen (steps 179, 180)
calls the same code. Each move deletes the body it moves (.spec-md §4).

1. `type KitFolderPicker with member this.pickThen (onPicked : KitFolder -> unit) : unit`. This is step 154's
   `Async.StartImmediate` body. `Some folder` calls `onPicked folder` and `None` calls nothing. It adds no posting, retry
   or `try`.
2. `OpenedKit.resolveAndShow (imports) (show) (onFailed) : unit`, an intrinsic member. It is step 156's resolution on
   open:
   - `[]` pending calls `show this`;
   - otherwise it opens `ImportDialogWindow(imports, ResolveKitSnapshot (this.library, this.decisions), onApplied)`.
     Apply re-binds through `this.withRebindings`: `Ok resolved` calls `show resolved`, and `Error e` closes the kit
     and calls `onFailed e`.
   - The dialog's `Closed` without an Apply closes the kit. One local `wasApplied` flag tracks that.
   - A failed close is logged with `Logger.logError … {error.describe}`.
3. `Program.fs` `LauncherWindow`:
   - Main and Inverse call `(pickerFor this).pickThen (fun folder -> openConstructorKit context folder <mode> ReadWriteKitAccess)`;
   - `pickConstructorKit` is deleted;
   - the `Ok` arm calls `opened.resolveAndShow context.catalogueImports showConstructor showKitError`;
   - the launcher's dialog, flag, close helper and re-binding code are deleted.
4. `KitOpeningTests` gains `pickThen hands on the folder the picker answers and nothing on a cancel`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitOpening.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitOpeningTests.fs`

## Risks

- **Dialog owner.** The slice's signature has no owner window, so `resolveAndShow` calls `dialog.Show ()` rather than
  the launcher's `dialog.Show(this)`. Step 179 calls the same signature from the start screen. No LauncherTests fact
  reads the dialog's owner; they observe windows through `Window.WindowOpenedEvent`.
- **Compile order.** `KitOpening.fs` compiles after `ImportDialogView.fs` and `ImportDialogWindow.fs`, so
  `OpenedKit` can name `ImportDialogWindow` directly.
