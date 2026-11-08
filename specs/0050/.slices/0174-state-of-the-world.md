# Where we are

Step 0174 is an `IMPLEMENT` step in spec 0050 S5.2, the kit start screen and the Launcher. It moves two launcher-local
bodies into `OpticalConstructor.Ui/KitOpening.fs`, so the start screen (steps 179, 180) calls the same code (.spec-md
§4: a reuse is a move or a call):
- how a picked folder reaches the code (step 154);
- how an opened kit reaches its window through the import dialog (step 156).

Verification belongs to the arc-runner after this act-only worker exits.

# What's working

- Added `KitFolderPicker.pickThen`, the one home of the picker start. It runs `Async.StartImmediate` on the UI thread,
  hands on `Some folder`, and calls nothing on a cancel.
- Added `OpenedKit.resolveAndShow`, the one home of resolution on open:
  - only an empty snapshot list shows the kit at once;
  - otherwise the import dialog confirms every row, Reuse included;
  - a cancelled dialog or a failed re-binding releases the kit.
- `LauncherWindow` calls `pickThen` and `resolveAndShow`, and its own `Async.StartImmediate`, dialog, flag and
  re-binding code are deleted.
- Added the `KitOpeningTests` fact `pickThen hands on the folder the picker answers and nothing on a cancel`.

# Tests

No gates were run: under invariant 6 this role does not run checks. The arc-runner still has to run:
- build;
- elevation;
- constructor-unit-tests, all discovered;
- ui-smoke, all discovered;
- ui-tests, `OpticalConstructor.Ui.Tests.LauncherTests`;
- the fourteen s20 section invariants.

The new fact is `OpticalConstructor.Tests.KitOpeningTests.pickThen hands on the folder the picker answers and nothing on
a cancel`. The constructor count should rise by 1 over step 0173's.

`LauncherTests` was not edited. Its facts of steps 154 to 156 run over the moved code with every assertion kept: the
picked kit with its experiments, the cancelled pick, the read-only offer, and the three resolution facts (the all-Reuse
confirmation included).

Nothing was measured here. The YAML repeats the latest recorded checkpoints (constructor 1,174 at step 172, ui-smoke
224), not a result of this round.

```yaml
gates:
  constructor_unit_tests: 1174
  ui_smoke_tests: 224
```

# Architecture

- **`pickThen`.** It is an instance extension member on `KitFolderPicker`, in the same `type KitFolderPicker with` block
  as `ofWindow`, and unique on its receiver (ELEV019). It catches nothing, because `ofWindow` already maps a dialog that
  cannot open to `None`.
- **`resolveAndShow`.** It is an intrinsic member of the `OpenedKit` record. It reaches `ImportDialogWindow` because
  `KitOpening.fs` compiles after `ImportDialogView.fs` and `ImportDialogWindow.fs`. The one mutable is the
  Apply-before-Closed flag, local to this IO edge. A failed close is logged with its `describe`.
- **The launcher's callbacks.** It passes `showConstructor` (its mode match) and `showKitError` (step 154's error
  display, which renders `openFailedCaption`).

# Deferred

Nothing in this slice is deferred. Step 180 removes the launcher's remaining kit opening, its mode match and the step-155
row, which stay in place now by the slice's own scope.

# Gotchas

- **The dialog has no owner.** It is now shown with `dialog.Show ()`, not `dialog.Show(this)`, because the slice's
  `resolveAndShow` signature, which step 179 reuses, takes no owner window. Only window ownership changes, and no
  `LauncherTests` fact reads it.
- **The binding name.** The launcher's `Ok` binding stays `kit`, because `opened` is already the window's
  `Result<AppContext, StorageError>` parameter.
- **`specs/0050/.manifest.state.json` belongs to the supervisor.** It was modified before this worker started and was
  not touched.

# Changelog

- 2026-11-08: Moved the launcher's picker start into `KitFolderPicker.pickThen` and its resolution on open into
  `OpenedKit.resolveAndShow`, made `LauncherWindow` call both with no copy of its own, and added one `KitOpeningTests`
  fact. Verification is left to the arc-runner.
