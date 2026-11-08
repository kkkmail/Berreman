# Where we are

Step 0173 is an `IMPLEMENT` step in spec 0050 S5.2, the kit start screen and the Launcher. It gives the two texts a
failed kit open shows one home each:
- step 154's `The kit could not be opened: <error>`;
- step 155's `The kit is open elsewhere: <holder>.`.

The launcher and the coming start screen will then read the same words (.spec-md §4, "A reuse is a move or a call").
Verification belongs to the arc-runner after this act-only worker exits.

# What's working

- Added `KitLibraryError.openFailedCaption` and `KitLockHolder.openElsewhereCaption` (`ControlCaption`) in
  `ConstructorUiIdOfDomain.fs`, next to `KitError.caption`. They are now the one home of the two kit-open failure texts.
- `LauncherWindow.showKitError` renders those captions, and both of its literals are deleted. The launcher shows the
  same words as before.
- Added the `KitOpeningTests` fact `the open captions name the error and the lock holder`.

# Tests

No gates were run: under invariant 6 this role does not run checks. The arc-runner still has to run:
- build;
- elevation;
- constructor-unit-tests, all discovered;
- ui-smoke, all discovered;
- ui-tests, `OpticalConstructor.Ui.Tests.LauncherTests`;
- the fourteen s20 section invariants.

The new fact is `OpticalConstructor.Tests.KitOpeningTests.the open captions name the error and the lock holder`. The
constructor count should rise by 1 over step 0172's.

`LauncherTests`' facts of steps 154 to 156 were not edited, because the text they assert is unchanged.

Nothing was measured here. The YAML repeats the latest recorded checkpoints (constructor 1,173 and ui-smoke 224, both
at step 171), not a result of this round.

```yaml
gates:
  constructor_unit_tests: 1173
  ui_smoke_tests: 224
```

# Architecture

- **Where the captions live.** Both are `ControlCaption` extension members in `OpticalConstructor.Ui`'s
  `ConstructorUiIdOfDomain`. That is the lowest project that sees both the Domain error types and the Controls caption
  type, and the same place as `KitError.caption`.
- **How the launcher uses them.** `showKitError` chooses between the two captions by matching
  `KitLibraryKitErr (KitLockedErr holder)`. It reads `.value` only at the TextBlock's `Text` and its automation-name
  setter.

# Deferred

Nothing in this slice is deferred.

# Gotchas

- **Similar texts that are out of scope.**
  - `The library could not be opened: …` (`Program.fs`) is the library-open failure.
  - `The diagnostic could not be opened: …` (`TestLauncherWindow.fs`) belongs to a different window.

  Neither is one of the two kit-open texts, so both stay as they are.
- **The test declares its own holder.** Step 129's `exampleLockHolder` is private to `KitTests.fs`, which this step may
  not edit. The new fact builds its own holder with the slice's values.
- **`specs/0050/.manifest.state.json` belongs to the supervisor.** It was already modified when this worker started,
  and it was not touched.

# Changelog

- 2026-11-08: Moved the launcher's two kit-open failure texts into `KitLibraryError.openFailedCaption` and
  `KitLockHolder.openElsewhereCaption`, made `LauncherWindow` render them, and added one `KitOpeningTests` fact.
  Verification is left to the arc-runner.
