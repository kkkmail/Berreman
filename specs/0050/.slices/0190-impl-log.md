# Step 0190 — impl log

## Progress

- Read the protocol, the project prompt, the slice and the gate snapshot. Also read manual 013, whose arc-origin
  elevation scope applies from 0160 on. No other manual ruling names step 0190, and no operator note is in flight.
- Grepped for exhaustive matches over `WindowKey`. Only `WireUiFinalCompositionTests.rootWindowFactory` lists
  every case; the other sites use wildcards.
- Grepped `Berreman/` for the new names (`KitWindowKey`, `KitWindowPresence`, `KitWindowActivated`,
  `NoKitWindowOpen`, `windowKey`, `activateKitWindow`). There is no prior use, so the ELEV014/015/016/019 uniqueness
  rules hold.
- Implemented (1)–(6) of the slice, as below. Built, then ran the development checks listed under Testing state.

## Files modified

- `OpticalConstructor.Ui/WindowLauncher.fs`: `WindowKey` gains `KitWindowKey of Kit.KitFolder`, with a one-line doc
  (spec 0050 S5.3).
- `OpticalConstructor.Ui/KitOpening.fs`:
  - `KitWindowPresence` = `KitWindowActivated` | `NoKitWindowOpen`.
  - `type KitFolder with`:
    - `windowKey` = `KitWindowKey this.canonical`;
    - `activateKitWindow ()` runs `openOrActivate this.windowKey` on `WindowLauncher.create`, over a factory that
      answers `WindowNotRegisteredErr` of its key, `SelectWindowModality.defaultValue` and `BrowseOpen`. Only
      `Ok (ActivatedWindow _)` is `KitWindowActivated`; every other answer is `NoKitWindowOpen`.
  - The `WindowLauncher.` and `WorkbenchSettings.` names are qualified, not opened. This keeps their case names
    (`Persisted`, `BrowseOpen`, …) out of the module's scope.
- `OpticalConstructor.Ui/KitStartView.fs`:
  - `KitStartContext.activateKitWindow : KitFolder -> KitWindowPresence` goes after `showKit`.
  - A private `kitStartChosen` runs it first for `CreateKit`, `OpenKit` and `OpenKitReadOnly`:
    - `KitWindowActivated` answers the model with no offer and no status, and opens, bumps and hands over nothing;
    - `NoKitWindowOpen` runs step 177's open unchanged. The open is now a thunk, so it never runs on an activation.
  - `updateKitStart`'s doc gains one line for this.
- `OpticalConstructor.Ui/KitStartWindow.fs`: composes `activateKitWindow`. On `KitWindowActivated` it calls
  `this.Close ()` and answers `KitWindowActivated`; on `NoKitWindowOpen` it answers `NoKitWindowOpen`.
- `OpticalConstructor.Ui.Tests/WireUiFinalCompositionTests.fs`: `rootWindowFactory` answers `KitWindowKey _` with
  `Error (WindowFactoryFailedErr (key, "a kit window opens over its opened kit"))`. Its doc says that the kit hand-off
  builds a constructor window, never this factory.
- `OpticalConstructor.Tests/KitStartTests.fs`:
  - the fixture context passes `activateKitWindow = fun _ -> NoKitWindowOpen`;
  - new fact `a kit whose window is open is activated, not opened, bumped or handed over`.
- `OpticalConstructor.Ui.Tests/KitStartWindowTests.fs`: the fixture context passes
  `activateKitWindow = fun _ -> NoKitWindowOpen`. No assertion changed.
- `OpticalConstructor.Tests/KitOpeningTests.fs`:
  - `open OpticalConstructor.Ui.WindowLauncher`;
  - new fact `two spellings of one kit folder are one window key, and no window holds a new kit`.

What the new `KitStartTests` fact covers:

- The kit is made by `openKitAt` and closed. The history is seeded with an older entry of the kit's folder.
- The context records each `activateKitWindow` call, and answers `KitWindowActivated` when the chosen folder's
  `windowKey` is the kit's.
- `OpenKit`, `CreateKit` and `OpenKitReadOnly` of the folder each start from a model carrying a `MakeKitOffer` and a
  status. Each one:
  - records exactly one activation and shows no kit;
  - answers no offer and no status;
  - leaves the stored and model history at the seed, and keeps the rows;
  - leaves the folder without `.oc/lock`, with every byte unchanged.

What the new `KitOpeningTests` fact covers:

- `C:/kits/a` and `C:\KITS\A\` give equal `windowKey`s, both equal to `KitWindowKey` of the canonical folder.
- `activateKitWindow ()` of a GUID-named folder under the test output answers `NoKitWindowOpen`, and creates no
  folder.

## Testing state

These are development runs only. Under Invariant 6, the gate verdict belongs to the arc-runner's gate engine. Logs
are under `specs/0050/.artifacts/`.

- **Build:** `dotnet build Berreman.slnx -c Release` exits 0 with 0 errors (`0190-build.log`). The only warnings
  are the two `SYSLIB0051` warnings from vendored `MathNetNumerics/Numerics/Exceptions.cs`, which were already
  there. No owned-code warning.
- **constructor-unit-tests:** 1209 passed, 0 failed (`0190-constructor-unit-tests.log`). That is 1207 + 2, the two
  declared facts. A filtered run of the two facts alone passed 2.
- **ui-smoke:** 230 passed, 0 failed (`0190-ui-smoke.log`).
- **ui-tests over `OpticalConstructor.Ui.Tests.KitStartWindowTests`:** 6 passed (`0190-ui-tests-kitstartwindow.log`).
- **Whole non-smoke UI suite:** 649 passed, 0 failed (`0190-ui-tests-whole.log`).
- **Elevation:** run with `ASG_ARC_BASE_COMMIT=f395597…` (`.arc-base-json`). Result: `ELEV-GATE: OK`, 0 new
  findings in 12 of 12 projects, 195 s, ELEV004 excluded (`0190-elevation.log`).
- **The fourteen s21 invariant greps:** none finds anything.
- **Line endings:** no CR in any touched file. `git diff --numstat` matches `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0190-build.log`
- `specs/0050/.artifacts/0190-constructor-unit-tests.log`
- `specs/0050/.artifacts/0190-ui-smoke.log`
- `specs/0050/.artifacts/0190-ui-tests-kitstartwindow.log`
- `specs/0050/.artifacts/0190-ui-tests-whole.log`
- `specs/0050/.artifacts/0190-elevation.log`
- `specs/0050/.artifacts/0190-testresults/` (the constructor run's results directory)

## Gotchas

- **For now, `activateKitWindow` always answers `NoKitWindowOpen` in the app.** Nothing registers a `KitWindowKey`
  until step 191's hand-off opens the constructor windows through the launcher. No fact in this step's census
  registers a real window under the key, because `OpticalConstructor.Tests` has no Avalonia platform. Step 191's
  `LauncherTests` facts cover the activated path end-to-end.
- **`activateKitWindow` is safe without a UI platform.** With no live key, `openOrActivate` only reads the empty
  registry map and calls the erroring factory. No `Window` API is touched.
- **An `Activate` that throws is `NoKitWindowOpen`.** The real launcher maps it to `WindowFactoryFailedErr`, and the
  slice maps every non-activation answer to `NoKitWindowOpen`. The start screen then tries the open, which meets
  this process's own lock and is offered read-only.
- **Interpretation:** the new start-screen fact starts each message from a model carrying a pending offer and a
  status. This shows that activation clears both ("answer no offer and no status"); a fresh model has neither.
