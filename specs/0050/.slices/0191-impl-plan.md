# Step 0191 — impl plan (WIRE_UI)

## Approach

Step 0190 gave the host-layer `WindowRegistry` a key for constructor windows (`WindowKey.KitWindowKey`, via
`KitFolder.windowKey`), and the start screen already asks the registry before it opens a kit
(`KitFolder.activateKitWindow`). Nothing registers a real constructor window yet, so activation always answers
`NoKitWindowOpen` in the app. This step wires the registration at the one place a constructor window is born, the
kit hand-off `KitConstructorWindows.showConstructorIn` in `OpticalConstructor.App/Program.fs`.

`showConstructorIn` opens through `WindowLauncher.create` over:

- a factory that builds `MainConstructorWindow(context, opened)` or `InverseConstructorWindow(context, opened)` by
  mode (step 154's match, moved into the factory);
- `context.settings.selectWindowModality`;
- `BrowseOpen`.

It opens under `opened.folder.windowKey`, and maps the answer:

- `Ok (CreatedWindow _)`: the window is registered and shown, and its `Closed` hook releases the kit (step 154).
  The registry forgets it on close (`WindowLauncher.fs:225`).
- `Ok (ActivatedWindow _)` / `Ok (RetargetedWindow _)`: a window already holds the kit. The opened kit is closed
  again, so that window keeps the kit.
- `Error (WindowFactoryFailedErr (_, reason))`: the opened kit is closed and `Logger.logError` records the reason.
  `WindowNotRegisteredErr` cannot come from an open, but the match is exhaustive, so it takes the same arm and
  carries its key. A failed close is logged, as `openKitAt` logs one (step 150).

No other production code changes.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`: `KitConstructorWindows.showConstructorIn` and
  its doc, one to three lines citing spec 0050 S5.3.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LauncherTests.fs`: two `ui-smoke` facts through the
  launcher and the start screen, as step 180's facts go:
  - `acceptance: choosing a kit whose window is open activates that window and opens no second one`:
    - Main's start screen opens a fresh folder through `newKitButton`, giving one `MainConstructorWindow`;
    - Inverse's start screen, over the same folder, presses `openKitButton`;
    - the press opens no window, closes the screen and raises the Main window's `Activated`;
    - the lock bytes are unchanged, and a read-write open still meets `KitLockedErr` naming this process;
    - this is repeated with the folder written with backslashes and in upper case.
  - `a kit can be opened again once its window is closed`:
    - the kit is made through Main's `newKitButton`, and that window is closed;
    - Main's start screen then opens it again through `openKitButton`, giving one `MainConstructorWindow`.
  - Each fact closes every window it opened.
- `WireUiFinalCompositionTests.fs` is authorized but needs no change. Its root factory already refuses
  `KitWindowKey` (step 190), and the constructor windows it builds directly never pass through
  `showConstructorIn`.

## Risks

- **Activation in headless.** `Activate ()` raises `Activated` even on an already-active window (step 0189's
  log), so the activation count is a real observation.
- **Registry leakage across facts.** Each kit folder is a fresh GUID, so a key that outlived one fact could not
  collide with another fact's key. Every fact still closes its windows in `finally`.
- **Existing launcher facts.** Every one of them closes its constructor window, which forgets the key, and the
  read-only facts open a kit held by a test-owned context, not by a window. So their paths are unchanged.
- **ELEV.** `showConstructorIn` is a private module function with elevated parameters. The factory lambda takes
  `WindowKey`, our type, and returns `Result<Window, _>`, so nothing new is a primitive.
