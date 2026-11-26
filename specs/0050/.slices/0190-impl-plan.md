# Step 0190 — impl plan

One window per kit (001 §E; spec 0050 S5.3). A start-screen open of a kit whose constructor window is open brings
that window forward and opens nothing. The windows meet in the existing host-layer `WindowRegistry`
(`WindowLauncher.fs`), keyed by the kit's canonical folder; no second registry is written.

## Approach

1. `WindowLauncher.fs`: `WindowKey` gains `KitWindowKey of Kit.KitFolder`, with a one-line doc.
2. `KitOpening.fs`:
   - `KitWindowPresence` = `KitWindowActivated` | `NoKitWindowOpen`;
   - `KitFolder.windowKey` = `KitWindowKey this.canonical`;
   - `KitFolder.activateKitWindow ()` runs `openOrActivate this.windowKey` on `WindowLauncher.create`. The factory
     answers `WindowNotRegisteredErr`, with `SelectWindowModality.defaultValue` and `BrowseOpen`. Only
     `Ok (ActivatedWindow _)` is `KitWindowActivated`.
3. `KitStartView.fs`: `KitStartContext.activateKitWindow` goes after `showKit`. A private `kitStartChosen` helper
   runs it before `CreateKit`, `OpenKit` and `OpenKitReadOnly`:
   - `KitWindowActivated` clears the offer and the status, and opens nothing;
   - `NoKitWindowOpen` runs step 177's open unchanged.
4. `KitStartWindow.fs`: composes `activateKitWindow`, which closes the screen when a window was brought forward.
5. `WireUiFinalCompositionTests.fs`: `rootWindowFactory` answers `KitWindowKey _` with `WindowFactoryFailedErr`, and
   its doc says so.
6. Tests:
   - both start-screen fixture contexts pass `fun _ -> NoKitWindowOpen`;
   - the two declared facts are added to `KitStartTests` and `KitOpeningTests`.

## Risks

- **Exhaustive matches over `WindowKey`.** A grep finds only `rootWindowFactory`.
- **ELEV uniqueness of the new names.** Grep finds no collision.
- **`KitOpening.fs` reaching `WindowLauncher`.** It is compiled after `WindowLauncher.fs`, so it qualifies
  `WindowLauncher.` instead of opening the module, so as not to shadow names.
- **`activateKitWindow` run in `OpticalConstructor.Tests`, with no Avalonia platform.** The registry map is empty
  and the factory errors, so no window API is touched.
