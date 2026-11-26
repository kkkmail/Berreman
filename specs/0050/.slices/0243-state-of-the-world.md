# Step 0243 — state of the world

## Where we are

Spec 0050, section S7.2 ("The Items panel, the flow strip and item mapping on copy"), step 0243 (WIRE_UI),
obligation `S72-WIRE`.

- Step 241 built the Items panel, and step 242 mounted it in the kit window behind the flow strip.
- Until this step the real constructor windows still held `initWith`'s empty in-memory item-model store, so their
  add form offered no item model.
- This step threads the app's item models, overlaid with the opened kit's snapshots, into both constructor windows.
  An item added in a real window is now a kit file with its model's snapshot, and the kit reopened lists it.

## What's working

- Thread `itemModels = context.itemModels.overlaidWith opened.overlay` into both constructor windows' seed, beside
  `catalogueImports`.
- Document on `MainConstructorWindow` and `InverseConstructorWindow` that their Items panel reads the app's item models.
- Add the ui-smoke acceptance fact: an item added in a real `MainConstructorWindow` is `CPL1.item-json` plus its
  model's `library/…@v1.item-model-json`, and a second window over the same folder lists it.

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster:

- build and elevation;
- constructor-unit-tests and ui-smoke over everything discovered;
- ui-tests over `AppContextTests`, `KitWindowTests`, `LauncherTests` and `TableAndElementRotationTests`;
- the fourteen s24 invariants.

The worker's development runs over the final tree, with logs under `specs/0050/.artifacts/0243-*`:

- **build:** exit 0, `0 Warning(s)`, `0 Error(s)`.
- **constructor-unit-tests:** `Passed!`, 1377 succeeded, in 66 s (estimate 180 s). That is checkpoint 241's 1377,
  unchanged.
- **ui-smoke:** `Passed!`, 245 succeeded, in 199 s (attempt timeout 600 s). That is checkpoint 241's 244 plus
  `acceptance: an item added in a constructor window is a kit file with its model's snapshot, and the window reopened
  lists it`, `Passed` in the TRX.
- **ui-tests (the four selected classes):** `Passed!`, 65 succeeded.
  - By class: AppContextTests 17, KitWindowTests 15, LauncherTests 8, TableAndElementRotationTests 25.
  - The count is unchanged: this step adds no ui-tests fact.
- **elevation** from the arc origin `f395597`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in 199 s.
  Known findings: 0.
- **The fourteen s24 invariant greps,** untracked files included: none finds anything.
- **Red before green:** the new fact was red before the wiring and red again with the wiring line removed
  (`AddItemOffer_33486eec-836a-4cbf-ac79-e3f14439ad4b_v1 must be in the window`). `Program.fs` was restored and
  verified by SHA-256.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1377
  ui_smoke_tests: 245
  ui_tests: 65
```

## Architecture

- **One seam, one record update.** `ConstructorScene.withAppContext` is where both windows take the app's seams. It
  now also takes the `OpenedKit` and sets `itemModels`. The overlay is the step-220/222 `ItemModelProxy.overlaidWith`.
  It is composed beside `KitLibraryStores` (`storesOf`), not inside it, as that member's doc requires.
- `itemIds` stays `initWith`'s `ItemIdProvider.system` (step 239), and the photo picker stays the system dialog the
  view opens (step 241). No other seam changed.

## Deferred

Nothing in this step's scope is deferred.

## Gotchas

- **An offer box can be wider than the window in the headless font.** The circular polarizer's offer is 1314 px wide
  in an 820 px `MainConstructorWindow`, at x = 196, so its centre is off-window and a centre press chooses nothing.
  - The fact presses the centre of the box's part inside the window and every enclosing `ScrollViewer`
    (`pressShownPart`), where a user reaches it.
  - It then asserts the offer reads as chosen.
  - Every other press in `KitWindowTests` is unchanged.
- **The system id provider.** A real window mints item ids with `ItemIdProvider.system`, so the fact reads CPL1's id
  from the kit before it looks up `kitItemRow`.
- **What the fact does not show.** The app's own database also holds the circular polarizer model, so the fact
  shows the app's models reach the window, not the snapshot fallback itself. That fallback is the
  `ItemModelProxy.overlaidWith` contract of steps 220 and 222, covered in `KitLibraryTests`.

## Changelog

- 2026-11-10 — Step 0243:
  - both constructor windows' seed threads the app's item models over the kit's snapshots;
  - the window docs say so;
  - one ui-smoke acceptance fact added.
