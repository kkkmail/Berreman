# Step 0284 — state of the world

## Where we are

Spec 0050 section S8.4 covers the slot bar, the quick swap, the direct problem and the grid generator's controls. Step
280 added the slot bar, steps 281–283 the grid panel, and step 242 put `mainView`, which holds both, into every
constructor window's Setups and Experiments steps. This step proves the direct problem and the quick swap through the
kit window itself, over a real kit folder that is closed and opened again. It also records the slot bar in `AGENTS.md`,
where every agent reads first (001 §J "Quick swap", "Direct problem"; .spec-md §9 "UI tests"). No product code changes.

## What's working

- Add the `AGENTS.md` gotcha **The slot bar is the quick swap**: chips in light order, `SwapSlot`, Tab and letters,
  `RecordExperiment` versus `ExpCommit`, and `Setup grid…` over `SetupGridDraft` / `GenerateSetupGrid`.
- Add the ui-smoke acceptance fact to `KitWindowTests`. In a kit window over a kit folder, Record experiment twice, a
  swap through the Obj 1 chip and Record experiment again write `Bench.setup-json`, `Bench (2).setup-json` (holding
  QZ2) and E1, E2 over `'Bench' revision 1` and E3 over `'Bench (2)' revision 1`.
- The fact reopens the folder through `openKitAt` and `loadKit`. `kitSetups` hold Bench and Bench (2), the collection
  holds E1–E3, and the reopened kit window lists both saved setups and the three experiment rows.
- Items are added through the Items panel's messages, from the open add form's own offers (`withOfferAdded`).
  `kitFilesIn` now serves both the item files and the new setup and experiment file listings.

## Tests

These are development runs on the final tree, using the snapshot's own commands. The arc-runner's gate engine runs the
gates themselves. No run went over its effective deadline.

- **build:** exit 0, with 0 errors and 0 warnings, in 28 s incremental (deadline 600 s). The first full build (77 s)
  showed only the vendored MathNet `SYSLIB0051` warnings, which are not ours.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, in 202 s (deadline 600 s).
- **ui-smoke:** passed, 249 of 249 (248 before, +1, the new fact), in 219 s (deadline 600 s). The TRX shows the new fact
  `Passed`.
- **ui-tests** (selected `KitWindowTests`): passed, 16 of 16, unchanged, in 23 s (deadline 360 s).
- **invariant-s28-\*** (16): every grep finds nothing, with and without `--untracked`.
- **Mutation check:** the slot bar's item press was made to send `AssignSlot` instead of `SwapSlot`. The new fact went
  red at the swap's status line. The product file is restored byte-for-byte.

```yaml
gates:
  ui_smoke_tests: 249
  ui_tests: 16
```

## Architecture

- No product change. `kitWindowView` already composes the slot bar and the grid panel through `mainView`.
- The fact's model reads `ctx.itemModels.overlaidWith opened.overlay`, as the App's private
  `ConstructorScene.withAppContext` sets it for real windows. `kitWindowModel` is unchanged, so no other fact's fixture
  moves.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- `kitWindowModel` (`initMainWithKit` + `loadKit`) keeps `initMainWithKit`'s empty in-memory item-model store, so its add
  form offers no binding model. A fact that adds items through the add form must give the model the app's item models.
- `ItemModelProxy.overlaidWith` is an extension in `OpticalConstructor.Domain.KitLibrary`. `KitWindowTests` now opens that
  module.
- `KitWindowTests` compiles before `SlotBarTests`, so the slot-bar fixture helpers cannot be called from it.
- The kit folder is `TestAppScope.freshKitFolder owner`, the copy facts' builder, rather than the slice's literal
  `<BaseDirectory>/KitWindowTests/<GUID>` (see the impl-log's Decisions).
- In ordinal order `Bench (2).setup-json` sorts before `Bench.setup-json`.

## Changelog

- 2026-11-11: Step 0284 proves the direct problem and the quick swap in a kit window over a kit folder that is reopened
  afterwards, and adds the slot-bar gotcha to `AGENTS.md`. ui-smoke went from 248 to 249, and the selected ui-tests
  (`KitWindowTests`) stayed at 16.
