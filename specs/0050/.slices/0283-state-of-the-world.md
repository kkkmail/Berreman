# Step 0283 — state of the world

## Where we are

Spec 0050 section S8.4 covers the slot bar, quick swap, the direct problem and the grid generator's controls. Earlier
steps built the parts:

- step 274 (S8.3) added the pure `SetupGrid`;
- step 276 added `GenerateSetupGrid`;
- step 280 added the slot bar;
- steps 281 and 282 added the grid panel's draft and its six semantic messages.

This step draws the panel. `SetupGridView` sits between the slot bar and the table, and the bar's new `Setup grid…`
opens it (001 §J "Grid generator", §O `SetupGridGenerateButton`).

## What's working

- Add the setup grid panel (`SetupGridView`) between the slot bar and the table. Each slot's row lists the kit's
  fitting items, an R1 field and the R1 angles being tried.
- Mark each combination ☑, ☐ or ✗ with the reason. Generate sends `GenerateSetupGrid` and is offered only for a ticked
  combination over a slot in a writable kit.
- Add `Setup grid…` to the slot bar. It opens the panel, including in a read-only kit.
- Add `model.setupGridState`, `setupGridHandlers`, the setup grid UiIds and `GridCombination.caption`.
- Add `SetupGridPanelTests` (five facts) and the ScrollExtent fact for Main with the grid open, and extend the slot bar's
  UiId fact.

## Tests

These are development runs on the final tree, using the snapshot's own commands. The arc-runner's gate engine runs the
gates themselves. No run went over its effective deadline.

- **build:** exit 0, with 0 errors and no warning from our code, in 15 s (deadline 600 s).
  - This is the final incremental build after the last source edit, a reflowed doc line in `SlotBarView.fs`.
  - The first full build (76 s) showed only the vendored MathNet `SYSLIB0051` warnings. These are pre-existing and not
    ours.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)` in 200 s (deadline 600 s), with known findings 0.
  This is the re-run on the final tree, after the last source edit. The first run gave the same result in 206 s.
- **constructor-unit-tests:** pass, 1472 of 1472, in 74 s (deadline 180 s). This is unchanged, as the step adds no
  constructor test.
- **ui-smoke:** pass, 248 of 248, in 203 s (deadline 600 s). That is 247 plus 1, the new smoke fact, which shows
  `Passed` in `ui-smoke.trx`.
- **ui-tests (19 selected classes):** pass, 374 of 374, in 178 s (deadline 360 s).
  - That is 369 (step 280's figure for the same classes without the new ones) plus 5.
  - All six named facts show `Passed` in `ui-tests.trx`:
    - four `SetupGridPanelTests` facts;
    - `the Main page with the setup grid open leaves everything reachable`;
    - the changed `the slot bar UiIds are the stable intent-named ids`.
- **invariant-s28-\* (all sixteen):** every grep finds nothing, also with `--untracked`
  (`specs/0050/.artifacts/0283-invariants.log`).
- **Mutation check** (`specs/0050/.artifacts/0283-mutation/`):
  - the unkeyed Generate row, the swapped marks and Generate ignoring the kit's access each turned a fact red;
  - the access mutant was run alone in a second round, because the mark mutant masked it;
  - the files were restored byte for byte and rebuilt.

```yaml
gates:
  constructor_unit_tests: 1472
  ui_smoke_tests: 248
  ui_tests: 374
```

## Architecture

- **`SetupGridView` is a pure projection** of `SetupGridState` and `SetupGridHandlers`, like `SlotBarView`.
  - The window model computes the state in one member, `setupGridState`. It is `Option.map2` over `setupGridOpt` and
    `setupGrid`, so `gridOver` is not repeated.
  - Marks come from `combination.slotsOver` (impossible, with `describe`), then `grid.ticked`.
- **No message, model field or option is added.** The panel sends step 282's six messages and step 276's
  `GenerateSetupGrid`, and the bar sends `OpenSetupGrid`.
- **The Generate/Close row is keyed by the ticked combinations' picks**, because `MaterialsWindowView.actionButton`
  subscribes its press once per id (`ItemsPanelView` and `KitStartView` use the same cure).
  - Each slot row is keyed by its slot id, so a tried item's `actionButton` never keeps a stale slot.
- **The item-label lookup is shared.** `Kit.ItemId.labelAmong` is the one rule for "label or `(not in this kit)`", used
  by `RoledSlot.chipCaption` and `GridCombination.caption`.
- **The table surface docks a vertical `StackPanel`** of the slot bar and, while open, the panel above the table.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **Never pass captured data through an `actionButton` without keying its container.** Its press is subscribed once per
  id. The mutation check shows an unkeyed Generate recording `LP1-(QZ#0)-LP2`, the grid from the first press.
- **`RotationControls.BarDegrees` does not resolve.** `BarDegrees` is declared at namespace level, so
  `SetupGridHandlers.addAngle` takes `BarDegrees`, the same type (step 282's gotcha).
- **`Angle.degree` takes `float<deg>`.** Tests write `22.5<deg>` with `open Berreman.Constants`.
- **A grid with no axis has one combination**, `☑ The table as it is`. It is ticked, but Generate is withheld because
  the grid has no axis.
- **`SetupGridPanelTests` reuses `SlotBarTests`' helpers**, now `internal`. A later change to those helpers reaches both
  classes.
- **The draft's earlier untick of a one-pick combination stops being listed** once another slot becomes an axis, because
  `gridOver` lists only unticks among the current combinations. The three-axis grid in the fact therefore shows every
  possible line ticked.

## Changelog

- 2026-11-11: Step 0283 draws the setup grid panel (`SetupGridView`) between the slot bar and the table, and opens it
  from the bar's `Setup grid…`. ui-smoke went from 247 to 248, the selected ui-tests from 369 to 374, and
  constructor-unit-tests stayed at 1472.
