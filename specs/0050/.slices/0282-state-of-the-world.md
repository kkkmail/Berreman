# Step 0282 — state of the world

## Where we are

Spec 0050 section S8.4 covers the slot bar, quick swap, the direct problem and the grid generator's controls. Step 274
(S8.3) added `SetupGrid`, and step 276 added `GenerateSetupGrid`, which records a grid. Step 281 added the pure
`SetupGridDraft`, the panel's editable options. This step adds the panel's semantic messages over the window model. The
panel and the "Grid…" button that send them are step 283's, so nothing is drawn yet (001 §J, §P; AGENTS.md "Every user
action is a semantic Elmish message").

## What's working

- Add `setupGridOpt` to the window model: the grid panel's draft while the panel is open, `None` from `initWith`.
- Add `model.setupGrid`: the open draft's `gridOver` the table, kit items and resolved models, `None` while closed.
- Add `OpenSetupGrid`, `CloseSetupGrid`, `ToggleSetupGridItem`, `AddSetupGridAngle`, `RemoveSetupGridAngle` and
  `ToggleSetupGridCombination`. Each edits the open draft through its Domain member and writes nothing.
- Leave `GenerateSetupGrid` unchanged. It records `model.setupGrid` and leaves the draft open, so the grid can be edited
  and sent again.
- Add three `SetupModelTests` facts: the draft's product and that the edits write nothing; untick, close and reopen;
  and generation of the ticked setups with the panel left open.

## Tests

These are development runs on the final tree, using the snapshot's own commands. The arc-runner's gate engine runs the
gates themselves. No run went over its effective deadline.

- **build:** exit 0, with 0 errors and 0 warnings on the final incremental build, in 16 s (deadline 600 s).
  - Every copy of `Softellect.Berreman.OpticalConstructor.Ui.dll` is newer than the restored source, so the Ui project
    was recompiled.
  - The first full build (73 s) also recompiled vendored MathNet. It showed only that project's pre-existing
    `SYSLIB0051` warnings (`MathNetNumerics/Numerics/Numerics.csproj`), none from our code.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)` in 202 s (deadline 600 s), with known findings 0.
  The source it analyzed is byte-identical to the final tree (`cmp` after the mutation check was restored).
- **constructor-unit-tests:** pass, 1472 of 1472, in 68 s (deadline 180 s). That is 1469 (checkpoint 280) plus 3. All
  three new facts show `Passed` in the TRX:
  - `the grid panel's draft lists the product of the items and angles tried in a slot, and writes nothing`;
  - `unticking a combination in the panel leaves it out, and closing the panel forgets the draft`;
  - `the panel's grid generates its ticked setups through GenerateSetupGrid and stays open`.
- **ui-smoke:** pass, 247 of 247, in 201 s (deadline 600 s). The count is unchanged.
- **ui-tests (selected: `InverseConstructorTests`, `SetupSaveLoadTests`):** pass, 18 of 18, in 26 s (deadline 360 s).
  This step adds no UI test, so the count is unchanged.
- **invariant-s28-\* (all sixteen):** each grep finds nothing, also with `--untracked`
  (`specs/0050/.artifacts/0282-invariants.log`).
- **Mutation check** (`specs/0050/.artifacts/0282-mutation/`):
  - Round 1 used four mutants: no angle wrap, `OpenSetupGrid` always empty, `CloseSetupGrid` keeping the draft, and
    `setupGrid` dropping the draft's unticks. All three new facts went red and the other 26 `SetupModelTests` stayed
    green.
  - Round 2 used only the open and close mutants. The second fact went red at "opening again keeps the draft".
  - The close mutant was masked behind the open one. The fact checks `closed.setupGridOpt = None` directly.
  - The file was then restored byte for byte and rebuilt.

```yaml
gates:
  constructor_unit_tests: 1472
  ui_smoke_tests: 247
  ui_tests: 18
```

## Architecture

- All six arms write nothing, so none goes through `guardKitWrite`, and a read-only kit takes them all (as
  `ToggleSlotChip` and `AssignSlot` do).
- The four edits share one private helper, `withGridDraftEdited`: the open draft mapped, or the model itself while the
  panel is closed. "With none open each leaves the model as it is" is therefore one rule, which a fact checks with
  `Assert.Same`.
- `OpenSetupGrid` keeps an open draft (`Option.defaultValue SetupGridDraft.empty`). `CloseSetupGrid` forgets it.
- `AddSetupGridAngle` converts through `BarDegrees.wrappedAngle`, the table's angle convention, so −45° on the bar is
  R1 315°.
- `model.setupGrid` sits beside `slotChipItems` on the extension that holds `tryItemModelChoice`. Models resolve through
  the stored version via `ItemModelProxy` (step 216's payloads), and none is manufactured.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- The slice writes `RotationControls.BarDegrees`. `BarDegrees` is declared at namespace level in
  `OpticalConstructor.Controls` (`RotationControls.fs:45`), outside the `RotationControls` module, so that qualified
  name does not resolve. The case is `AddSetupGridAngle of Library.ElementId * BarDegrees`, the same type. Step 283's
  `SetupGridHandlers.addAngle` uses the same qualified name and will need the same reading.
- The slice's pointer `TableAndElementRotationView.fs:815` for the angle convention has moved. `wrappedAngle` is on the
  `BarDegrees` extension near line 1190.
- Step 272's table already has its sample at R1 45°, holding QZ. Generating (QZ, 45°) still counts as a new setup
  ("3 new"), because the table is not yet a kit setup.
- The draft keeps its unticks after generation, and `setupGrid` is unchanged by it. Sending the same grid again
  re-uses the setups the kit now holds (step 276).

## Changelog

- 2026-11-11: Step 0282 adds the grid panel's six semantic messages, `setupGridOpt` and `model.setupGrid` to the window
  model. constructor-unit-tests went from 1469 to 1472. ui-smoke stayed at 247, and the selected ui-tests at 18.
