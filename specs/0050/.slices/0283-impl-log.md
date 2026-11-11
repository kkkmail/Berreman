# Step 0283 — impl log

## Progress

- I read the following, and there is no operator note:
  - the task and both system prompts;
  - the project prompt;
  - the slice and its gates snapshot;
  - the step 280 impl log and the step 282 state of the world.
- I read the code the slice cites:
  - `SlotBarView`, the `UiIds` slot-bar group and `ConstructorUiIdOfDomain`;
  - `SetupGrids.fs`, `Setups.SlotRole` / `RoledSlot` and `SetupError.describe`;
  - the window model's `setupGrid` / `slotBarState` / `slotBarHandlers`, and `mainView`'s table surface;
  - `MaterialsWindowView`'s boxes, and the `RotationControls` field rule;
  - the test helpers: `SlotBarTests`, `ScrollExtentTests`, `ItemsPanelTests`' read-only fact,
    `LibraryWindowTests.clickOn` and the `FacetedTreeControlsTests` Enter press.
- I wrote the impl plan.
- `UiIds.fs`:
  - `setupGridOpenButton` closes the slot-bar group;
  - the new group "The setup grid generator (SetupGridView)" holds the four fixed ids and `setupGridCombination`.
- `ConstructorUiIdOfDomain.fs`:
  - `setupGridItemChoice`, `setupGridAngleField` and `setupGridAngle` sit on the extension that holds `slotChip`;
  - `Kit.ItemId.labelAmong` (see Decisions) and `SetupGrids.GridCombination.caption` are at the end of the file.
- New `SetupGridView.fs`. It is compiled between `SlotBarView.fs` and `TableAndElementRotationView.fs` by one
  `<Compile>` line.
- `SlotBarView.fs`: `SlotBarHandlers.openSetupGrid` is added, and the chip row ends with `Setup grid…`, offered under
  any access.
- `TableAndElementRotationView.fs`:
  - `model.setupGridState` sits after `setupGrid`;
  - the private `setupGridHandlers` sits after `slotBarHandlers`, which gains `openSetupGrid`;
  - the table surface docks a vertical stack of the slot bar and the open panel on top.
- The first build was green, with no warning from our code.
- Tests:
  - new `SetupGridPanelTests.fs`, last in the compile list, with the five facts;
  - `ScrollExtentTests` gains `the Main page with the setup grid open leaves everything reachable`;
  - `SlotBarTests`' UiId fact checks `setupGridOpenButton`;
  - `SlotBarTests`' fixture helpers became `internal` and are reused (see Decisions).
- The first test build failed with FS0001 because `Angle.degree` takes `float<deg>`. I fixed it with `22.5<deg>`.
- The 12 facts of the three classes (5 + 6 + 1) passed on their first run.
- Mutation check, `specs/0050/.artifacts/0283-mutation/`:
  - **Round 1** used three mutants, all caught:
    - the Generate/Close row not keyed: the smoke fact went red, because the kit recorded `LP1-(QZ#0)-LP2`, the stale
      grid of the first press;
    - ticked and unticked marks swapped: four facts went red;
    - Generate ignoring the kit's access: masked in the read-only fact by the mark mutant.
  - **Round 2** used the access mutant alone. The read-only fact went red with "SetupGridGenerateButton must be
    absent".
  - Both files were restored byte for byte (`cmp`) and rebuilt. The backups are deleted.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/SetupGridView.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/SlotBarView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SetupGridPanelTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SlotBarTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`

Every file is in the slice's authorized edits.

## Decisions

- **The Generate/Close row is keyed by the combinations Generate makes.**
  - `MaterialsWindowView.actionButton` subscribes its press once per id (`SubPatchOptions.OnChangeOf id.value`), so an
    unkeyed Generate keeps the grid it was first drawn with.
  - The key is `SetupGridActions_` followed by each ticked combination's picks: slot id, item GUID or `-`, and R1 in
    round-trip degrees. `generate` reads only `ticked`, so equal keys send equal setups.
  - This is the repo's own cure: `ItemsPanelView` keys its Add/Set rows and `KitStartView` its offer pair the same way.
  - The mutation check proves the key is needed.
- **Each slot's row is keyed by its slot id (`SetupGridSlot_<id>`).**
  - A tried item is an `actionButton` holding the slot id it was drawn for.
  - When a table edit gives a role to another slot while the panel is open, the row is recreated, not patched with a
    stale slot.
- **The item-label lookup is shared, not copied.**
  - `GridCombination.caption` needs `RoledSlot.chipCaption`'s label rule: the label, or `(not in this kit)`.
  - It moved into one member, `Kit.ItemId.labelAmong`, which both captions call. `chipCaption`'s output is unchanged,
    and `SlotBarTests` still passes.
- **A pick whose slot the table lacks is captioned by its slot id.** The slice names only the role of a slot on the
  table. Such a combination is impossible anyway (`UnknownSetupSlotErr`), so its reason says so.
- **The table surface docks one vertical `StackPanel`** holding the slot bar and, while open, the panel, instead of the
  bar's lone `Border`.
  - The DockPanel's children stay one list literal, and the bar keeps its width bound, so it still wraps.
  - The fact asserts the order bar, panel, table.
- **`setupGridState`** is `Option.map2` over `setupGridOpt` and `setupGrid`. These are `Some` together, and this reuses
  `setupGrid` rather than repeating `gridOver`.
  - `generateAvailability` is a match over `kitWrites`, the axes and `ticked`, with no `bool`.
- **The angle field** follows the rotation bar's rule:
  - its text is read by `BarDegrees.tryCreate` on Enter or blur;
  - a read value goes to `addAngle` and the field is cleared;
  - an unreadable text is left as typed and ignored.

  The field carries no `text` attribute, so it stays uncontrolled. Its two subscriptions are `SubPatchOptions.Always`,
  as the slot bar's chip handlers are (step 280).
- **Visuals the slice leaves open:**
  - role labels are docked left with `minWidth 90`, so the rows line up;
  - the field has the boxes' right and bottom margins;
  - an impossible line wraps.
- **Test fixture reuse.**
  - `SetupGridPanelTests` reuses `SlotBarTests`' scene and window helpers, made `internal`, rather than copying them:
    `withScene`, `withItemAdded`, `itemLabelled`, `linearPolarizerChoice`, `plateChoice`, `withMainWindow`,
    `controlsUnder`, `tryControlById`, `controlById`, `assertAbsent`, `boxLabelOf`, `topLeftOf`, `pressBox` and
    `latestOf`.
  - `SlotBarTests.fs` is in the authorized edits, and none of its facts changed beyond the UiId check.
- **Assertions added inside the authorized facts:**
  - the fixture asserts the light order Source, Prep 1, Obj 1, Analyzer 1, Detector 1;
  - the empty draft shows `☑ The table as it is`;
  - the lines carry `SetupGridCombination_1..4`;
  - the angle boxes read `0° ×` and `45° ×`;
  - tried items are on `positiveBackground`;
  - the first three-pick line reads `☑ Prep 1: LP1 @ <R1>, Obj 1: QZ @ 0°, Analyzer 1: LP2 @ <R1>`;
  - impossible lines are `TextBlock`s in `messageColor` and the possible ones are boxes;
  - Generate and Close have positive and negative fills;
  - unticking the only combination withholds Generate;
  - Close leaves `setupGridOpt = None`;
  - the read-only grid takes its edit and shows its line;
  - the typed angle clears the field and shows `45° ×`.

## Testing state

Development runs used the snapshot's own commands on the final tree. None exceeded its effective deadline.

| Gate | Result | Time (deadline) |
|---|---|---|
| build | pass, 0 errors and no warning from our code (incremental, after the last edit) | 15 s (600 s); the first full build was 76 s, with only vendored MathNet `SYSLIB0051` |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0; re-run after the last source edit | 200 s (600 s); the first run was 206 s |
| constructor-unit-tests | pass, 1472 of 1472, unchanged | 74 s (180 s), run beside the first elevation run |
| ui-smoke | pass, 248 of 248 (247 before, +1) | 203 s (600 s) |
| ui-tests (19 selected classes) | pass, 374 of 374 (369 before, +5) | 178 s (360 s) |
| invariant-s28-* (16) | every grep finds nothing, also with `--untracked` | — |

- In the TRX reports, all seven named facts show `Passed`:
  - in `ui-smoke.trx`, the smoke fact;
  - in `ui-tests.trx`, the four `SetupGridPanelTests` facts without a trait, the `ScrollExtentTests` fact and the
    changed `SlotBarTests` UiId fact.
- Elevation's "suppressed" tally is `ELEV004`, which the gate excludes and only measures. The Ui project's tally is
  518. The new `.value` reads are at render lines:
  - the slot row's key;
  - the captions' labels;
  - the reason text.
- The only interim source edit was the doc reflow in `SlotBarView.fs`. It came after the first elevation run had
  started, so the build and elevation were run again on the final tree.

## Artifacts

- `specs/0050/.artifacts/0283-build.log`
- `specs/0050/.artifacts/0283-elevation.log`
- `specs/0050/.artifacts/0283-constructor-unit-tests.log`
- `specs/0050/.artifacts/0283-ui-smoke.log`
- `specs/0050/.artifacts/0283-ui-tests.log`
- `specs/0050/.artifacts/0283-setupgrid-tests.log`: the three classes alone, first green run
- `specs/0050/.artifacts/0283-mutation/`: the two mutation rounds' build and test logs
- `specs/0050/.artifacts/0283-invariant-cmds.txt` and `0283-invariants.log`

## Gotchas

- **The slice writes `RotationControls.BarDegrees`, which does not resolve.** It is the namespace-level `BarDegrees`, so
  the handler uses that (step 282's gotcha).
- **`Angle.degree` takes `float<deg>`.** The UiId fact writes `22.5<deg>` with `open Berreman.Constants`.
- **The fixture's second polarizer is the analyzer.** `AddElement` places it behind the sample. The fixture asserts the
  light order rather than assuming it.
- **The empty draft's single combination is ticked.** `☑ The table as it is` is offered, but Generate is withheld
  because the grid has no axis.
- **A one-pick untick stops being listed** once the polarizers become axes. The draft keeps it, but `gridOver` lists only
  unticks among the current combinations, so the fact's eight-line grid shows every possible line ticked.
- **No python on this machine.** Edits used the Edit tool, sed and perl.
- **Invariant 6 and local runs.** I ran no gate as the gate engine: the snapshot's commands above are development runs,
  as earlier steps recorded them, and the engine's verdict decides.
