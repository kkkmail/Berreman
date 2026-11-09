# Step 0229 — state of the world

## Where we are

Spec 0050, section S6 ("Send and receive kits; copy experiments between kits"), step 0229 (IMPLEMENT), obligations
`S72-COPY-VIEW` and `S6-COPY`.

- Steps 227 and 228 made Copy experiments map or add the items the copied experiments use, behind the kit window's
  messages (`KitCopySource.itemRows`, `ChooseCopiedItem`).
- This step draws that mapping table in the kit bar's copy panel. The item mapping can now be chosen from the
  window, so experiment copying includes item mapping before S7.

## What's working

- Added the copy panel's item-mapping table. Each item the chosen experiments use gets one block: its label and
  model version, then `Add` and a `Use <label>` choice per item of the kit, with the row's choice filled.
- The table sits inside the experiment list's bounded scroller (160 px), so the kit bar is no taller and every
  control stays reachable.
- Added the `kitCopyItemList` UiId and `kitCopyItemChoice <source item> <choice>`
  (`KitCopyItemChoice_<source GUID>_Add` / `_<kit item GUID>`).
- Added three `KitWindowTests` facts.
  - The ui-smoke fact presses `Add`, and in a second branch `Use <label>`, then Copy.
  - It then reopens the kit and checks which piece the copied experiment is bound to.
- The kit-window scroll law now holds with forty chosen experiments and twenty item rows.

## Tests

Development runs over the final source, recorded in `0229-impl-log.md`. The gate engine gives the verdict
(Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors, and no lowercase `error` in the log.
- **constructor-unit-tests** (all discovered): **1348** passed, 0 failed, 0 skipped (unchanged).
- **ui-tests** over `KitWindowTests` and `ScrollExtentTests`: **25** passed, 0 failed, 0 skipped.
  - That is 23 + `the copy-mapping UiIds are the stable intent-named ids` + `an open source lists the items its chosen
    experiments use, each with Add and the kit's items, the choice marked`.
  - The changed `the kit window: the kit bar and the page stay reachable` passed.
  - All three are `Passed` in the TRX.
- **ui-smoke** (all discovered): **239** passed, 0 failed, 0 skipped.
  - That is 238 + `pressing a mapping choice and Copy adds or maps the item as chosen`, which is `Passed` in the TRX.
  - It took 196 s against the 600 s `attempt_timeout_seconds`; no deadline was exceeded.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 195 s;
  - 0 known findings;
  - 1317 suppressed findings (excluded-rule ELEV004 hits; 1315 at step 228).
- **The fourteen `invariant-s22-*` gates:** each holds.
- **The facts bite.** Eight mutants of the view and the id derivation were each caught, and the sources were
  restored byte for byte, then rebuilt:
  - the table never shown;
  - the table outside the scroller, caught also by the scroll law;
  - the choice never marked;
  - every choice sending Add;
  - no Use choices;
  - the intro shown without rows;
  - a mapped id repeating the source GUID;
  - the line showing the binding instead of the model.
- Logs are in `.artifacts/0229-dev-*.log` and `.artifacts/0229-mutants/`.

```yaml
gates:
  constructor_unit_tests: 1348
  ui_smoke_tests: 239
  ui_tests: 25
```

## Architecture

- **The table is a pure projection of `kitCopyOpt.itemRows` and `kitItems`.**
  - Each choice sends step 228's `ChooseCopiedItem (source id, choice)`, and Copy sends `CopyChosenExperiments`, so
    the press and a test drive the same update.
  - Storage still decides what is added or mapped (step 227's `itemsToAdd`). The view adds no mapping logic.
- **One scroller bounds the panel.** The bar sits outside the page's scroll (step 188), so the table shares the
  toggles' `MaxHeight` 160 viewer. A long source or many items never pushes a bar control or the page out of reach.
- **Ids follow the identity rule.** A choice's id is its source `ItemId`, then `Add` or the target `ItemId`, never a
  label or a preset, because one id is one physical piece (spec 0050 S7.2).
- **The styling reuses the kit bar's.** The chosen box is `MaterialsWindowView.actionButton` on `positiveBackground`
  and the others are `verbButton`s, so a filled box reads as the choice.

## Deferred

Nothing in this step's scope is deferred.

## Gotchas

- **A choice box switches helpers in place.** One keyed `Border` turns from a `verbButton` into an `actionButton`
  when it is chosen. Its message is fixed by its id, so `actionButton`'s `OnChangeOf id` press closure is safe.
- **`Use <label>` boxes do not wrap.** A label wider than the copy panel would be clipped sideways. Lab labels are
  short, and the scroll fixture's fit at 800 × 600.
- **A `MapOntoKitItem` naming an id that `kitItems` lacks marks no box.** The copy refuses such a row with a typed
  error (step 227).
- **`ItemModelVersionId.entryReference` is an extension in `OpticalConstructor.Domain.LibraryEntryFiles`.**
  `KitWindowTests.fs` now opens that module.
- **The brief's "both source pieces sharing that model" follows step 227's census.**
  - The source holds the assigned Laser600 and an unassigned `Laser600 spare` of the same preset.
  - The Use target T is a third-id `Laser600 bench` with its own `intensityScale`.
  - After the reopen, E1 resolves to exactly T's id and parameters, and neither source piece is in the kit.
- **`tallestKitBar` gives the kit three items** so each of its twenty rows offers `Add` and three `Use` choices. With
  none, each row would offer `Add` alone, which is not the bar at its tallest.

## Changelog

- 2026-11-09 — Step 0229:
  - added the copy panel's item-mapping table (`kitCopyItemList`, `kitCopyItemChoice`) inside the bounded experiment
    list;
  - added three `KitWindowTests` facts: the ids; the list over a real source; the ui-smoke press of `Add` and
    `Use <label>` with a reopen;
  - grew the tallest kit-bar scroll fixture to forty chosen experiments and twenty item rows over three kit items;
  - ui-tests 23 → 25 over the selected classes, ui-smoke 238 → 239.
