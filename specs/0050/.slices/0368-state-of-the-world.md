# Step 0368 — state of the world

## Where we are

Spec 0050, section S11.2 (the forward model of an experiment and the study residual).

- Step 0367 moved the first half of the table's run rules into Domain: drawn incidence, branch, bound source and
  operating wavelength.
- This step moves the second half: how a slot binds for a run and what the source emits. It follows 001 §N "One
  item-model evaluator", .spec-md §4 "A reuse is a move" and AGENTS.md "Reuse, never copy".
- The forward model can now evaluate a recorded setup's slots in Domain. The table's `chartForRun` and
  `model.runEmission` call the same members, and every chart keeps its numbers and its texts.

## What's working

- Added `ItemEvaluation.SlotRunParam` and `SlotRunError` to Domain. `SlotRunParam` holds the library, the materials,
  the kit's items, the item-model resolver and the values source. `SlotRunError` is `SlotItemErr` | `SlotEvaluationErr`.
- Moved slot binding into `SetupSlot.runElementWith`, `runStepOf` and `runEmissionOf`. A polarizer or sample slot runs
  as its item's model, or else as its preset or sample binding. A source, detector, mirror or lens slot is no run
  element.
- Moved the item resolution onto `SlotRunParam.resolveSlotItem`. `AssignSlot`, `SwapSlot` and the run share it, and
  the UI's copy is deleted.
- `chartForRun` and `model.runEmission` call the Domain members. The pre-resolved item map and the inline binding
  arms are deleted, and the failure charts keep their texts.
- Added three constructor-unit facts to `ItemEvaluationTests`: slot binding, item errors, and source emission.

## Tests

These are development runs of each gate's own command from `0368.gates`, over the final tree:

- **build:** exit 0, `0 Error(s)`, 22–54 s (deadline 600 s). The only warnings are the exempt NU1902.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 224s` (deadline 600 s).
  - It ran from the arc origin through `ASG_ARC_BASE_COMMIT`.
  - 1519 ELEV004 findings suppressed, 0 known findings.
- **constructor-unit-tests:** `Passed!`, 1614 of 1614 (1611 + 3), 71 s (deadline 180 s). Passed:
  - `a slot runs as its item through the given values, else as its library binding, and a source, detector, mirror or lens slot is no run element`;
  - `a slot whose item the kit lacks or whose model does not resolve answers SlotItemErr`;
  - `the run's source emits its item's state through the given values, and the default set without a source item`.
- **ui-smoke:** `Passed!`, 258 of 258, 228 s (allowance 600 s).
- **ui-tests** (the six selected classes): `Passed!`, 99 of 99, 107 s (deadline 360 s).
  - No UI test file changed, so the count over these classes is unchanged.
  - 0367's 101 included `OutOfBandBadgeTests`, which this step's selection does not.
- **invariant-s37-\* (20):** each decoded `--find-nothing` grep finds nothing over the final tree.

No run went past its deadline.

```yaml
gates:
  constructor_unit_tests: 1614
  ui_smoke_tests: 258
  ui_tests: 99
```

## Architecture

- **`SlotRunParam` is the one input a slot runs over.**
  - It is `[<ReferenceEquality>]`, because it holds the `choiceOf` and `valuesOf` functions.
  - The table builds it once, as `member internal model.slotRunParam`, from its library, its material store, its kit
    items, `tryItemModelChoice` and each item's own parameters.
  - The Domain members take it whole, so a recorded setup's forward model can supply other values through
    `valuesOf`.
- **`resolveSlotItem` lives on `SlotRunParam`.** The run, `AssignSlot` and `SwapSlot` all resolve an item through it.
  This keeps the UI's earlier promise of "one resolution", and its error texts are unchanged.
- **`SetupSlot.runElementWith` / `runStepOf` / `runEmissionOf` are optional extensions in `ItemEvaluation`.** The
  table's view already opens that module. `ItemEvaluation.fs` now opens `Placement` and `Setups`; both are compiled
  earlier.
- **`ItemEvaluation.sequenced` is generic in its error.** The run sequences `SlotRunError` path steps through the one
  ordered collector (manual 016), not a second fold.
- **One builder path in `chartForRun`.** Every bound element, a polarizer preset included, goes through
  `sweptOrFixed`. A preset's Mueller matrix reads no incidence, so the numbers are identical.

## Deferred

Nothing. Every requirement of the step landed in this round.

## Gotchas

- An or-pattern split across two lines inside `Error ( … )` fails with FS0010 / FS0583. Use two match arms that share
  one body.
- Opening `Placement` brings the `CatalogueKind` case `Sample` into scope beside the library's `Sample` record type.
  Patterns resolve to the case and annotations to the type. A test fixture writes `CatalogueKind.Sample` for clarity.
- Per-step binding changes which error a chart shows only when one path has both an unresolvable item and an earlier
  binding error. Each text is still the same `describe`.

## Changelog

- 2026-11-13: step 0368. Moved slot binding for a run (`SetupSlot.runElementWith` / `runStepOf`), the source's
  emission (`runEmissionOf`) and the item resolution (`SlotRunParam.resolveSlotItem`) from the table into Domain
  (S11.2). `chartForRun` and `model.runEmission` call them. Added three constructor-unit facts.
