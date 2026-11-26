# Step 0265 — state of the world

## Where we are

Spec 0050, section S8.2 (setups as slots), step 0265 (IMPLEMENT).

- Step 0252 made `chartForRun` fold every polarizer and sample on a detector's path through `ItemEvaluation`.
- Steps 0256–0264 made the table a setup whose slots hold kit items (`AssignSlot`, `SwapSlot`, dial readings drawn
  through the item's zero).
- This step makes the run law read those slots. A slot holding an item is evaluated through the item's own model and
  parameters, at its drawn orientation: as an object in a sample slot, as an element otherwise.
- Step 0266 gives the source slot's item its emission.

## What's working

- Ran a slot's item through its own model and parameters (`boundWith` over `ParameterValues.ofParameters`, `muellerAt`
  at its drawn R1 and incidence, the varied axis swept): an `ObjectStep` in a sample slot, an `ElementStep` otherwise
- Counted a sample slot holding an item as an object, a polarizer slot holding an item as a polarizer, and a varied
  slot holding an item as bound
- Showed `The run cannot be evaluated` with `UnknownSlotItemErr`'s text for a slot item the kit or stores lack
- Shared one slot-item resolution (`resolveSlotItem`) between `AssignSlot`, `SwapSlot` and the run
- Added three `ExperimentControlsTests` facts: a real polarizer item's leakage, a sample item's own thickness, a
  dial-mounted item's drawn angle

## Tests

No gate verdict is claimed (Invariant 6); the supervisor runs the frozen roster. The development runs below used each
gate's own command over the final tree, with logs in `specs/0050/.artifacts/0265-dev-*.log`.

- **build:** exit 0, 0 warnings, 0 errors (deadline 600 s). The first Ui-only build of the round recompiled the
  vendored MathNet and showed its two existing `SYSLIB0051` warnings (`MathNetNumerics/Numerics/Exceptions.cs`,
  untouched); every later build showed none.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 201 s (deadline 600 s).
- **constructor-unit-tests:** `Passed!`, 1429 succeeded, 0 failed, 0 skipped, 74 s (deadline 180 s). Unchanged, as no
  constructor test changed.
- **ui-smoke:** `Passed!`, 245 succeeded, 0 failed, 0 skipped, 203 s (deadline 600 s). Count unchanged.
- **ui-tests (six selected classes):** `Passed!`, 89 succeeded, 0 failed, 0 skipped, 107 s (deadline 360 s).
  `ExperimentControlsTests` holds 49 facts, against 46 at HEAD, so the count rises by 3. The three new identities are
  `Passed` in `TestResults/ui-tests.trx` under the census's exact names.
- **Red, measured against HEAD's view file** (`0265-red-check-tests.log`). HEAD's file was copied in, the solution
  rebuilt, then the new file restored and checked against its SHA-256.
  - The leakage fact failed: x = 45 read 0.5, the preset's Malus, not 0.505.
  - The thickness fact failed: x = 0 read the 1 mm default solve.
  - The dial fact passed: the dial angle was already drawn by steps 258/260.
- **The fifteen s26 invariants** each find nothing (`0265-dev-invariants.log`).
- `git diff --numstat` equals `--ignore-cr-at-eol` for both edited files: no CRLF churn.
- No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1429
  ui_smoke_tests: 245
  ui_tests: 89
```

## Architecture

- **Items are resolved before any point is read.** `chartForRun` resolves every item held by a slot on a captured
  path, once, into `slotItems : Result<Map<ElementId, Item * ItemModelChoice>, SetupError>`.
  - It never holds the source's item, which emits rather than acts on the light (step 266).
  - An error is the failure chart. `Ok` feeds `builderFor`, which tries a slot's item before step 252's preset and
    sample arms.
  - The evaluation pipeline stays on `ItemEvaluationError` and `ItemEvaluation.sequenced`.
- **One sweep shape.** The local `sweptOrFixed` holds both the item arm's and step 252's sample arm's evaluation: drawn
  R1 and incidence, swept when varied, and the point's wavelength. Step 252's preset arm is unchanged.
- **One resolution of a slot's item.** `model.resolveSlotItem` (internal) holds the kit-item lookup, the model
  resolution and both `UnknownSlotItemErr` texts. `slotAssigned` and `chartForRun` call it.
- **With no item in any slot, `slotItems` is empty,** and every chart is step 252's. `FsxAcceptanceTests` and the chart
  classes pass unchanged.

## Deferred

Nothing in this step's scope. The source slot's emission is step 0266's, as its brief states.

## Gotchas

- **The source is the first step of every path.** `BeamLayout.layout` starts each walk with the root's own step. A
  source item must therefore never be built as an element step: `boundWith` refuses a source choice with
  `NotAnOpticalElementErr`.
- **A library-model item keeps the slot's pre-binding.** A real LP item in an added linear polarizer stays bound to
  `polarizerLp`. A run that read `boundOf` first would evaluate the ideal preset. Read the slot's item first.
- **`member private` on a type extension is unreachable from module functions** (FS0491). Use `internal`.
- **`ItemEvaluation.ParameterValues.ofParameters` does not compile qualified** (FS0039): the union case shadows the
  type, as with `ParameterName`. Open `OpticalConstructor.Domain.ItemEvaluation`.
- **The 1 mm and 2 mm glass plates differ only slightly** at 600 nm. At 0° the difference is about 1e-11, so a
  thickness assertion needs a 1e-12 tolerance, not a looser one.

## Changelog

- 2026-11-10: Step 0265. The run law reads the slots' items: a slot holding an item is evaluated through its own model
  and parameters at its drawn orientation, as an object in a sample slot, and the source's item is left to step 266.
  `objectIds` counts sample items, and the no-analyzer and varied-bound checks read a slot's item. One
  `resolveSlotItem` is shared with `AssignSlot` and `SwapSlot`. Three `ExperimentControlsTests` facts were added.
  Constructor 1429, smoke 245, selected UI 89 (+3).
