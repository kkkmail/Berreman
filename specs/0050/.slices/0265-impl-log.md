# Step 0265 — implementation log

## Progress

- Read the task, the ASG system prompts, the project prompt, the slice, and the 0252 / 0263 / 0264 states of the world.
- Wrote the impl-plan.
- `TableAndElementRotationView.fs`:
  - added `model.resolveSlotItem` (internal, beside `tryItemModelChoice`); `slotAssigned` now calls it;
  - `objectIds` counts a `Sample` slot holding an item;
  - `chartForRun` resolves the captured paths' slot items into `slotItems`, and `builderFor` evaluates a slot's item
    through `boundWith` / `muellerAt` via a local `sweptOrFixed`, which step 252's sample arm now shares;
  - `noPolarizerOnPath` and `variedIsBound` read a slot's item;
  - one local `cannotEvaluate` builds the failure chart.
- The first Ui build failed (FS0491): a `member private` on the model is not reachable from a module function. Made it
  `member internal`, as `objectIds` is. The rebuild had 0 warnings and 0 errors.
- `ExperimentControlsTests.fs`: the three facts and their fixtures. Opening `ItemModels`, `Kit` and `ItemEvaluation`
  was needed: `ItemEvaluation.ParameterValues.ofParameters` does not compile qualified (FS0039), because the union
  case shadows the type, as step 263 found for `ParameterName`.
- Solution build: 0 warnings, 0 errors. The three new facts pass (3 of 3).
- **Red check.** HEAD's view file was copied over the new one (a file copy, with no git mutation), then the solution
  was rebuilt and the three facts were run against it:
  - `a slot's item … leakage passes crossed light` failed (x = 45: expected 0.505, got 0.5, the preset's Malus);
  - `a sample item is solved at its own thickness …` failed (x = 0: the 1 mm default solve, not the item's 2 mm);
  - the dial fact passed. Steps 258/260 already draw a dial item's R1, and a preset item's slot is bound to its preset,
    so this fact guards the drawn angle rather than new code.
  - The new file was restored and checked against its SHA-256.
- Extended the first new fact with the failure path the brief names. A slot's item that the kit no longer holds
  (`{ held with kitItems = [] }`) shows `The run cannot be evaluated` with the `describe` of `UnknownSlotItemErr`
  naming the item. Rebuilt with 0 warnings; the three facts pass.
- Ran each gate's own command over the final tree (Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`

## Decisions

- **The source slot's item is not a step.** The source is the first step of every `BeamLayout` path, and
  `boundWith` refuses a source choice with `NotAnOpticalElementErr`. If the source's item were built as an
  `ElementStep`, every source item (step 266) would show the failure chart. So `slotItems` never holds a
  `LightSource` slot, and the source keeps step 252's "no builder". This follows the S8.1 mapper's request ("Prep and
  Analyzer slots become `ElementStep`s and Object slots `ObjectStep`s"). Step 266 gives the source item its emission.
  A detector is never on a path, because the walk does not append it.
- **Items are resolved before any point is read.** `chartForRun`'s evaluation pipeline carries
  `ItemEvaluationError`, and `ItemEvaluation.sequenced` is fixed to that error type, while an unresolved slot item is a
  `SetupError` (`UnknownSlotItemErr`). So `chartForRun` first resolves every item held by a slot on a captured path,
  once, into `slotItems : Result<Map<ElementId, Item * ItemModelChoice>, SetupError>`. An error shows the
  `The run cannot be evaluated` chart with its `describe`. `Ok` feeds `builderFor`. This adds no new error union and no
  second sequencing combinator. An unresolved item therefore takes precedence over an evaluator error.
- **One resolution, three callers.** `slotAssigned`'s "kit item, then `tryItemModelChoice`" lookup, with its two
  texts, moved unchanged into `model.resolveSlotItem`. `AssignSlot`, `SwapSlot` and the run call it ("Reuse, never
  copy"). It is `internal`, as `objectIds` is: an extension's `private` member is not reachable from module functions.
- **The item arm precedes the binding.** `withItem` binds a preset or sample item's slot to its entry, and a library
  model item keeps the slot's pre-binding (the real LP stays bound to `polarizerLp`). So `builderFor` matches the
  slot's item first, and the step-252 arms run only for `None`.
- **One sweep shape.** The item arm evaluates exactly as step 252's sample arm does: drawn R1 and incidence, R1 swept
  when varied under VaryR1, incidence swept when varied under VaryR2, and the point's wavelength under
  VaryWaveLength. That body is now the local `sweptOrFixed`, which both arms call; the sample arm's evaluation is
  unchanged. Step 252's preset arm (normal incidence, w0) is left exactly as it was, as the brief requires for an empty
  slot.
- **One failure chart.** The local `cannotEvaluate` builds the `The run cannot be evaluated` chart for both the slot-item
  error and the evaluator errors. `SampleMaterialErr` keeps its own title.
- **`objectIds`** answers a `Sample` slot holding an item before it reads the binding, so a library-model item in a
  sample slot (a retarder, a black box) is an object. The bound-sample rule is unchanged.
- **The dial fact's reference** is a second fresh `initMain ()` table: two polarizers added, the second turned to R1 40°,
  bare (`EmptySlotContent`, bound to `polarizerLp`). Its chart's title and every point equal the dial table's within
  1e-12.
- **The sample fact also asserts that the 1 mm default reads otherwise** at some incidence. At 0° the 1 mm solve differs by
  about 1e-11, against HEAD's 0.918315009373143 and the item's 0.9183150093826846. This makes "at its own thickness" observable, and the fact is red against HEAD.

## Testing state

Development runs of each gate's own command over the final tree. No verdict is claimed (Invariant 6); the supervisor
runs the frozen roster.

- build: exit 0, 0 warnings, 0 errors (deadline 600 s).
- elevation: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 201 s (deadline 600 s). The product
  file has not changed since; the last edit touched only the test file, which elevation does not analyze.
- constructor-unit-tests: `Passed!`, 1429 succeeded (unchanged), 0 failed, 0 skipped, 74 s (deadline 180 s).
- ui-smoke: `Passed!`, 245 succeeded (unchanged), 0 failed, 0 skipped, 203 s (deadline 600 s).
- ui-tests over the six selected classes (the gate's filters, `ui-smoke` and `kit-acceptance` excluded): `Passed!`, 89
  succeeded, 0 failed, 0 skipped, 107 s (deadline 360 s). `ExperimentControlsTests` holds 49 facts, against 46 at HEAD
  (+3). The three new identities are `Passed` in `TestResults/ui-tests.trx`.
- The fifteen s26 invariant greps each find nothing.
- `git diff --numstat` equals `--ignore-cr-at-eol` for both edited files: no CRLF churn.
- No run exceeded its deadline.

Every requirement of the slice is addressed; nothing is deferred.

## Artifacts

- `specs/0050/.artifacts/0265-dev-build.log`: the final solution build.
- `specs/0050/.artifacts/0265-dev-ui-build.log`: the round's Ui-only build.
- `specs/0050/.artifacts/0265-dev-new-facts.log`: the three new facts alone, 3 of 3.
- `specs/0050/.artifacts/0265-red-check-build.log`, `0265-red-check-tests.log`: the red check against HEAD's view file.
- `specs/0050/.artifacts/0265-red-check-new-view.sha256`: the restored file's hash.
- `specs/0050/.artifacts/0265-dev-elevation.log`: the elevation run.
- `specs/0050/.artifacts/0265-dev-constructor-unit-tests.log`: the whole constructor suite.
- `specs/0050/.artifacts/0265-dev-ui-smoke.log`: the ui-smoke suite.
- `specs/0050/.artifacts/0265-dev-ui-tests.log`: the six selected UI classes.
- `specs/0050/.artifacts/0265-dev-invariants.log`: the fifteen invariant greps.
