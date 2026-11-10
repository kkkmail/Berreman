# Step 0251 — impl log (IMPLEMENT: one entry from an item's model to its bound element)

## Progress

- [x] `ItemEvaluation.fs`: `NotAnOpticalElementErr`, `ItemModelChoice.boundWith`.
- [x] `ItemEvaluationTests.fs`: two new facts, widened error-rendering fact.
- [x] Development runs: build, constructor tests (all discovered), elevation, invariant greps, red by mutation.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemEvaluation.fs`
  - `ItemEvaluationError.NotAnOpticalElementErr of reason : string`, beside `NotALightSourceErr`, rendered
    `the model is not an optical element: <reason>`; the type's doc names the new condition.
  - A second `type ItemModelChoice with` block, after `BoundOpticalElement` (the first block, with `emissionWith`,
    precedes it), holding `member this.boundWith (materials : MaterialProxy) (values : ParameterValues) :
    Result<BoundOpticalElement, ItemEvaluationError>`:
    - `SourcePresetChoice (model, _)` → `NotAnOpticalElementErr "'<name>' (<id>) is a light source, which emits"`;
    - `PolarizerPresetChoice (_, polarizer)` → `Ok (BoundPolarizerPreset polarizer.behavior)`;
    - `StackSampleChoice (_, _, sample)` → `BoundOpticalElement.ofStackSample materials values sample` (step 249);
    - `LibraryModelChoice model`, matched on `model.entry.model` with no wildcard:
      - `OpticalElementModel form` → `BoundOpticalElement.ofOpticalForm values form` (step 248);
      - `EmptySlotModel` → `Ok BoundEmptySlot`;
      - `IntensityDetectorModel` / `MuellerPolarimeterModel` / `EllipsometerModel` → `NotAnOpticalElementErr`
        "… is an intensity detector / a Mueller polarimeter / an ellipsometer, which reads";
      - `SourcePresetItemModel` / `IdealPolarizerItemModel` / `StackSampleItemModel` → `NotAnOpticalElementErr`
        "… is a light-source model bound to no source preset / an ideal-polarizer model bound to no polarizer
        preset / a stack-sample model bound to no sample".
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemEvaluationTests.fs`
  - new: `every item model binds as its kind, and a source or a detector is no optical element`;
  - new: `an item's own parameter values reach its Mueller matrix`;
  - widened: `every evaluation error renders its condition and its payload` covers `NotAnOpticalElementErr`
    (distinct renders 7 → 8).

## Decisions

- **"The typed missing-binding error" is `NotAnOpticalElementErr` whose reason names the binding the model lacks.**
  - The slice requires a `LibraryModelChoice` of `SourcePresetItemModel`, `IdealPolarizerItemModel` or
    `StackSampleItemModel` to answer "the typed missing-binding error". It names no such case.
  - The step's authorized contracts create exactly `boundWith` and `NotAnOpticalElementErr`, and "No other member is
    added", so a separate `MissingBindingErr` case would be an unauthorized contract.
  - The precedent is step 246's `emissionWith`: the same unbound source form answers `NotALightSourceErr` "… is a
    light-source model bound to no source preset". `boundWith` follows it: the error is typed (an
    `ItemEvaluationError` case) and its payload names the model and the binding it lacks. An unbound binding form
    has no physics to bind, so "not an optical element" is true of the choice as given.
- **A bound choice's model form is not re-checked.** `PolarizerPresetChoice` binds its preset's behaviour and
  `StackSampleChoice` its sample, as the slice writes them; a mis-bound choice is refused earlier, by
  `ItemDraft.addTo` (step 216). The source, polarizer and sample arms call the existing evaluators
  (`emissionWith` stays the source's door; `ofStackSample`, `BoundPolarizerPreset`), so no physics is duplicated.
- **The refusal reason `'<name>' (<id>) is <kind>`** matches `emissionWith`'s. Each member keeps its own one-line
  closure because each wraps a different error case; sharing it would need a new member or module function, which the
  slice forbids ("No other member is added").
- **The detector and source kinds say why** ("which reads", "which emits"), so the log line explains the refusal.
- **Test fixtures** come from step 210's shipped entries (`BuiltinCatalogue.itemModel` of the ideal-polarizer,
  stack-sample, light-source, circular-polarizer, empty-slot and three detector models) and from Built-in bindings
  (`BuiltinCatalogue.preset PresetIds.polarizerLp`, `BuiltinCatalogue.source PresetIds.heNe633`,
  `BuiltinCatalogue.sample SampleIds.glassPlate1mmId` at `SampleVersionId.firstOf`). No model is manufactured and no
  preset or sample is used as a model.
- **The stack-sample binding is compared by what it evaluates to.** `BoundStackSample` holds `[<ReferenceEquality>]`
  resolved materials, so two bindings of one sample are never `=`. The fact compares the Mueller matrices exactly on
  both branches (20° spin, 30° incidence, 600 nm) against `ofStackSample`, and checks that the item's values reach
  the sample: a negative plate thickness is refused exactly as `ofStackSample` refuses it.
- **Within the declared first fact, beyond the slice's minimum,** the three unbound binding forms are checked to
  answer the missing-binding refusal, and the circular polarizer is also checked to be `BoundOpticalForm (form,
  defaults)`. No new test identity is added.
- **CPL1 is made through `ItemDraft.addTo []`** (step 216) over `FixedMounting`, so it holds exactly the model's
  axes and no dial zero; `c2.retardance` is set by `withParameterState` (step 234) and read back through
  `ParameterValues.ofParameters item.parameters`.

## Testing state

These are development runs. Invariant 6: the arc-runner's gate engine gives the verdict, not these runs.

- **build (first):** exit 0, `0 Error(s)`, `2 Warning(s)` (`.artifacts/0251-build.log`).
  - Both warnings are the vendored MathNet C#'s pre-existing `SYSLIB0051` in `MathNetNumerics/Numerics/Exceptions.cs`.
    They appear only when that project recompiles; this first build recompiled it. Steps 0001–0006 recorded the same.
  - Not this step's code, and outside its `touches`. Every later build was `0 Warning(s)`.
- **constructor-unit-tests (first, the gate's command):** `Passed!`, 1399 succeeded, 0 failed, 0 skipped, 1 m 08 s
  (`.artifacts/0251-constructor-unit-tests.log` / `.trx`).
- **elevation (first)** with `ASG_ARC_BASE_COMMIT=f395597c…`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`,
  194 s (`.artifacts/0251-elevation.log`).
- **Doc-comment edit after the first runs:** the `ItemEvaluationError` doc had grown to four lines; it was rewritten
  to three. Everything below runs over the final tree.
- **Red by mutation** (`.artifacts/0251-red-mutation-build.log`, `.artifacts/0251-red-mutation-tests.log`):
  - Mutation: `SourcePresetChoice` bound as `Ok BoundEmptySlot`, and `OpticalElementModel form` bound over its
    defaults instead of the item's values.
  - Build 0 warnings. `ItemEvaluationTests`: 21 total, 2 failed, 19 succeeded. The two that failed:
    - `every item model binds as its kind, …`: `Assert.Equal() Failure: Values differ`;
    - `an item's own parameter values reach its Mueller matrix`: `element (1,0): expected 0.3535533905932738, got
      3.061616997868383E-17`.
  - The production file was restored from a copy taken before the mutation; `cmp` reported it identical.
- **build (final):** exit 0, `0 Warning(s)`, `0 Error(s)`, no `error` in the log (`.artifacts/0251-build-final.log`).
- **constructor-unit-tests (final, the gate's command):** `Passed!`, total 1399, succeeded 1399, failed 0, skipped 0,
  1 m 07 s (estimate 180 s) (`.artifacts/0251-constructor-unit-tests-final.log` / `.trx`).
  - 1399 is checkpoint 250's 1397 plus the two declared facts.
  - The two new facts and the widened `every evaluation error renders its condition and its payload` are `Passed` in
    the TRX under their declared identities.
- **elevation (final):** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 192s`; known findings 0, suppressed 1350
  (the excluded ELEV004) (`.artifacts/0251-elevation-final.log`).
- **The fourteen s25 invariant greps** (final tree, `--untracked`): all 14 find nothing (`.artifacts/0251-invariants.log`).
- **LF:** `git diff --numstat` equals `--ignore-cr-at-eol` (30/3 and 111/1); no CR in either edited file.

No test is removed or renamed, and no assertion is removed or weakened. The render fact's distinct count moves
7 → 8 because it gains a case.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0251-build.log`, `0251-build-final.log`
- `0251-constructor-unit-tests.log` / `.trx`, `0251-constructor-unit-tests-final.log` / `.trx`
- `0251-elevation.log`, `0251-elevation-final.log`
- `0251-invariants.log`
- `0251-red-mutation-build.log`, `0251-red-mutation-tests.log`

## Gotchas

- **"The typed missing-binding error" names no case.** It is read as `NotAnOpticalElementErr` with a reason naming
  the binding the model lacks (see Decisions). If a later step wants a dedicated case, it must declare that contract.
- **`boundWith` lives in a second `type ItemModelChoice with` block.** The first block (`emissionWith`) is declared
  before `BoundOpticalElement`, which F#'s declaration order forbids it from naming.
- **The bound stack sample has reference equality.** Compare what it evaluates to, never the `Result` of two bindings.
- **A first build after MathNet's obj is stale recompiles it** and prints its two pre-existing `SYSLIB0051` warnings.
  A second build is clean.
