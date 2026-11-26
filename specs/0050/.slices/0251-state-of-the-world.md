# Step 0251 — state of the world

## Where we are

Spec 0050, section S8.1 ("The run law walks every object; the source is real"), step 251 (IMPLEMENT).

- Steps 244–250 built the item-model evaluator's parts:
  - `ParameterValues`;
  - the source arm, `emissionWith`;
  - the bound elements and `muellerAt` for polarizer presets, the empty slot, the parameterised optical forms, stack
    samples and cut plates.
- This step adds the one entry from an item's chosen model to its bound element: `ItemModelChoice.boundWith`.
- It is the door S8.2's slots and S11.2's forward model call. A source still emits through `emissionWith`, and a
  detector's gain, dark offset and noise are S11.2's.

## What's working

- Added `ItemModelChoice.boundWith materials values`, which binds a polarizer preset, a stack sample, an optical form or an empty slot through the existing constructors
- Added `ItemEvaluationError.NotAnOpticalElementErr`, rendered `the model is not an optical element: <reason>`, answered for a source, a detector, or a binding model chosen without its binding
- The `ItemModelForm` match in `boundWith` has no wildcard, so a new model form is a compile error there
- Added two `ItemEvaluationTests` facts and widened the render fact (constructor tests 1397 → 1399)

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster: build, elevation,
constructor-unit-tests over everything discovered, and the fourteen s25 invariants.

The worker's development runs over the final tree, with logs under `specs/0050/.artifacts/0251-*`:

- **build:** exit 0, `0 Warning(s)`, `0 Error(s)`.
  - The round's first build recompiled the vendored MathNet C#, which printed its two pre-existing `SYSLIB0051`
    warnings. Every later build, the final one included, printed none.
- **constructor-unit-tests,** the gate's command: `Passed!`, 1399 succeeded, 0 failed, 0 skipped, in 67 s
  (estimate 180 s).
  - That is checkpoint 250's 1397 plus the two declared facts:
    - `every item model binds as its kind, and a source or a detector is no optical element`
    - `an item's own parameter values reach its Mueller matrix`
  - The widened `every evaluation error renders its condition and its payload` also passes.
  - All three are `Passed` in the TRX under their declared identities.
- **elevation** from the arc origin `f395597`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in 192 s
  (estimate 600 s). Known findings: 0. It was run again over the final tree after a doc-comment edit.
- **The fourteen s25 invariants,** by their gate commands: all hold. The greps with untracked files included find
  nothing.
- **Red, measured by mutation.**
  - The mutation: a source bound as an empty slot, and an optical form bound over its defaults instead of the item's
    values.
  - Both new facts went red (2 of 21).
  - The mutation was reverted (`cmp` identical) and everything above was re-run.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1399
```

## Architecture

- **`boundWith` only routes; it adds no physics.**
  - A polarizer preset choice becomes `BoundPolarizerPreset` of its behaviour.
  - A stack-sample choice goes through `BoundOpticalElement.ofStackSample` (step 249).
  - A standalone optical form goes through `BoundOpticalElement.ofOpticalForm` (step 248).
  - An empty slot becomes `BoundEmptySlot`.
  - So every Mueller matrix still comes from `muellerAt`, and every emission from `emissionWith`.
- **It is a second `type ItemModelChoice with` block,** after `BoundOpticalElement`. The first block, holding
  `emissionWith`, precedes the type it would have to name.
- **Every refusal is one typed case, `NotAnOpticalElementErr`.** It covers a source, a detector, and a binding form
  chosen without its binding. Its reason is `'<model name>' (<model id>) is <kind>`, as `emissionWith`'s is, and the
  kind says why: "which emits", "which reads", or "… bound to no source preset / polarizer preset / sample".
- **A bound choice's model form is not re-checked.** A mis-bound choice is refused earlier, when the item is drafted
  (`ItemDraft.addTo`, step 216).

## Deferred

Nothing. Every requirement of the step landed in this round. The slots that call `boundWith` (S8.2) and the
detector's forward model (S11.2) are scheduled steps, not deferrals from this one.

## Gotchas

- **The slice asks for "the typed missing-binding error" but names no such case.**
  - The step may create only `boundWith` and `NotAnOpticalElementErr`, and "No other member is added".
  - So a `LibraryModelChoice` of a binding form answers `NotAnOpticalElementErr`, its reason naming the binding the
    model lacks. That follows step 246's `emissionWith`, which answers `NotALightSourceErr` "… bound to no source
    preset" for the same choice.
- **A bound stack sample holds its resolved materials with reference equality.** Two bindings of one sample are never
  `=`, so tests compare what they evaluate to, and their typed refusals.
- **A first build after MathNet's obj goes stale recompiles the vendored C#** and prints its two pre-existing
  `SYSLIB0051` warnings. These are not ours to fix in this step. A second build is clean.

## Changelog

- 2026-11-10 — Step 0251: added `ItemModelChoice.boundWith` (choice × values → bound element) and
  `ItemEvaluationError.NotAnOpticalElementErr`; two new `ItemEvaluationTests` facts and a widened render fact
  (1397 → 1399).
