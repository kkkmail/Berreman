# Step 0245 — state of the world

## Where we are

Spec 0050, section S8.1 ("The run law walks every object; the source is real"), step 0245 (IMPLEMENT), obligation
`S81-PARAMETER-VALUES`.

- Step 244 wrote every published parameter name once, as a `ParameterName` member.
- This step opens the home of the one item-model evaluator (001 §N): `OpticalConstructor.Domain/ItemEvaluation.fs`,
  compiled after `MuellerInverse.fs`. It gives the module the values an evaluation reads.
  - `ParameterValues` gets its numbers from an item's parameter states or from a model's axis defaults.
  - It checks them against the model's axes and their kinds' ranges, refusing with a typed `ItemEvaluationError`.
- Later steps of the section add the evaluation itself, and its own error cases, to this module.

## What's working

- Add `ItemEvaluation.fs`, the item-model evaluator's home, compiled last in the Domain.
- Add `ItemEvaluationError` (`MissingParameterValueErr`, `ParameterOutOfRangeErr`) with its `describe` renderings.
- Add `ParameterValues`:
  - `ofParameters` reads a known value, an unknown's start or a calibrated value;
  - `defaultsOf` reads a model's axis defaults;
  - `withValue` replaces or adds one number, and `valueOf` reads one.
- Add `checkedFor`. It refuses a missing number, or one outside its kind's range, with the first failure in axis order.
- Add `ItemEvaluationTests`, with four facts.

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster:

- build and elevation;
- constructor-unit-tests over everything discovered;
- the fourteen s25 invariants.

The worker's development runs over the final tree, with logs under `specs/0050/.artifacts/0245-*`:

- **build:** exit 0, `0 Warning(s)`, `0 Error(s)`.
- **constructor-unit-tests:** `Passed!`, 1382 succeeded, 0 failed, 0 skipped, in 67 s (estimate 180 s).
  - That is checkpoint 244's 1378 plus the four new facts:
    - `parameter values are read from an item's parameter states`
    - `defaults come from a model's axes and one value can be replaced`
    - `values are checked against a model's axes and their kinds' ranges`
    - `every evaluation error renders its condition and its payload`
  - Each is `Passed` in the TRX under its declared identity.
- **elevation** from the arc origin `f395597`: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in 194 s.
  Known findings: 0.
- **The fourteen s25 invariant greps,** untracked files included: none finds anything.
- **Red before green:** the test project failed to compile before the production module (`FS0039` for
  `ParameterValues`, `ItemEvaluationError` and both cases).

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1382
```

## Architecture

- **One map of numbers by published name.** `ParameterValues` wraps `Map<ParameterName, ParameterValue>`. The
  evaluator reads each number under the very `ParameterName` a model publishes (step 244).
- **A kind's range is checked here, against the model's axes** (step 214: a kind's range is its model's).
  - `Parameter.validate` in `Kit.fs` still checks only finiteness, bounds and sigmas.
  - The ranges:
    - fraction: [0, 1];
    - scale and thickness: at least 0;
    - angle, phase, Mueller element and reading: any finite number.
  - NaN and ±∞ fail every kind.
- **The refusal's reasons:**
  - out of range: `<name> = <value> lies outside <range>`, the value round-trip in the invariant culture;
  - missing: the parameter's name alone.
- **Values no axis names are kept and ignored,** so one `ParameterValues` can serve a compound's several models.

## Deferred

Nothing in this step's scope is deferred. The evaluation itself (model × parameters × orientation × λ → Mueller matrix)
belongs to the section's later steps, as the brief states.

## Gotchas

- **`Map` equality on a `ParameterValues` holding a NaN is false.** F# compares the values with PER semantics. Do not
  assert `Ok values` for values that hold a NaN.
- **The invariant-culture `"R"` rendering of +∞ is `Infinity`, not `∞`.** A reason that names an infinite value reads
  `intensityScale = Infinity lies outside [0, ∞)`.
- **`ParameterValues` has no `.value` accessor,** by the brief ("no other member is added"). A later step that needs
  the whole map adds its own member with its reason.

## Changelog

- 2026-11-10 — Step 0245:
  - add `ItemEvaluation.fs` with `ItemEvaluationError` and `ParameterValues` (`ofParameters`, `defaultsOf`,
    `withValue`, `valueOf`, `checkedFor`);
  - add four constructor-unit-tests facts in `ItemEvaluationTests`.
