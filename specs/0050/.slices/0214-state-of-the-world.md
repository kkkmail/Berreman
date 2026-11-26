# Step 0214 — state of the world (IMPLEMENT: a parameter and its state)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), is landing the shared item
contracts. They must exist before S7, so that copying an experiment can map its items (001 §H, §S6; S7.1;
001 §I).

- Step 213 added an item's identity and the model version it instantiates.
- This step adds a parameter and its state to the `Kit` module, after `ItemModelInstance`. Its name reuses
  `ParameterName`, and it is known, unknown (start, bounds, optional Gaussian prior) or calibrated (value, sigma,
  fit run).
- Step 215 adds the `Item` record, and step 216 adds `AddItem`, which makes an item's parameters through
  `Parameter.knownAtDefault`.

## What's working

- Add `Kit.ParameterInterval`, `ParameterSigma`, `ParameterPrior`, `UnknownParameterData`, `FitRunName` and
  `CalibratedParameterData`. Same-typed pairs sit in records, never in case tuples.
- Add `Kit.ParameterState` (`KnownParameter` | `UnknownParameter` | `CalibratedParameter`) and
  `Parameter = { name : ParameterName; state : ParameterState }`.
- `Parameter.knownAtDefault` makes a known parameter at a model axis's name and default.
- `Parameter.validate` refuses a non-finite number, a search interval that is empty or reversed, a start outside
  it (ends included) and a sigma that is not above zero. It answers `InvalidParameterStateErr` naming the
  parameter and the broken rule.
- Add two `KitItemTests` facts. Constructor unit tests: 1292 → 1294.

## Tests

These are development runs, recorded in `0214-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 errors; the final build had 0 warnings.
- **constructor-unit-tests:** 1294 passed, 0 failed, 0 skipped (+2), in 64 s against a 180 s estimate. Both
  declared identities pass:
  - `a parameter is created known at its axis default`;
  - `a parameter whose state keeps its rules is kept, and one that breaks them is InvalidParameterStateErr`.
- **The facts bite.** Three mutations each fail their fact: equal bounds allowed, a start on the lower bound
  refused, and `knownAtDefault` ignoring the axis default. The source was then restored.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded): `ELEV-GATE: OK - 0 new finding(s) in 12
  project(s)`, 193 s against a 600 s deadline.
- **The fourteen `invariant-s22-*` gates:** each holds.
- The UI suites are not in this step's roster. No UI file or shared fixture changed.

```yaml
gates:
  constructor_unit_tests: 1294
```

## Architecture

- **A parameter's state is pure Domain data in `Kit`, appended in the brief's order after `ItemModelInstance`.**
  Every number is an `ItemModels.ParameterValue`, and the name is `ItemModels.ParameterName`.
- **`validate` checks structure only.** The structure is:
  - finite numbers;
  - an ordered interval holding the start;
  - a positive sigma.

  A kind's range belongs to its model (S8.1) and is not checked here.
- **`validate` reports the first broken rule, finiteness first.** So a NaN is named as itself, never as a
  misplaced bound. Numbers render with the invariant culture's round-trip format.

## Deferred

Nothing in this step's scope. By the spec's plan:

- the `Item` record is step 215;
- `AddItem`, the first caller of `Parameter.knownAtDefault`, is step 216.

Neither is deferred by this step.

## Gotchas

- **Record labels repeat across types.** `sigma` is a field of both `ParameterPrior` and
  `CalibratedParameterData`, and `name` of both `Parameter` and `ItemParameterAxis` / `KitFile`. Annotate record
  values with their type.
- **The vendored MathNet `Numerics` project prints two `SYSLIB0051` warnings whenever it recompiles.** This is
  pre-existing, and not from this step's files. An incremental build that skips it shows 0 warnings.
- **`ParameterValue -0.5` must be parenthesised:** `ParameterValue (-0.5)`.
- **In this Git Bash, count CR bytes with `tr -cd '\r' | wc -c`.** `grep -c $'\r'` falsely matches every line.

## Changelog

- 2026-11-09 — step 0214: added `Kit.ParameterInterval`, `ParameterSigma`, `ParameterPrior`,
  `UnknownParameterData`, `FitRunName`, `CalibratedParameterData`, `ParameterState` and `Parameter` (with
  `knownAtDefault` and `validate`); added two `KitItemTests` facts; constructor unit tests 1292 → 1294.
