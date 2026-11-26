# Step 0208 — state of the world (IMPLEMENT: the parameterised item models as data)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), closing on its S7 prerequisite
(001 §H, §I, §S6): the shared item contracts land before S7 so that copying an experiment can map its items.

- Steps 205–207 gave `ItemModels.fs` the parameter vocabulary and made the existing models publish their axes.
- This step adds the parameterised item models of 001 §I as data: the forms an item-model library entry holds,
  and the parameters each form publishes.
- Step 209 adds the item-model store seam over `ItemModelForm`. Step 210 makes it a library kind and ships the
  built-ins. Step 216 instantiates items from a resolved choice.

## What's working

- Add the item-model forms: `PolarizerLeakage`, `RetardanceDispersion`, `BlackBoxNormalisation`, `OpticalForm`
  with `CompoundComponent`, and `ItemModelForm` (the 001 §I table).
- `OpticalForm.parameterAxes` and `ItemModelForm.parameterAxes` publish each form's names, kinds and defaults:
  - the ideal LP / quarter-wave / identity defaults;
  - a compound's components under `c<k>.` prefixes;
  - a detector's gain and dark offset, then its noise axes unchanged.
- Add the typed binding of the source, ideal-polarizer and stack forms: `ItemModelBindingKind`, `ItemModelBinding`,
  `ItemModelForm.bindingKindOpt` and `tryBoundParameterAxes`. The bound axes come from step 207's members.
- Add five `ItemModelsTests` facts (constructor unit tests 1270 → 1275).

## Tests

These are development runs, recorded in `0208-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors.
- **constructor-unit-tests:** 1275 passed, 0 failed, 0 skipped (1270 → 1275, +5). The five new identities are
  `Passed` in the TRX.
- **elevation** (from the arc origin): `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects (ELEV004 excluded).
- **The fourteen `invariant-s22-*` greps:** each finds nothing.

```yaml
gates:
  constructor_unit_tests: 1275
```

## Architecture

- **The item models are pure data with generated axes** (spec 0050 S7.1, 001 §I). The fitter's axes are
  generated from a form, never hand-written. No evaluator is added: turning a form into a Mueller matrix or a
  reading is the run law's (001 §N, S8.1).
- **A compound is recursive.** `OpticalForm` and `CompoundComponent` are one recursive group. A compound's axes
  are its components' axes, prefixed by their 1-based light-order position, so the prefixes nest
  (`c2.c1.retardance`).
- **The binding forms stay nullary,** because they are serialized payloads.
  - The form declares the binding it needs (`bindingKindOpt`).
  - The resolved preset or sample travels as an `ItemModelBinding`, whose axes are step 207's members.
  - `tryBoundParameterAxes` joins the two and refuses a mismatched binding with `None`.
  - Unbound, a binding form publishes nothing.

## Deferred

Nothing in this step's scope.

- By the spec's plan: the store seam is step 209; the library kind, schema and shipped built-ins are step 210;
  instantiating an item from a resolved choice is step 216.

## Gotchas

- **`ItemModels.fs` now opens `System`, `Berreman.Geometry` and `Berreman.Fields`.** `Angle` lives in Geometry,
  although the brief names only Fields.
- **A binding form's `parameterAxes` is `[]`.** Use `tryBoundParameterAxes` with the resolved binding for an
  item's axes.
- **Axis names are identities** that the item's parameter states and fit reports key on. Reordering a compound's
  components renames its parameters.
- **The black box's scale case replaces the row-major head `m00`.** The element list's order is load-bearing.

## Changelog

- 2026-11-09 — step 0208: added the item-model forms to `ItemModels.fs`, with their `parameterAxes` and the typed
  binding of the source, ideal-polarizer and stack forms, plus five new facts.
