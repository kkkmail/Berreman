# Step 0234 — state of the world

## Where we are

Spec 0050, section S7.1 ("Items, parameters and item models"), step 0234 (IMPLEMENT), obligation
`S71-SET-PARAMETER-STATE`.

- Steps 214–216 built an item's parameters and their validation, the `Item` record and `AddItem`'s `ItemDraft`.
- This step adds `SetParameterState`'s pure update: one item, one named parameter, its state replaced in place
  (001 §P, §I).
- Step 235 publishes a revision, which is what moves an item's `revision`.

## What's working

- Added `Item.withParameterState name state`. It replaces one named parameter's state in place and keeps the order,
  the other parameters, the revision and the history.
- A name the item lacks is refused with `UnknownItemParameterErr`, naming the parameter and the item's label.
- A state that breaks its rules is refused with the `InvalidParameterStateErr` of `Parameter.validate`.
- Added three `KitItemTests` facts over CPL1 / CPL2 of the circular polarizer model:
  - replace in place, keeping the revision;
  - refusal of an unknown name and of reversed bounds;
  - independent parameters for two items of one model.

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster: build, elevation,
constructor-unit-tests over everything discovered, and the fourteen s23 invariants.

The worker's development runs over the final tree are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0234-*`:

- **build:** 0 errors. The only warnings are the vendored MathNet's two `SYSLIB0051`, the same as step 0233.
- **constructor-unit-tests:** 1356 passed. That is checkpoint 232's 1353 plus 3, the declared facts.
- **KitItemTests:** 19, up from 16, and the TRX holds the three declared ids.
- **elevation** from the arc origin `f395597`: `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects, in 197 s.
- **The fourteen s23 invariant greps:** none finds anything.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1356
```

## Architecture

- **A pure member on the record it updates.**
  - `withParameterState` is an intrinsic member of `Item`, beside `tryFindParameter`, so it is no free function of
    one of our types (`ELEV013`).
  - It writes nothing. The Elmish `SetParameterState` message, and whatever records the item, will call it and
    persist the result.
- **One validation rule.** The member builds the `Parameter` and defers to `Parameter.validate` (step 214). It adds
  no second rule set, and the refusal is that function's error, unchanged.
- **The working revision is edited, not a new one.** `revision` and `history` move only when a revision is
  published (step 235), so repeated edits stay in revision 1. The facts pin that by setting the default back and
  getting the original item.

## Deferred

Nothing in this step's scope is deferred. `Item.withParameterState` and its three facts are complete.

## Gotchas

- **`UnknownItemParameterErr`'s payload is `'<name>' on '<label>'`.** A dotted `<label>.<name>` would be ambiguous
  beside component-dotted names like `c2.retardance`.
- **An unknown name is refused before the state is validated.** An unknown name with a broken state is
  `UnknownItemParameterErr`.
- **The S7.1 facts sit at the end of `KitItemTests`, after the S7.2 section.** They reuse its `modelItem`,
  `circularPolarizerModel` and `heldCpl1` fixtures, and module-level `let`s must precede their use.

## Changelog

- 2026-11-09 — Step 0234:
  - added `Item.withParameterState`, a pure update of one item's named parameter state that keeps the revision;
  - added three `KitItemTests` facts (constructor-unit-tests 1353 → 1356).
