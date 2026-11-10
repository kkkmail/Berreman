# Step 0259 — state of the world

## Where we are

Spec 0050 section S8.2 (setups as slots), step 0259 (IMPLEMENT). Step 0256 made `Setups.SetupSlot` the Domain shape
of a table element plus its content, and 0257/0258 added slot roles and the pure assign/swap halves. This step
collapses the bijection: the table's elements ARE slots, so the Ui's `TestElement` is deleted and the Main scene's
model holds `Setups.SetupSlot list`, every placed slot empty. Nothing drawn, laid out or run changes. Step 0260
deletes the scene capture/restore bridge, and step 0263 puts items into slots.

## What's working

- Deleted `TestElement`; `TableAndElementRotationModel.elements` is now `Setups.SetupSlot list`.
- Moved `elementX`, `boxCorners` and the drawing members onto `type Setups.SetupSlot with` extensions in the Ui.
- Seeds, `AddElement` / `AddElementBoundTo`, `restoreScene` and the TestWindows' `onRay` build slots with `EmptySlotContent`.
- `layoutElements` is the slots' own `layoutElement`.
- Added `MainSceneMsgTests` "the table's elements are slots: each one placed is empty, and each lays out as the
  table lays it out"; `TestElement` is gone from `Berreman/`.

## Tests

No gate verdict is claimed (Invariant 6); the supervisor runs the frozen roster. Development runs over the final tree
used each gate's own command; logs are `specs/0050/.artifacts/0259-dev-*.log`.

- **build:** exit 0, 0 errors, 71 s (deadline 600 s). Only the two pre-existing vendored MathNet `SYSLIB0051` warnings.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 198 s (deadline 600 s), known findings 0.
- **constructor-unit-tests:** `Passed!`, 1421 succeeded, 0 failed, 0 skipped, 74 s (deadline 180 s); count unchanged.
- **ui-smoke:** `Passed!`, 245 succeeded, 0 failed, 0 skipped, 216 s wall while elevation ran beside it (deadline 600 s).
- **ui-tests (the 16 selected classes):** `Passed!`, 324 succeeded, 0 failed, 0 skipped, 127 s (deadline 360 s). The
  new fact is `Passed` in the TRX; it is the only identity added, so the selected count rises by 1.
- **`SceneSaveLoadTests`** (edited, outside the selection): 4 passed.
- **The fifteen s26 invariants** each find nothing; `git grep -n -w "TestElement" -- Berreman/` finds nothing.
- No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1421
  ui_smoke_tests: 245
  ui_tests: 324
```

## Architecture

- **One record for a table element.** `Setups.SetupSlot` (Domain, the lower type) survives; the Ui's `TestElement` of
  the same shape minus `content` is deleted (AGENTS.md "A bijection is collapsed, not bridged").
- **The slot lays itself out.** `model.layoutElements` maps each slot's `layoutElement`, so the one-ray-law shape is
  written once, in `Setups.fs`.
- **Ui-only geometry stays in the Ui.** `elementX`, `boxCorners` and the test-scene drawing are extension members on
  `SetupSlot` in `TableAndElementRotationView.fs`, where the `TestElement` members were.
- **Scene capture/restore** still maps slot ↔ `Scene.SceneElement`, restoring with `EmptySlotContent`, until step 0260
  deletes the bridge.

## Deferred

Nothing in this step's scope. Assigning items to slots (0263) and deleting `SceneElement` / capture / restore (0260)
are later steps by the spec's plan.

## Gotchas

- **Two unlisted test files changed.** `KitWindowModelTests.fs` and `SceneSaveLoadTests.fs` named `TestElement` only as
  an `Assert` type argument. The acceptance grep and the build require the rename, so only the type argument changed:
  no assertion, identity or count.
- **`Scene.fs` needed no edit.** `SceneElement`'s doc never named `TestElement`, so slice item (3) is a no-op.
- **Annotate slot records in the view.** It opens `Scene`, whose `SceneElement` shares `id` / `placement` / `zoom` /
  `beam`, and does not open `Setups`. Every construction is annotated `Setups.SetupSlot`.
- **`.manual/016`:** the copied-item fixture in `InverseConstructorTests` keeps its three stored items and bindings;
  only its `theSample` / `theDetector` helpers' return annotations changed.

## Changelog

- 2026-11-10: Step 0259 deleted `TestElement`. The table's elements are now `Setups.SetupSlot`s with `EmptySlotContent`,
  and `layoutElements` reads each slot's `layoutElement`. Added one `MainSceneMsgTests` fact (selected UI 324,
  constructor 1421 unchanged, smoke 245).
