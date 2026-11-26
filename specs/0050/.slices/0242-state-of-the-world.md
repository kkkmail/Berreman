# Step 0242 — state of the world

## Where we are

Spec 0050, section S7.2 ("The Items panel, the flow strip and item mapping on copy"), step 0242 (IMPLEMENT),
obligation `S72-FLOW-STRIP-VIEW`.

- Step 238 gave the window its `ConstructorFlowStep` state and `ShowFlowStep` message.
- Step 241 gave the Items panel its view, which no window mounted yet.
- This step mounts both. The constructor window now shows the flow strip on its left: Items, Setups and Experiments,
  each reachable directly, each under its `ConstructorUiId`. The step's surface fills the rest: the Items panel for
  Items, the workbench for Setups and Experiments.
- Threading the App's item models into the window (0243) is the next slice's own scope.

## What's working

- Added the flow strip view (`FlowStripView.flowStripView`): one box per step, top to bottom; the shown step is filled.
- The kit window shows the strip left of its step's surface: the Items panel, the table, or the Experiments bay.
- The surface is keyed `items` / `workbench`: a change between them recreates it, and Setups ↔ Experiments keeps the
  workbench.
- Added the strip's ids (`flowStrip`, `flowItemsStep`, `flowSetupsStep`, `flowExperimentsStep`) and
  `ConstructorFlowStep.flowUiId`.
- The strip scrolls when the kit bar leaves a short window too little height.
- The scroll-extent law brings a corner into view through every nested viewer. Added four facts: ui-tests over the
  selection 25 → 28, ui-smoke 243 → 244.

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster:

- build;
- elevation;
- constructor-unit-tests;
- ui-smoke;
- ui-tests, selected to `KitWindowTests` and `ScrollExtentTests`;
- the fourteen s24 invariants.

The worker's development runs over the final tree, with logs under `specs/0050/.artifacts/0242-*`:

- **build:** exit 0, `0 Warning(s)`, `0 Error(s)` (`0242-build.log`).
- **constructor-unit-tests:** `Passed!`, 1377 succeeded, 0 failed, 0 skipped, in 66 s (estimate 180 s). This is
  checkpoint 241's 1377, unchanged.
- **ui-smoke:** `Passed!`, 244 succeeded, 0 failed, 0 skipped, in 201 s (attempt timeout 600 s). That is checkpoint
  241's 243 plus `pressing the flow strip's steps shows the Items panel, the Experiments bay and the table`, `Passed`
  in the TRX. The run preceded a comment-only edit in `KitWindowView.fs`. The final rebuilt tree re-passed that fact
  and both selected classes.
- **ui-tests (the two selected classes):** `Passed!`, 28 succeeded, 0 failed, 0 skipped, in 25 s. That is 25 before
  plus three facts, each `Passed` in the TRX under its declared id:
  - `the flow strip UiIds are the stable intent-named ids`;
  - `the flow strip shows Items, Setups and Experiments left of the surface its step shows`;
  - `the kit window's Items step leaves everything reachable`.

  `the kit window: the kit bar and the page stay reachable` passes with the strip present, its assertion unchanged.
- **elevation** from the arc origin `f395597`: exit 0, `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in
  202 s, on the final tree. Known findings: 0.
- **The fourteen s24 invariant greps,** untracked files included: none finds anything.
- **Mutation check:** nine mutations of the new code, each caught by a declared fact (`0242-mutations.py`,
  `0242-mutations-run.log`). Every source was restored by SHA-256 and rebuilt. The mutations were:
  - the boxes bottom to top;
  - Items and Setups ids swapped;
  - the strip docked right;
  - Items showing the workbench;
  - Experiments showing the Items panel;
  - the shown step unfilled;
  - every box sending Setups;
  - the workbench keyed by its step;
  - the strip's id missing.

  Separately, an unscrolled strip fails the scroll-extent law under the new harness.
- **Outside the gate's selection:** the whole ui-tests suite ran once as a regression check: 672 of 673 passed.
  The one failure is `InverseConstructorTests.recommitting a copied experiment preserves its assigned physical ids`.
  - It fails deterministically on a model-only path (`updateTableAndElementRotation`), with "the kit has no item of
    that id". This step's diff does not reach that path.
  - The refusal came in step 0227, after the fact was written in 0221. 0227's selection did not include the class.
  - Step 0261 selects the class and rewrites the experiment ↔ setup reference the fact exercises.
  - `InverseConstructorTests.fs` is outside this step's edits, so the fact is left as found. The `HEAD` reproduction
    is in Gotchas.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1377
  ui_smoke_tests: 244
  ui_tests: 28
```

## Architecture

- **The strip is a view of its own** (`FlowStripView.fs`, between `ItemsPanelView.fs` and `KitWindowView.fs`). It is
  a pure projection of the window's `flowStep`. Every box sends `ShowFlowStep`, the same message a test sends.
- **The host places it.** `kitWindowView` docks the strip left inside a vertical `ScrollViewer`, after the kit bar
  and before the fill. The strip itself does not know where it sits.
- **The surface is one keyed slot**, the `Ribbon.ribbonView` discipline. FuncUI 2.0's `View.withKey` keys a typed
  view, so each surface is the child of a keyed `Border` (`items` / `workbench`).
- **The id of a step's box is a member of the step** (`ConstructorFlowStep.flowUiId`). The ids themselves are fixed
  members of `ConstructorUiId` in their own group.

## Deferred

Nothing in this step's scope is deferred. Threading the App's item models (0243) and the later flow steps (Data,
Unknowns, Plan, Fit, Results, Publish, from S9.3, S11 and S14) are later slices' own scope.

## Gotchas

- **Nested scroll viewers.** Avalonia's `ScrollContentPresenter` marks a bring-into-view request handled once it
  scrolls. A single `BringIntoView` therefore never reaches the viewer around a nested one, such as the Items panel's
  offer list inside the panel.
  - The scroll-extent law (`unreachable`) now repeats the request once per enclosing `ScrollViewer`.
  - Without that, the Items step's 71 offer corners fail. They fail the same way with `itemsPanelView` mounted alone,
    so they are not the kit window's.
- **The strip must scroll.** At 1000 × 540 the tallest kit bar leaves less height than three headless-wide boxes
  need, about 200 × 30 px each.
- **The table canvas and a ribbon pane carry their id as `Name`**, not AutomationId; the new facts look them up by
  name.
- **A pre-existing red outside the selection:** `InverseConstructorTests.recommitting a copied experiment preserves
  its assigned physical ids` (see Tests).
  - It was reproduced on an unmodified `git archive HEAD` copy, built and run under `.artifacts/` and deleted after.
    That run failed the same way with the same message (`0242-head-check-build.log`, `0242-head-check-test.log`), so
    it predates this step.
  - The operator should know the class is red before step 0261 selects it.
- **Selected ui-tests counts are not checkpointed.** The 28 is this step's selection: 25 + 3.

## Changelog

- 2026-11-10 — Step 0242:
  - added `FlowStripView.flowStripView`, the strip's four ids and `ConstructorFlowStep.flowUiId`;
  - the kit window shows the strip left of a keyed surface, Items panel or workbench, the strip in a vertical
    scroller;
  - the scroll-extent law reaches through nested viewers;
  - four facts added (ui-tests over the selection 25 → 28, ui-smoke 243 → 244).
