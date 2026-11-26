# Step 0238 — state of the world

## Where we are

Spec 0050, section S7.2 ("The Items panel, the flow strip and item mapping on copy"), step 0238 (IMPLEMENT),
obligation `S72-FLOW-STATE`.

- 001 §O asks for a left flow strip: Items → Setups → Experiments → Data → Unknowns → Plan → Fit → Results → Publish.
- In this section the strip has three steps: Items, Setups (the table) and Experiments (the existing Experiments bay).
  Per .spec-md §3.22, a step appears only when it works.
- This step makes the strip model state: which step the window shows, and how a step press and a ribbon bay choice
  keep that step and the open bay in agreement.
- No view reads the state yet. Drawing the strip and the Items panel belongs to later S7.2 steps (step 0239 onward).

## What's working

- Added `ConstructorFlowStep` (`ItemsFlowStep`, `SetupsFlowStep`, `ExperimentsFlowStep`). `all` lists the steps in
  001 §O's order, and `caption` gives "Items", "Setups" or "Experiments".
- Added `TableAndElementRotationModel.flowStep`. A window opens on `SetupsFlowStep` with the Add bay, in both the
  forward and inverse constructors.
- Added `ShowFlowStep`:
  - Items sets the step alone.
  - Setups leaves the Experiments bay for Add and keeps any other bay.
  - Experiments opens the Experiments bay.
- `SelectBay` keeps the strip on Experiments for the Experiments bay, and on Setups for any other bay.
- Added three `KitWindowModelTests` facts (constructor-unit-tests 1363 → 1366).

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster:

- build;
- elevation;
- constructor-unit-tests, over everything discovered;
- ui-smoke, over everything discovered;
- ui-tests, selected to `OpticalConstructor.Ui.Tests.KitWindowTests`;
- the fourteen s24 invariants.

The worker's development runs over the final tree are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0238-*`:

- **build:** exit 0, then `0 Warning(s)`, `0 Error(s)` on the rebuild from the restored source.
- **constructor-unit-tests:** `Passed!`, 1366 succeeded, 0 failed, 0 skipped, in 64 s (estimate 180 s). That is
  checkpoint 236's 1363 plus the 3 declared facts:
  - `the flow strip's steps follow 001's order and a window opens on Setups`;
  - `showing a flow step shows its surface, and the Experiments step is the Experiments bay`;
  - `choosing a ribbon bay keeps the flow strip on Setups or Experiments`.
- **ui-smoke:** `Passed!`, 242 succeeded, 0 failed, 0 skipped, in 3 min 18 s (attempt timeout 600 s).
- **ui-tests (`KitWindowTests`):** `Passed!`, 13 succeeded, 0 failed, 0 skipped, in 19.5 s.
- **elevation** from the arc origin `f395597`: exit 0, `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in
  203 s. Known findings: 0.
- **The fourteen s24 invariant greps:** none finds anything.
- **Mutation check:** four mutations of the new state were each caught by a declared fact:
  - the window opens on Items;
  - `SelectBay` ignores the step;
  - Setups keeps the Experiments bay;
  - the Experiments step does not open the bay.

  The source was then restored byte for byte, rebuilt and re-run.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1366
  ui_smoke_tests: 242
  ui_tests: 13
```

## Architecture

- **Window state, not kit state.** `ConstructorFlowStep` is never stored, so it carries no `value` wire tag, the same
  as `ConstructorMode`. Scene capture and restore leave it alone.
- **One update for both constructors.** The forward and inverse scenes both build through `initWith` and share
  `updateTableAndElementRotation`, so they show the same strip with no code of their own.
- **The ribbon and the strip agree through two writers.** `SelectBay` derives the step from the bay, and
  `ShowFlowStep` derives the bay from the step: Experiments opens the Experiments bay, and Setups leaves it. Items
  leaves the ribbon untouched, because the Items surface is not a bay.
- **Room for the later steps.** The order is 001 §O's. S9.3, S11 and S14 add Data, Unknowns, Plan, Fit, Results and
  Publish to `all` once each works (.spec-md §3.22). The doc on `all` records this.

## Deferred

Nothing in this step's scope is deferred. Drawing the strip, and the Items panel the Items step shows, belong to
later S7.2 steps' own slices; they are not left over from this step.

## Gotchas

- **A record update of `ribbon` bypasses the strip.** Several Ui.Tests set `ribbon` directly
  (`ScrollExtentTests`, `ElementMenuTests`, `SourceAndDetectorMoveTests`, `LibraryWindowTests`) and leave
  `flowStep` on Setups. That is harmless while no view reads `flowStep`. The step that draws the strip should
  route those tests through `SelectBay`, or accept the mismatch.
- **Items keeps whatever bay was open.** A model on Items can still hold `BayNames.experiments`. Pressing Setups from
  there opens Add, which fact 2 checks.

## Changelog

- 2026-11-09 — Step 0238:
  - added `ConstructorFlowStep`, `TableAndElementRotationModel.flowStep` and `ShowFlowStep`;
  - made `SelectBay` keep the flow step on Setups or Experiments;
  - added three `KitWindowModelTests` facts (constructor-unit-tests 1363 → 1366).
