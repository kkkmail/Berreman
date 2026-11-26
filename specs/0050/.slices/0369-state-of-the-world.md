# Step 0369 — state of the world

## Where we are

Spec 0050 section S11.2 (the study's pre-fit checks). The solver hand-off's three validation checks now live in Domain
as `Experiment.prefitCheckWith`, the checks a study runs before a fit; `SolverHandoffView` calls them and stays the
screen's display until S11.5 deletes it.

## What's working

- Added `PrefitChecks` in Domain: `SetupSlot.specification`, the typed `PrefitError` and `Experiment.prefitCheckWith`, answering the kept, validated points
- `SolverHandoffView.validateExperiment` maps `prefitCheckWith` onto its status with every caption unchanged
- Deleted the view's private slot-state rule; the setup line tags slots through `specification`
- Added `PrefitCheckTests` (5 facts): slots, missing data, unreadable / inconsistent data, kept points, error text

## Tests

```yaml
gates:
  constructor_unit_tests: 1619
  ui_smoke_tests: 258
  ui_tests: 9
```

- build: exit 0, no warning from our code (79 s; deadline 600 s).
- constructor-unit-tests: Passed!, 1619 (1614 at checkpoint 367, +5) in 71 s (deadline 180 s).
- ui-tests over `SolverHandoffWindowTests`: Passed!, 9, unchanged, 19 s (deadline 360 s).
- ui-smoke: Passed!, 258 in 229 s (deadline 600 s).
- elevation from arc base `f395597c`: OK, 0 new findings in 13 projects, 218 s (deadline 600 s).
- The twenty s37 invariants: every decoded `git grep` finds nothing.
- Nothing deferred. All durations are within their deadlines.

## Architecture

- `PrefitChecks.fs` compiles last in Domain, after `ExperimentMatrices.fs`, because it reads setups, experiments and
  the data seam (`ExperimentDataLoad.loadAndValidate`).
- `PrefitError` keeps the measured-data error as its payload (`UnreadableDataErr` for malformed / empty files,
  `InconsistentDataErr` for range / units mismatches), so the screen maps it onto its parse / validation status through
  the existing `ExperimentDataError.caption`.
- The reasons are the view's texts, moved verbatim, so the screen's captions are byte-identical.

## Deferred

Nothing.

## Gotchas

- The slice calls the retired rule `unspecifiedElements`. In the tree it was the private `HandoffSlotState` /
  `SetupSlot.handoffState`, and that is what was deleted.
- The setup-not-found reason keeps the view's prefix (`its setup is not in the collection: …`) so the caption is
  unchanged. It still names the reference (`'Missing' revision 1`).
- In a test, an untyped `{ points = … }` record infers `EllipsometricSeries`, so annotate `IntensitySeries`.

## Changelog

- 2026-11-13 — step 0369: the hand-off's three checks moved into Domain as `Experiment.prefitCheckWith`, plus
  `PrefitCheckTests` (+5 constructor facts).
