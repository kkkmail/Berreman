# Step 0287 — impl plan

## Approach

The scan becomes pure Domain data in `OpticalConstructor.Domain/Experiments.fs`. Nothing reads it before step 288.

1. `VariableRange.sweptValues : SweptValue list`. It computes `n = max 2 points` values, the k-th being
   `min + (max − min) · k / (n − 1)`. The arithmetic is evaluated in the same order as `chartForRun`'s `xs`
   (`lo + (hi - lo) * float k / float (n - 1)`), so each number is bit-identical. Step 293 points `chartForRun` at it.
2. After `ElementLabel`, in this order:
   - `ScanAxis` with its `describe` member.
   - `ScanSetting`.
   - `ScanPoint`.
   - `ScanBlock` with its `points` member: the product of the axes, built by `List.foldBack` so the first axis is
     outermost, with the block's capture on each point.
   - `ExperimentScan` with `points`, `axes`, `oneAxis` and `darkLine`.
3. `Experiment.description` renders its varied text through `ScanAxis.describe`. The format string then exists once
   (AGENTS.md: reuse, never copy), and its output does not change. This is the shape step 288 gives `description`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Experiments.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentProxyTests.fs`: three new facts, plus test-local
  `sample` / `analyzer` element ids and the shared S9.1 scan fixtures.

## Risks

- **Record-label inference.** New records reuse the labels `elementId`, `label`, `variable`, `range` and
  `sourceWaveLengthOpt`. Placing them before `VariedElement`, `Experiment` and `ExperimentDraft` keeps those types
  latest for an unannotated label lookup. `ScanSetting.value` is a new label, so the build will show whether any
  unannotated `.value` lookup elsewhere changes resolution.
- **ELEV.** Every field is elevated, and every new surface is a member. There is no new module-level name and no new
  DU case.
