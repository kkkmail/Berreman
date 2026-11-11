# Step 0287 — state of the world

## Where we are

Spec 0050 section S9.1 covers Experiments v2: scans, roles, sessions and attachments. Steps 285 and 286 laid the
run's role, session, notes and name. This step adds the scan as pure Domain data: one or more blocks, each a product
of axes read on one capture (001 §K, §Q AT-2). The scan generalises today's `VariableParameter`, `VariableRange` and
`sourceWaveLengthOpt` rather than duplicating them. Nothing reads the scan yet:
- step 288 makes `Experiment` hold a scan and deletes `VariedElement`;
- step 293 points `chartForRun`'s x values at `sweptValues`.

## What's working

- Add `VariableRange.sweptValues`: `max 2 points` values from `min` to `max`, evenly spaced. It is `chartForRun`'s
  grid, in the same arithmetic order.
- Add `ScanAxis` with `describe`, plus `ScanSetting` and `ScanPoint`.
- Add `ScanBlock.points`, the product of a block's axes. The first axis is outermost, and every point is read on the
  block's capture. A block of no axis is one empty point, a dark line.
- Add `ExperimentScan`, with `points` and `axes` (block after block), `oneAxis` and `darkLine`.
- `Experiment.description` renders its varied text through `ScanAxis.describe`. The text is unchanged.
- Three new constructor-unit-tests facts cover all of the above.

## Tests

These are development runs on the final tree, using the snapshot's own commands. The arc-runner's gate engine runs the
gates themselves. No run went over its effective deadline.

- **build:** exit 0, 0 warnings and 0 errors, in 54 s against a 600 s deadline. The rebuild after the mutation check
  took 15 s, also with 0 warnings.
- **constructor-unit-tests:** Passed!, 1480 of 1480, 0 failed and 0 skipped, in 69–71 s against a 180 s deadline.
  That is 1477 at the last checkpoint plus the three new facts. The TRX shows each new fact `Passed`:
  - `a range sweeps its points evenly from its minimum to its maximum`
  - `a scan block's points are the product of its axes, the first outermost, each read on the block's capture`
  - `a scan is its blocks' points in order, so one experiment joins two designs`
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0 and ELEV004 excluded, in
  199 s against a 600 s deadline.
- **invariant-s29-\*** (16): every decoded grep finds nothing.
- **Red first:** before the Domain code existed, the facts failed to compile on exactly `ScanAxis`, `ScanBlock` and
  `sweptValues`.
- **Mutation check:**
  - Nesting `ScanBlock.points` with the last axis outermost turned the block fact red at its order assertion.
  - The file was restored byte for byte (SHA-256 checked), rebuilt and re-run green: 1480 of 1480.

```yaml
gates:
  constructor_unit_tests: 1480
```

## Architecture

- **Order in `Experiments.fs`.** The new types come after `ElementLabel` and before `VariedElement`:
  `ScanAxis` → `ScanSetting` → `ScanPoint` → `ScanBlock` → `ExperimentScan`.
  - Several record labels are shared: `elementId`, `label`, `variable`, `range` and `sourceWaveLengthOpt`.
  - Because the new types come first, an unannotated lookup of those labels still resolves to `VariedElement`,
    `Experiment` and `ExperimentDraft`, as before. The whole solution built unchanged.
- **The product is a `List.foldBack` over the axes.** Each axis prefixes its settings to every point of the axes
  after it, so the first axis is outermost without any list append. With no axis, the seed `[ [] ]` is the dark
  line's one empty point.
- **`sweptValues` evaluates `lo + (hi - lo) * float k / float (n - 1)`**, exactly the expression in `chartForRun`.
  Step 293's chart therefore keeps every number. The design's angles (0, 20, …, 360; 0, 22.5, 45, 67.5; 89, 44.5,
  0) are exact.
- **`Experiment.description` builds its `ScanAxis` from `varied` and `range`** and appends `, capture <label>`.
  - The varied text's format string exists once (AGENTS.md: reuse, never copy).
  - This is the text step 288 gives a one-block, one-axis scan.
  - No member was added.

## Deferred

Nothing. Every requirement of the slice landed this round.

`chartForRun`'s own `xs` stays in `OpticalConstructor.Ui`. That project is outside this step's `touches` and authorized
edits, and step 293's brief replaces `xs` with `sweptValues`.

## Gotchas

- **F# lambdas take no return-type annotation** (`fun (x : T) : U -> …` is `FS0010`). Annotate the parameter, or
  annotate the expression.
- **What the scan fact pins, and what it does not.** `a scan is its blocks' points in order…` compares its last six
  points with `incidenceByAzimuthBoth.points`, so it does not pin the product's order by itself. The block fact does,
  and the mutation check showed it.
- **Test-local names.** `sample` and `analyzer` are private `ElementId` values in `ExperimentProxyTests`. Neither
  shadows anything the module uses, because the `Sample` kind case and `BuiltinCatalogue.sample` are spelled
  differently or qualified.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11: Step 0287 adds S9.1's scan shapes: `VariableRange.sweptValues`, `ScanAxis`, `ScanSetting`, `ScanPoint`,
  `ScanBlock` and `ExperimentScan`. `Experiment.description` now reuses `ScanAxis.describe`. constructor-unit-tests
  went from 1477 to 1480.
