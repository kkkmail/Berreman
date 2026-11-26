# Step 0293 — state of the world

## Where we are

Spec 0050 section S9.1 covers Experiments v2: scans, roles, sessions and attachments.
- Step 288 gave the draft and the experiment a multi-variable `ExperimentScan` (blocks of axes), step 287 the axis
  grid (`VariableRange.sweptValues`), step 289 the dial-reading variable, and step 292 the `ExpAddScanAxis` /
  `ExpAddScanBlock` messages that build a scan.
- Until now the chart drew one block of one axis only, so a two-variable draft or a multi-block experiment showed
  nothing.

This step makes the experiment chart, the pop-out chart and Export data read the whole scan. The bay's Add variable and
Add block buttons remain step 296's.

## What's working

- `chartForRun` takes the model and an `ExperimentScan` and draws every block: per value of its outer axes, one series
  per captured branch and reading, over its last axis.
- A path step is built once per block and swept only when an axis of the block is on it or varies the wavelength.
- A one-block, one-axis scan draws exactly the previous chart: names, labels, title, description and numbers.
- The draft chart, a collected experiment's View chart and Export data now carry every series of a multi-variable scan;
  `chartForParams` (no caller) is deleted.
- AGENTS.md's experiment gotcha says the chart is the scan's.
- Three new `ExperimentControlsTests` facts cover the two-variable draft, a two-block collected experiment and the export.

## Tests

These are development runs with the gates' own commands and this step's selectors. The arc-runner's gate engine runs
the gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors (incremental) | 15 s | 600 s |
| constructor-unit-tests | Passed!, 1492 of 1492, 0 failed, 0 skipped | 74 s | 180 s |
| ui-tests (6 selected classes, the gate's exact command) | Passed!, 95 of 95, 0 failed, 0 skipped | 105 s | 360 s |
| ui-smoke | Passed!, 249 of 249, 0 failed, 0 skipped | 206 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 203 s | 600 s |
| invariant-s29-\* (16) | every decoded `git grep` finds nothing | < 1 s each | 300 s |

- **ui-tests: 95** over this step's six classes (step 0292 selected three):
  - ExperimentControls 39, which is 36 + the three new facts, all `Passed` in the TRX;
  - FsxAcceptance 14 and WorkbenchInfoAndCapture 8, unchanged, so the one-axis numbers hold;
  - ElementMenu 15, InverseConstructor 14, MainWorkbench 5.
- **constructor-unit-tests: 1492** and **ui-smoke: 249**, both unchanged.
- **Mutation check (red):**
  - Ignoring the outer axes' settings failed the two-variable fact (the scan read 1.0 where the film turned to 20° reads
    0.985).
  - Restoring the one-block limit in `chartForExperiment` failed the collected fact (7 series expected, 0 drawn).
  - Truncating the export to one series failed the export fact.
  - The product file was restored byte for byte (SHA-256 checked) and rebuilt.

```yaml
gates:
  constructor_unit_tests: 1492
  ui_smoke_tests: 249
  ui_tests: 95
```

## Architecture

- **One builder, settings-driven.** `StepBuilder.SweptStep` takes the point's `ScanSetting list` (the group's settings
  then the x setting) in place of a swept `float`. A swept step reads:
  - its incidence from a `VaryR2` setting on it;
  - its rotation from a `VaryR1` setting on it, or a `VaryDialReading` one at the item's physical angle
    (`SetupSlot.tryFindDialMounted`, resolved once per step);
  - its wavelength from any `VaryWaveLength` setting;
  - each else its drawn value, or `operatingWaveLength model scan.sourceWaveLengthOpt`.
- **The groups are the domain's product.** The outer axes' values come from `ScanBlock.points` over the outer axes
  (first outermost), and the x values from the x axis's `sweptValues`. No product or grid is re-implemented.
- **A block gives series, a status, or nothing.** The private `ScanBlockChart` record holds a drawn block. The first
  status in block order (no analyzer, the run cannot be evaluated, materials failed) is the chart, so a failure is never
  hidden behind another block's series. Blocks without an axis, with an element off the table or unbound, or with
  nothing captured give no series; a scan of no series is the empty chart.
- **The draft chart is the chart its commit will have.** `experimentResult` reads
  `(Experiments.chooseMeasurement capture collection).draft.scanOpt`, the same coercion `ExpCommit` records.
- **The multi-variable description reuses `Experiment.description`.** The chart wraps the scan in an `Experiment` whose
  other fields it never reads, because a scan with an axis is described from its blocks alone.

## Deferred

Nothing. Every requirement of the step landed in this round. The bay's Add variable / Add block buttons are step 296's,
by the spec.

## Gotchas

- **Spinning an isotropic sample changes nothing.** The plane of incidence is set by the beam and the normal, not by
  the sample's in-plane orientation. A glass plate's R1 sweep is therefore flat, and a test using one cannot tell whether
  the inner axis was applied. The new facts use the built-in uniaxial film (`SampleIds.uniaxialId`, optic axis in its
  plane).
- **How a non-trivial scan is named and labelled** (interpretations of the slice, recorded in the impl-log):
  - A one-axis block in a scan of several blocks has no group settings, so its series reads `Intensity — block 1`.
  - The x label joins the one-variable x labels (`Rotation R1 (°)`), not `VariableParameter.label`, so a two-axis
    block over R1 is labelled as a one-axis R1 chart.
  - The y label applies the one-variable rule over every drawn block's detector kinds.
  - The title is the first drawn block's.
- **The first block that cannot be drawn replaces the whole chart** with its status chart (no analyzer, cannot be
  evaluated, materials failed), even when other blocks have series.
- **Every `VaryR2` axis is clamped**, outer ones included, to [0, `r2SweepMaxDegrees`].
- **The preset polarizer now follows a wavelength axis.** Before, it stayed at `w0`. The numbers are unchanged, because
  `BoundPolarizerPreset.muellerAt` ignores the wavelength.
- **The multi-variable description wraps the scan in an `Experiment`** to read `Experiment.description`. The id, name
  and setup it carries are never read for a scan with an axis.
  - `Experiments.fs` is outside this step's edits, so a `ScanBlock` text member could not be added.
  - Copying `blockText` into the view would have duplicated it.
- **AGENTS.md "in no more words"** is read as "add exactly that sentence". The rest of the gotcha is unchanged.
- **A non-incremental build recompiles the vendored MathNet**, which prints its existing `SYSLIB0051` warnings. The
  final incremental build reports 0 warnings.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0293: the experiment chart reads the whole scan.
  - `chartForRun` takes `(model) (scan)`; `chartForParams` is deleted.
  - The draft chart, the collected chart and Export data carry every series of a multi-variable or multi-block scan.
  - One-axis charts are unchanged.
  - Three new `ExperimentControlsTests` facts.
