# Step 0293 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice, AGENTS.md and steps 0288 / 0289 / 0292 outputs. The operator
  note is empty.
- [x] `chartForRun` takes `(model) (scan : Experiments.ExperimentScan)` and draws every block; `chartForParams` deleted.
- [x] `experimentResult` charts `draft.scanOpt` read on `model.draftCapture`; `chartForExperiment` charts `exp.scan`.
- [x] AGENTS.md gotcha: one added sentence.
- [x] Three new `ExperimentControlsTests` facts.
- [x] Development runs: build, constructor-unit-tests, ui-smoke, ui-tests (selected), elevation, s29 invariants.
- [x] Mutation checks (red), then the product file restored byte for byte and rebuilt.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - `StepBuilder.SweptStep` now takes the point's `ScanSetting list` in place of a swept `float`.
  - New private record `ScanBlockChart` (a drawn block: series, detector kinds, x variable, one-variable title and
    description).
  - `chartForRun` rewritten over a scan; `chartForParams` deleted; `experimentResult` and `chartForExperiment` chart the
    scan; the hook-wiring comment names `experimentResult` in place of the deleted `chartForParams`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs` — a new S9.1 section: the
  `polarizerFilmTable` fixture, the `filmOver` / `twoVariableDraft` helpers and the three facts.
- `AGENTS.md` — the sentence "An experiment's chart is its scan's: one series per value of the outer axes and captured
  branch, over the last axis of each block."

## Decisions

- **Groups reuse the domain's product.** The outer axes' values are `{ axes = outer; capture }.points` (step 288's
  `ScanBlock.points`, first axis outermost), each point's settings one group; the x settings are the x axis's
  `sweptValues` (step 287), so the one-axis x grid is today's arithmetic exactly.
- **A step is swept when an axis of the block is on its element, or any axis varies the wavelength**; otherwise it is
  built once (`FixedStep`). A swept step reads the point's settings: incidence from a `VaryR2` setting on it, rotation
  from a `VaryR1` setting or a `VaryDialReading` setting at its physical angle (`tryFindDialMounted`, resolved once per
  step), wavelength from any `VaryWaveLength` setting; each else its drawn value / `operatingWaveLength`. When two
  settings name the same element and variable, the innermost wins (`List.tryFindBack`).
- **The preset polarizer follows a wavelength axis.** Today it stayed fixed at `w0` in a wavelength sweep; the slice says
  every element runs at a `VaryWaveLength` setting. `BoundPolarizerPreset.muellerAt` ignores the wavelength
  (`Propagation.behaviorMueller behavior rotation`), so the numbers are unchanged.
- **Every `VaryR2` axis is clamped**, outer ones included, so an incidence setting is always computable and the series
  name shows the value actually run.
- **Combining blocks.** A block gives its series, a status chart (no analyzer, cannot be evaluated, materials failed) or
  nothing (no axis, element off the table, unbound, nothing captured). The first status in block order is the chart;
  else the drawn blocks' series in order; no series is the empty chart. One block of one axis is therefore today's chart.
- **Series names.** The suffix parts are the group's setting texts then `block <k>` when the scan has more than one
  block; with no part the name is today's, else `<today's> — <parts joined by ", ">`. So a one-axis block of a
  two-block scan reads `Intensity — block 1`.
- **x label** joins the distinct drawn x variables' one-variable x labels (`Rotation R1 (°)`, …) with ` / `; the y
  label is today's rule over the drawn blocks' detector kinds; the title is the first drawn block's.
- **Description.** One block of one axis: today's. Otherwise `Source <λ> nm` (or `Source swept` when any axis varies the
  wavelength), then the scan as `Experiment.description` names it. The chart wraps the scan in an `Experiment` whose
  other fields it never reads (a scan with an axis is described from its blocks alone); `Experiments.fs` is outside this
  step's edits, so no `ScanBlock` text member could be added, and copying `blockText` would duplicate it.
- **`experimentResult`** reads `(Experiments.chooseMeasurement capture collection).draft.scanOpt`, the same coercion
  `ExpCommit` records, so the draft chart is the chart its commit will have.

- **The test table holds the uniaxial film** (`SampleIds.uniaxialId`, n = 1.5 / 1.65 / 1.65, optic axis in its plane),
  not a glass plate. Spinning an isotropic plate about its normal changes nothing, so with a plate the inner R1 axis
  would be untested (the first run with the 1 mm plate showed every series flat in R1). The slice asks for "a sample".
- **AGENTS.md: "in no more words"** is read as "add exactly that statement and nothing else". The gotcha gains one
  sentence; the rest is unchanged (rewriting it to keep the word count would go beyond the slice).

## Testing state

Development runs with each gate's own command, one after another except where noted. The arc-runner's gate engine runs
the gates themselves. No run exceeded its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors (incremental) | 15 s | 600 s |
| constructor-unit-tests | Passed!, 1492 / 1492 | 74 s (beside elevation) | 180 s |
| ui-tests (6 selected classes, the gate's command) | Passed!, 95 / 95 | 105 s (beside elevation) | 360 s |
| ui-smoke | Passed!, 249 / 249 | 206 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 203 s | 600 s |
| invariant-s29-\* (16) | every decoded `git grep` finds nothing | < 1 s each | 300 s |

- ui-tests per class (TRX): ElementMenu 15, ExperimentControls 39 (36 + 3), FsxAcceptance 14 (unchanged), InverseConstructor
  14, MainWorkbench 5, WorkbenchInfoAndCapture 8 (unchanged). The three new facts are `Passed`.
- Mutation checks:
  - `settingOn` reading only the innermost setting (outer axes ignored) → the two-variable fact fails: "Intensity (T) —
    Sample #4: Incidence R2 20° at R1 0: the scan read 1.000000000000011, the turned film 0.9846315583986155".
  - `chartForExperiment` back to one block → the collected fact fails (expected 7 series, got 0).
  - Export truncated to one series → the export fact fails ("Exported 1 series" for 6).
  - The product file was restored (SHA-256 checked) and rebuilt; the three facts pass again.

## Artifacts

- `specs/0050/.artifacts/0293-build.log`
- `specs/0050/.artifacts/0293-elevation.log`
- `specs/0050/.artifacts/0293-constructor-unit-tests.log`
- `specs/0050/.artifacts/0293-ui-tests.log`
- `specs/0050/.artifacts/0293-ui-smoke.log`
