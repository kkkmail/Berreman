# Step 0293 — impl plan

## Approach

`chartForRun` (`OpticalConstructor.Ui/TableAndElementRotationView.fs`) changes shape to `(model) (scan :
Experiments.ExperimentScan)` and draws every block of the scan; `chartForParams` (no caller) is deleted.

Per block with at least one axis, in order:

- the last axis is the x axis (a `VaryR2` range clamped to [0, `r2SweepMaxDegrees`] as today), its element the varied
  one: captures (`model.captureDetectors`), the no-analyzer check and the bound check apply to it as today;
- the other axes (also clamped when `VaryR2`) give the groups: the domain's own product
  (`{ axes = outer; capture }.points`, first outermost), one group of series per point;
- every path step is built once per block (`StepBuilder`): `FixedStep` when no axis of the block touches it, else a
  `SweptStep` over the point's settings (the group's plus the x one), reading incidence (`VaryR2` on it), spin /
  rotation (`VaryR1`, or `VaryDialReading` at its physical angle) and wavelength (any `VaryWaveLength` axis), each
  else its drawn value / `operatingWaveLength model scan.sourceWaveLengthOpt`;
- each group reads every captured branch as today, one series per branch and reading.

A block's outcome is either its series (with its title, x label and detector kinds) or a status chart (no analyzer, the
run cannot be evaluated, sample materials failed), or nothing (no axis, missing / unbound element, nothing captured).
The scan's chart: the first status in block order, else all series; no series is the empty chart.

Naming: today's name, then ` — ` and the group's settings (`<axis label>: <variable.label> <%g value><unit>`) joined by
`, `, then `, block <k>` when the scan has more than one block; with nothing to add (one block of one axis) the name is
today's. One block of one axis keeps today's labels, title and description exactly; otherwise the x label joins the
distinct x variables' axis labels with ` / `, `xAxisKind` is angular when every x variable is an angle, the title is
the first drawn block's, and the description is the source text then the scan named by `Experiment.description`.

`experimentResult` charts `draft.scanOpt` with its last block's capture replaced by `model.draftCapture`;
`chartForExperiment` charts `exp.scan` (the one-axis limit goes). Export data and the pop-out read `experimentResult`
unchanged. AGENTS.md's gotcha gains the scan sentence.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs` (+3 facts)
- `AGENTS.md`

## Risks

- One-axis charts must stay byte-identical (FsxAcceptanceTests, WorkbenchInfoAndCaptureTests, ElementMenuTests): the
  x grid is `sweptValues` (same arithmetic as today's `xs`), and the swept step evaluates the same orientation and
  wavelength as today's per-variable arms.
- Naming the scan through `Experiment.description` needs an `Experiment` value: the chart wraps the scan in one whose
  other fields it never reads (a scan with an axis is described from its blocks alone).
- Normal-incidence reflection geometry in the test table: the sample's drawn R2 is 0, the axes set its incidence.
