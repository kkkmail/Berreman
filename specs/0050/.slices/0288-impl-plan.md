# Step 0288 — impl plan

Experiment v2 (spec 0050 S9.1): `Experiment` holds a `scan`, a `role`, a `sessionOpt` and `notes` in place of
`varied`, `measurement`, `range` and `sourceWaveLengthOpt`. `VariedElement` and `Experiment.variedLabel` are deleted;
`ScanAxis` (step 287) is their successor. The draft builds a scan of several axes and blocks.

## Approach

1. `OpticalConstructor.Domain/Experiments.fs`
   - Reshape `Experiment`. `description` keeps today's two texts for a one-block, one-axis scan and a one-block,
     no-axis scan. Any other scan joins its block texts with `; `.
   - `ExperimentDraft` gains `outerAxes` and `closedBlocks`, plus the `axisOpt` and `scanOpt` members.
     `canCommit` matches `axisOpt`.
   - Add `addScanAxis` and `addScanBlock`.
   - `commit` builds from `scanOpt`. An edited experiment keeps its role, session, notes and data file. An appended
     one is a `ScienceRun` with no session, no notes and no data file.
   - `edit` splits the scan back into the draft.
2. `OutOfBandDiagnostic.requestedWavelengthsOf` spans the scan's wavelength axes. It reuses `requestedWavelengthsFor`
   over a spanning range.
3. `ExperimentDataLoad.loadAndValidate`:
   - a scan of one block of one axis validates against that axis;
   - a scan of one block of no axis validates against `None` and `forVariable VaryR1`;
   - any other scan answers `DataRangeMismatchErr` and reads nothing.
4. `experiment.schema.json`: the new required keys. `scan` is `{blocks, sourceWaveLengthOpt}`, `role` is an integer
   1–7, `sessionOpt` is null or a non-blank string, and `notes` is a string. The version stays 1.
5. `TableAndElementRotationView.chartForExperiment` charts only a scan of one block of one axis.
6. Fixtures. Every test fixture that builds an `Experiment` is re-pointed to `ExperimentScan.oneAxis` or `darkLine`,
   with `ScienceRun`, no session and `ExperimentNotes.none`. The build finds any outside the listed files.
7. New and changed tests, as the slice lists them: three new facts and three changed expectations.

## Files

The Domain files `Experiments.fs`, `OutOfBandDiagnostic.fs` and `ExperimentDataLoad.fs`; the Storage file
`schema/experiment.schema.json`; the Ui file `TableAndElementRotationView.fs`; and the test fixtures the build finds.

## Risks

- Record-label resolution. `sourceWaveLengthOpt`, `elementId`, `variable`, `range` and `capture` are shared labels,
  so every literal is annotated.
- `commit` while editing with no edited axis (a dark line). Here `scanOpt` is `None`, so the collection is unchanged.
  Editing a dark line and committing still gives it back.
- JSON shape: `role` goes through the existing `ClosedSetTagConverter`, and `sessionOpt` unwraps to a string or null.
