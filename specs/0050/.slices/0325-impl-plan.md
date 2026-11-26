# Step 0325 — impl plan: the Data step's review panel

## Approach

Show the reviewed experiment's points in the Data step (spec 0050 S9.3; 001 §K, §O): a heading, the exclusion-reason
field, a points chart where an excluded point stays drawn as a ring, and one row per point with Exclude or Include.
The model side already exists (steps 313–315: `ReviewExperimentData`, `SetExclusionReason`, `ExcludePoint`,
`IncludePoint`, `reviewedDataOpt`, `dataDraft`, `MappedAttachment.tryFindExclusion`); this step is view + ids + tests.

1. `OpticalConstructor.Controls/ExperimentControls.fs`, after the inline chart: `PointMark` (`KeptPointMark` |
   `ExcludedPointMark`), `PlottedPoint` and `pointsChartView`, a `Canvas` of `chartFrame`'s size drawn by the existing
   private `PlotWindow.over` (the points as one series), `gridLines`, `axisLines`, `axisLabels`; a 5 × 5 filled dot in
   `seriesColors.[0]` per kept point, a 9 × 9 unfilled red ring (stroke 1.5) per excluded point, centred; no line.
2. `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: `MappedPointNumber.uiPosition`, `MappedPoints.pointCaptions`
   (`%g` numbers, five shapes), `MappedPoints.chartPoints` (x the 1-based number, y the first reading value), and on
   `ConstructorUiId` `dataReviewButton`, `dataPointRow`, `dataExcludePointButton`, `dataIncludePointButton`.
3. `OpticalConstructor.Controls/UiIds.fs`, Data step group: `dataReviewPanel`, `dataExclusionReasonField`,
   `dataPointsChart`, `dataPointDot`, `dataExcludedPointMark`.
4. `OpticalConstructor.Ui/DataStepView.fs`: a `Review` verb per data-file row (always offered); after the split panel,
   the review panel when `reviewedDataOpt` is `Some` and its experiment is held.
5. `OpticalConstructor.Ui.Tests/DataStepTests.fs`: extend the ids fact; three new ui-tests facts and one ui-smoke fact.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/ExperimentControls.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/DataStepView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/DataStepTests.fs`

## Risks

- **Stale handlers.** `availableWorkbenchButton` re-patches its press handler only when its id changes. Point ids do
  not carry the experiment, so the review panel is keyed by the reviewed experiment: reviewing another one recreates it.
- **An unblurred reason field.** A verb box takes no focus, so a typed but uncommitted reason would be lost. Exclude
  commits the field's live text first, as Create setup does (spec 0050 S4.3), then sends `ExcludePoint` with it.
- ELEV: unique names (`PointMark`, `PlottedPoint`, the two cases, `pointsChartView`), no primitive in a public
  signature, and no `bool` mapping of the two-case `PointMark`.
