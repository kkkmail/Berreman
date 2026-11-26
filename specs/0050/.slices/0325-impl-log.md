# Step 0325 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice and the surrounding code (steps 313–315, 323, 324).
- [x] ExperimentControls: `PointMark`, `PlottedPoint`, `pointsChartView`.
- [x] UiIds: the five Data step review ids.
- [x] ConstructorUiIdOfDomain: `MappedPointNumber.uiPosition`, `MappedPoints.pointCaptions` / `chartPoints`, four ids.
- [x] DataStepView: the Review verb and the review panel (the Ui project compiled: `.artifacts/0325-dev-ui-build.log`).
- [x] DataStepTests: the changed ids fact, three ui-tests facts, one ui-smoke fact.
- [x] Mutation check (`.artifacts/0325-dev-mutation.log`): 10 of 10 caught, each source restored byte-identical.
- [x] Final development runs of every gate over the final tree: all green.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/ExperimentControls.fs`: after the inline chart's `chartCanvas`,
  `PointMark` (`KeptPointMark` | `ExcludedPointMark`), `PlottedPoint` and `pointsChartView`. The view calls the private
  `PlotWindow.over` (the points as one series named by the y label), `gridLines`, `axisLines` and `axisLabels`.
  - A kept point is a 5 × 5 `Ellipse` filled with `seriesColors.[0]`, carrying `dataPointDot`.
  - An excluded point is a 9 × 9 unfilled `Ellipse` with a red stroke of 1.5, carrying `dataExcludedPointMark`.
  - Each mark is centred on its point. No polyline is drawn.
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`, in the Data step group: `dataReviewPanel`,
  `dataExclusionReasonField`, `dataPointsChart`, `dataPointDot` and `dataExcludedPointMark`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`:
  - `Kit.MappedPointNumber.uiPosition`;
  - `ImportProfiles.MappedPoints.pointCaptions` and `.chartPoints`;
  - a private `ImportProfiles.MappedReading.firstValue`, the one match from a reading to its signal or m00, which both
    members share;
  - on `ConstructorUiId`: `dataReviewButton`, `dataPointRow`, `dataExcludePointButton` and `dataIncludePointButton`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/DataStepView.fs`:
  - each data-file row gains `Review` after Attach, always offered;
  - the split panel's frame is now the shared `stepPanelFrame`, which the review panel also uses;
  - new `typedReason`, `reviewedPointRow` and `dataReviewPanel`, the last shown after the split panel.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/DataStepTests.fs`: the ids fact extended, three ui-tests facts
  and one ui-smoke fact, with test-local fixtures (`e1Attachment`, `e1Series`, `e1WithData`, the mock `reviewData`,
  `withReviewModel`, `withRecordingWindow`, `idsUnder`).

## Decisions

- **Exclude commits the field's live text** (spec 0050 S4.3 precedent; S9.3). A verb box takes no focus, so pressing
  Exclude does not blur the reason field. A reason typed without Enter would then still be the draft's old text, and
  `ExcludePoint` would be refused for want of a reason.
  - Exclude therefore reads the field's live text (`typedReason`, falling back to `model.dataDraft.exclusionReason`).
  - It sends `SetExclusionReason` with it, then `ExcludePoint (id, n, reason)`, as Create setup commits the setup name
    before `CreateSetup`.
  - Both messages already exist. When the field was committed, the reason sent is exactly `model.dataDraft.exclusionReason`.
- **The review panel is keyed by its experiment** (`DataReviewPanel_<id>`). `availableWorkbenchButton` re-patches its press
  handler only when its id changes, and a point's ids carry no experiment. Reviewing another experiment therefore
  recreates the panel, and no Exclude or Include acts for the previous one.
- **No panel for an experiment no longer held.** When `reviewedDataOpt` names an experiment the collection no longer
  holds, nothing is shown: there is no name or badge to head the panel, and `ExcludePoint` would change nothing.
- **`stepPanelFrame`.** The split panel's border attributes moved into one private list that both panels use, rather
  than copying them into the review panel.
- **The Review verb** is `availableWorkbenchButton ControlOffered`, the look of the attach and split verbs. Exclude and
  Include are `availableWorkbenchButton model.kitWrites`, so they are disabled and dimmed while the kit takes no writes.
- **The chart canvas has the inline chart's light background** (248), since it is not framed by a border box.

## Testing state

Development runs of each gate's own command over the final tree (`.artifacts/0325-dev-gates-summary.log`):

- build: exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout (22 s; deadline 600 s).
- constructor-unit-tests: Passed!, 1552 of 1552 (70 s; deadline 180 s).
- ui-smoke: Passed!, 256 of 256, up from 255 by `pressing Review shows the experiment's points and their chart` (222 s;
  deadline 600 s).
- ui-tests (`DataStepTests`, `ExperimentControlsTests`): Passed!, 49 of 49. `DataStepTests` has 6 facts, 3 of them new;
  `ExperimentControlsTests` has 43 (18 s; deadline 360 s).
- elevation: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded (207 s; deadline
  600 s).
- The sixteen `invariant-s31-*`: each `INVARIANT HOLDS` (`.artifacts/0325-dev-invariants.log`).

The first selected ui-tests run failed one fact. A raised Enter delivered `SetExclusionReason` twice (see Gotchas), and
the fact now holds the reasons sent rather than their count. Every run since is green.

## Artifacts

- `.artifacts/0325-dev-ui-build.log` — the first compile of the Ui project.
- `.artifacts/0325-dev-build.log` — the solution build.
- `.artifacts/0325-dev-ui-tests.log` — the selected ui-tests run.
- `.artifacts/0325-dev-mutation.log` — the mutation check.
- `.artifacts/0325-dev-constructor-unit-tests.log`, `0325-dev-ui-smoke.log`, `0325-dev-elevation.log`,
  `0325-dev-invariants.log`, `0325-dev-gates-summary.log` — the final gate runs.
- `.artifacts/0325-dev-trx/` — the three TRX reports.
- `.artifacts/0325-scratch/` — the edit and mutation scripts.

## Gotchas

- **Qualified Berreman names resolve to the union case.** `Berreman.Geometry.Angle.degree` does not compile:
  through a module path, `Angle` is the case, not the type. The test opens `Berreman.Geometry`, `Berreman.Fields` and
  `Berreman.Media` (as `FsxAcceptanceTests` does), and builds `SampleAzimuth` by its case.
- **Enter may commit a text field twice.** `KeyDownEvent` routes tunnel then bubble, and the field's handler does not
  mark it handled, so a raised Enter can deliver `SetExclusionReason` twice. The commit is idempotent; the test holds the
  reasons sent (distinct), not their count. Create setup's name field behaves alike.
  - A verb box's press sets `Handled` first, so Exclude sends each message once.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.
