# Step 0325 — state of the world

## Where we are

This step is in spec 0050 section S9.3: one file serving many experiments, exclusions, and the Data step (001 §K, §O).

- Steps 313–315 gave the model per-point exclusions with a reason and the Data step's review (`ReviewExperimentData`,
  `SetExclusionReason`, `ExcludePoint`, `IncludePoint`, `reviewedDataOpt`), with no surface showing them.
- Step 323 added the Data step and step 324 its split panel.
- This step adds the review panel. A data-file row's Review shows the experiment's points, each with its caption. An
  excluded point is listed with its reason and drawn as a ring beside the kept dots. Exclude sends the typed reason, and
  Include takes the exclusion back.
- Steps 326–327 add the step's further panels, the experiment matrix among them.

## What's working

- Added a Review verb to each data-file row of the Data step, offered over a read-only kit too.
- Added the review panel: heading `Points of <name> — <badge>`, the `Reason:` field, a points chart, then one row per
  point.
- Excluded points stay on the chart as red 9 × 9 rings, kept points are 5 × 5 dots, and no line joins them.
- An excluded row reads `excluded — <reason>` in the message colour with Include; others offer Exclude. Both are
  disabled while the kit takes no writes.
- Added `MappedPoints.pointCaptions` / `chartPoints`, `ExperimentControls.pointsChartView` and nine new UiIds.
- Tests: +3 ui-tests facts in `DataStepTests`, +1 ui-smoke (255 → 256), and the ids fact extended.

## Tests

These are development runs of each gate's own command over the final tree (`.artifacts/0325-dev-gates-summary.log`). The
arc-runner's gate engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 22 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 207 s | 600 s |
| constructor-unit-tests | Passed!, 1552 of 1552, 0 failed, 0 skipped | 70 s | 180 s |
| ui-smoke | Passed!, 256 of 256, 0 failed, 0 skipped | 222 s | 600 s |
| ui-tests (`DataStepTests`, `ExperimentControlsTests`) | Passed!, 49 of 49 | 18 s | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` (`.artifacts/0325-dev-invariants.log`) | under 1 s each | 300 s |

- **The slice's test effects pass under their exact ids** (`.artifacts/0325-dev-trx/`):
  - ui-tests: `each mapped point reads as its caption and plots at its number`, `the review panel lists every point, an
    excluded one marked with its reason, and plots the excluded points as rings`, `pressing Exclude sends the point with
    the typed reason, and Include takes it back`, and the changed `the Data step UiIds are the stable intent-named ids`;
  - ui-smoke: `pressing Review shows the experiment's points and their chart`.
- **Counts.**
  - ui-smoke rose by 1, from 255 to 256.
  - In the selected ui-tests, `DataStepTests` rose by 3, from 3 to 6; `ExperimentControlsTests` holds 43 and is now also
    selected, for 49 in all.
  - constructor-unit-tests is unchanged at 1552.
- **Mutations** (`.artifacts/0325-dev-mutation.log`). All 10 were caught by a new fact, and each source was restored
  byte-identical. They covered:
  - Exclude sending the draft instead of the field's text; Review sending nothing;
  - an excluded point drawn as a dot; the ring off its centre; the excluded reason out of the message colour;
  - Review withheld, or Exclude offered, over a read-only kit;
  - a 0-based chart x; a capture caption without its index; a mapped Mueller row reading m11.
- The final gate runs came after the mutations.
- No run exceeded its effective deadline.
- Every touched file is LF only (0 CR bytes, counted in Python).

```yaml
gates:
  constructor_unit_tests: 1552
  ui_smoke_tests: 256
  ui_tests: 49
```

## Architecture

- **A pure projection of the review.** The panel reads `reviewedDataOpt`, `dataDraft`, the experiment's exclusions
  (`tryFindExclusion`), `dataBadgeOf` and `kitWrites`. It sends only the existing messages (`ReviewExperimentData`,
  `SetExclusionReason`, `ExcludePoint`, `IncludePoint`); the update is untouched.
- **Captions and chart points are facts about the mapped points.** They are extension members of `MappedPoints` in
  `ConstructorUiIdOfDomain`, because Controls sees no Domain type.
  - The captions cover all five shapes by `%g`: intensity, ellipsometric, capture row, observation and mapped row.
  - A chart point's x is the 1-based point number; its y is the reading's first value.
  - A private `MappedReading.firstValue` is the one match from a mapped row's reading to its signal or m00.
- **The chart reuses the inline chart's drawing.** `pointsChartView` is a `Canvas` of the inline chart's size drawn with
  the private `PlotWindow.over`, `gridLines`, `axisLines` and `axisLabels`. Only the markers are new.
- **The review panel is keyed by its experiment** (`DataReviewPanel_<id>`). Point ids carry no experiment, and a verb's
  press handler is re-patched only when its id changes. Reviewing another experiment therefore recreates the panel.
- **Exclude commits the field's live text first.** A verb box does not blur the field, so this follows Create setup's
  rule (spec 0050 S4.3). When the field was committed, the reason sent is exactly `model.dataDraft.exclusionReason`.
- **One panel frame.** The split panel's border moved into `stepPanelFrame`, which both Data step panels use.

## Deferred

Nothing. Every requirement of the slice landed. The Data step's further panels belong to steps 326–327, by the
spec's plan.

## Gotchas

- **Exclude may send two messages.** Exclude reads the reason field's live text, then sends `SetExclusionReason` and
  `ExcludePoint` with it. A check of Exclude's messages should pick out `ExcludePoint`, not expect a single message.
- **Enter may commit a text field twice.** `KeyDownEvent` routes tunnel then bubble, and the field does not mark it
  handled, so a raised Enter can send `SetExclusionReason` twice. The commit is idempotent; Create setup's name field
  behaves the same way.
- **Through a module path, a Berreman name is its union case.** `Berreman.Geometry.Angle.degree` does not compile,
  because `Angle` resolves to the case. Open `Berreman.Geometry` / `Fields` / `Media` instead, as the tests now do.
- **No panel for a removed experiment.** When `reviewedDataOpt` names an experiment the collection no longer holds, no
  panel is shown.
- **The heading's badge comes from `experimentDataStatus`.** The review fixture sets E1's badge explicitly (2 kept, 1
  excluded), because the kit fixture loads E1 without data.
- **Grep miscounts CR bytes here.** Git Bash's `grep -c $'\r'` counts every line. Count CR bytes in Python, or use
  `git ls-files --eol`.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.

## Changelog

- 2026-11-12 — Step 0325: the Data step's review panel.
  - Review per data-file row; the reviewed points' captions, a dot-and-ring chart and per-point Exclude / Include.
  - Nine new UiIds; `MappedPoints.pointCaptions` / `chartPoints`; `ExperimentControls.pointsChartView`.
  - Tests: +3 ui-tests facts in `DataStepTests`, +1 ui-smoke (256); the ids fact extended.
