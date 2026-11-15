# Where we are

Slice 0415 implements spec 0050 S11.5's Inverse-only Fit step after Unknowns. Attempt 02 repairs the prior supervisor build's unavailable UI fixture references and ambiguous F# record inference. The source and all authorized test changes are ready for supervisor review and the unchanged 25-gate verification. This IMPLEMENT worker did not execute gates or claim a verified build.

## What's working

- Added Fit navigation, fit-run selection, calibration use and pre-fit checks against the latest fit's data.
- Added joint and staged run controls, per-iteration progress, Cancel, fit errors and chronological history.
- Reused the study bar and job host, withheld run verbs for read-only kits, and preserved scrolling and stable automation IDs.
- Added the four authorized facts and extended existing navigation, ID and read-only assertions.
- Repaired test compilation references with one shared UI-local history fixture and explicit experiment/completion record types.

These bullets describe delivered source, not runtime pass results.

## Tests

No build, test, elevation or invariant command ran in either worker attempt. IMPLEMENT Invariant 6 assigns execution to the supervisor after worker exit. The retry task reports the supervisor's prior build failed with 14 errors and 10 warnings in 77.51 seconds, below its 600-second deadline. This retry repairs the reported fixture-resolution and record-inference causes in source; it does not claim a passing rebuild. `commit_ready=false` reflects pending supervisor verification. No worker development duration or deadline overrun is claimed.

The supervisor must run the frozen `0415.gates` roster:

- Build and arc-origin elevation: 600 seconds each.
- Whole constructor-unit-tests: 180 seconds; one authorized new fact, expected 1,753 cases from checkpoint 413's 1,752.
- Whole ui-smoke: 600 seconds; one authorized new fact, expected 261 cases from checkpoint 413's 260.
- Selected ui-tests over KitWindowTests, ScrollExtentTests and StudyViewTests: 360 seconds; two authorized new facts.
- All twenty `invariant-s40-*` gates: 300 seconds each.

The new model test covers valid and invalid fit selection, calibration choice, ordered E1/E2 checks, MissingDataErr, latest-fit hash comparison while an older run is selected, the first-fit/no-study states and unchanged kit bytes. The UI facts cover the three run verbs and their layout/payloads, accessible names, calibration choice, pre-fit success/error/flag/empty states, history selection/order, disabled run verbs while running, progress, Cancel, completion refresh without reload, failure text, and reachability at 800 × 600 and 1000 × 540. Existing tests cover read-only withholding and Inverse-only flow membership.

The YAML retains historical floors only. Constructor and smoke come from checkpoint 413; ui_tests is the preceding handoff's inherited 16, not a measured total for this selection. The supervisor owns census and checkpoint publication.

```yaml
gates:
  constructor_unit_tests: 1752
  ui_smoke_tests: 260
  ui_tests: 16
```

## Architecture

StudyFitView projects the existing window state and dispatches the existing StudyFit messages. `studyPrefitsOpt` delegates to `Study.prefitWith` with the last chronological fit's dataRead; selecting an older run affects selection alone. No storage format or fitting algorithm changes.

The view reuses StudyUnknownsView.studyBarView and MaterialsWindowView's action/verb controls. The run-action container is keyed by study and calibration use to refresh captured request values. Fit action views add accessible names to the existing border views. The existing shell host owns job execution, cancellation and completion persistence; completion updates history through the existing model handler.

The UI history fixture lives once in `ScrollExtentTests.fs`, before its consumer `StudyViewTests.fs` in the existing compile order. Its records take the actual study's name, fit mode and experiment list and are written through the existing `writeFitRun` boundary. Numerical-result collections are empty because these facts exercise history and job-host presentation. No additional project reference, copied storage fixture or test identity was introduced.

## Deferred

No requested implementation is deferred. Supervisor review and gate execution remain mandatory before acceptance, as required by the worker role protocol.

## Gotchas

- Preserve Unknowns' AllowAutoHide=false and StudyViewTests.pressStudyBox's bounded render-until-hit wait. The Fit scroller also reserves scrollbar space; every new study press uses that helper.
- The shared study fixtures leave their source unspecified. New pre-fit tests bind their local source to the existing HeNe preset before reloading so missing data is tested after slot validation.
- The fit-history selection must not replace the latest fit as the source of pre-fit data hashes.
- `FitRunFileFixtures` belongs to the separate constructor test project and is unavailable in the UI suite. Both affected UI files now call `StudyFitViewFixture`; retain its compile-order placement.
- Persistent output is restricted to the assigned handoff documents; no runtime artifacts were produced.
- The entry `.manifest.state.json` change belongs to the supervisor and was preserved. No Git mutation, version bump, gate/census edit, external-repository write or notebook change occurred.
- Source edits are confined to the eleven authorized product/test paths. The ordinary and ignore-CR-at-EOL numstats match; no CRLF churn was introduced.

## Changelog

- 2026-11-14: Implemented the study Fit screen, model selections and stable IDs, with the four authorized test additions and navigation/read-only extensions; handed verification to the supervisor.
- 2026-11-14 (attempt 02): Repaired the supervisor-reported test fixture references and record-type ambiguity within the authorized files, preserving every assertion and the frozen verification contract.
