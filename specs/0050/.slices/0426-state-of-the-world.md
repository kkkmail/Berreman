# Where we are

Slice 0426 implements S121-STUDY-GAIN-VIEW for spec 0050 S12.1. The study
surfaces now expose the gain-model and fit-mode messages delivered by 0425.
All requested source and test changes are committed in 1fae25fb, followed by
the operator's one-line ELEV001 repair in 1c5a7f80. This recovery reviewed the
committed implementation against the slice and immutable obligation, confirmed
manual 029's repair, and refreshed only the three worker handoff documents.
The active supervisor must complete normal review and its unchanged 27-gate
cycle before publishing measured checkpoint 425.

# What's working

- Added stable session-grid, session-gain and fit-mode automation IDs and descriptive gain captions.
- Added ordered session gain choices and the empty-session explanation to Unknowns.
- Added selected fit-mode controls before Run, with read-only mode text and disabled gain editing.
- Added the two declared interaction facts and extended ID, read-only and scroll coverage.
- Reused the current experiment collection and retained scroll-control presence, type and reachability assertions.
- Preserved the operator's reuse of SessionName.caption and the 0414 input-readiness and scrollbar fixes.

These describe delivered source, not measured runtime results.

# Tests

No worker build, test, analyzer or invariant command was executed. Invariant 6
of implement_worker.system-md assigns those verdicts to the supervisor after
worker exit. commit_ready=false reflects pending supervisor verification;
the requested implementation is complete. No worker development duration or
deadline overrun exists to report.

The operator's existing evidence at
../.artifacts/0426-operator-repair/gate-results.json records the following
separate run. These are historical operator measurements, not results of this
worker turn or an acceptance of checkpoint 425.

| Gate | Operator's recorded outcome | Duration | Effective deadline |
| --- | --- | --- | --- |
| build | Passed | 4.0 s | 600 s |
| elevation | Passed | 230.2 s | 600 s |
| constructor-unit-tests, whole suite | 1761/1761 | 157.5 s | 180 s |
| ui-smoke, whole suite | 264/264 | 230.4 s | 600 s |
| ui-tests, selected classes | 44/44 | 47.9 s | 360 s |
| kit-acceptance, selected class | 2/2 | 23.8 s | 3600 s |
| All 21 invariant-s41-* gates | Passed | 0.2 s each | 300 s each |

The record reports zero failures/skips and no deadline overrun. Manual 029
records elevation over 13 projects from the immutable f395597c arc origin,
with zero new findings and ELEV004 excluded. The selected UI classes remain
KitWindowTests, ScrollExtentTests and StudyViewTests; kit acceptance remains
KitAcceptanceTests. The supervisor must rerun this unchanged 27-gate roster
and publish checkpoint 425 only on green, following normal profile-1 recovery
without --accept-current-slice. It is already running this recovery dispatch;
no second supervisor was launched.

Exactly two test identities were added:

- OpticalConstructor.Ui.Tests.StudyViewTests.each session of the study offers its gain models, and choosing one sends SetSessionGainModel
- OpticalConstructor.Ui.Tests.StudyViewTests.the Fit step's mode choice sends SetFitMode

Both use TestAppScope.studyKitIn with E1 and E3 assigned CalibrationRun through
SetExperimentRole. The gain fact checks the Auto columns, headers, membership,
accessible controls and three captions, then captures each StudyCommand and
reads its saved gain back. It also covers no write on initialization and the
empty-session explanation. The mode fact checks labels, accessibility names,
selected styling, placement before Run, both commands and their saved modes.

The three authorized existing facts retain every earlier assertion. The ID
fact checks the exact new IDs; the read-only fact checks the disabled gain box,
absent mode controls and mode text; the Unknowns reachability fact explicitly
includes the sessions grid and gain box in the existing two-screen-size law.

The selected UI identity delta remains +2. Constructor, smoke and kit acceptance
identities are unchanged. The YAML below is retained from the prior handoff:
constructor 1761 from checkpoint 424, smoke 264 from checkpoint 423, and the
inherited ui_tests 9 / kit_acceptance_tests 0 entries. It does not republish the
operator measurements as baselines. The supervisor owns measured counts and
checkpoint publication.

```yaml
gates:
  constructor_unit_tests: 1761
  ui_smoke_tests: 264
  ui_tests: 9
  kit_acceptance_tests: 0
```

# Architecture

Unknowns uses Study.sessionsIn, StudySession.gainModelOffers and
Study.gainModelOf directly, reading model.experimentCollection.experiments as
the existing views and experiment-edit handler do. ComboBox selection emits SetSessionGainModel through
StudyCommand; callbacks refresh with FuncUI patches and ignore unchanged or
unselected values. The domain and existing kit write guard remain authoritative.
The session-name cell reuses the existing SessionName.caption member; no
conversion alias, boundary annotation or analyzer suppression was introduced.

Fit uses FitMode.all, the existing positive action/verb button presentation and
SetFitMode. A study-specific parent key refreshes retained action handlers when
the selected study changes. Run-stage buttons and requests are unchanged.
Read-only kits show their mode as text and disable gain choices.

Fixed IDs remain in Controls, Domain-derived IDs and captions in
ConstructorUiIdOfDomain, and each surface in its own view file. No durable data
shape, serializer, migration, storage path or domain rule changed.

# Deferred

No requested implementation is deferred. The supervisor must execute the
frozen gate roster before acceptance.

# Gotchas

- A saved gain no longer present in the current offers leaves the ComboBox
  unselected; rendering never rewrites the study or invents another offer.
- The spec's model.kitExperiments spelling is not a current model member; use
  experimentCollection.experiments. No alias or new model API was introduced.
- The window helper shares the original mounting path while optionally recording
  messages; existing tests continue to use it without recording.
- ScrollExtentTests compiles before SlotBarTests. Its sessions assertions reuse
  subjects, keeping both controls inside the existing reachability law without
  changing project order or duplicating the later module's lookup helper.
- The source diff has no line-ending churn. No Git mutation, version bump,
  external-repository write, lab/notebook edit or gate/oracle/census edit occurred.
- The pre-existing specs/0050/.manifest.state.json and .supervisor-lock.diag
  edits belong to the supervisor and were not changed by this worker. Published
  contracts, descriptors, counters, checkpoints and the arc origin are untouched.
- Implementation and repair are already committed by the operator; do not
  amend, reset or re-stage them. This recovery changes only the three handoffs.
- No new runtime artifacts were produced. Existing operator evidence stays
  under specs/0050/.artifacts/0426-operator-repair/.

# Changelog

- 2026-11-15: Added session gain and fit-mode controls with the declared UI coverage; handed verification to the supervisor.
- 2026-11-15 (attempt 02): Repaired missing view imports and stale experiment references in views and the test helper; handed post-repair verification to the supervisor.
- 2026-11-15 (attempt 03): Removed the scroll test's two forward references, preserved its assertions and handed verification to the supervisor.
- 2026-11-15 (operator recovery): Reviewed committed implementation and caption repair, refreshed the three handoffs and returned normal verification/checkpoint publication to the active supervisor.
