# Where we are

Slice 0426 implements S121-STUDY-GAIN-VIEW for spec 0050 S12.1. The study
surfaces now expose the gain-model and fit-mode messages delivered by 0425.
All requested source and test changes are present within the seven authorized
files. Attempt 03 repairs the two forward references in the Unknowns scroll
test reported by the supervisor, preserving attempt 02's import and collection
repairs. Post-repair compilation and verification remain pending.

# What's working

- Added stable session-grid, session-gain and fit-mode automation IDs and descriptive gain captions.
- Added ordered session gain choices and the empty-session explanation to Unknowns.
- Added selected fit-mode controls before Run, with read-only mode text and disabled gain editing.
- Added the two declared interaction facts and extended ID, read-only and scroll coverage.
- Repaired view imports and reused the current experiment collection in the session grid and test fixture.
- Removed scroll-test forward references while retaining control presence, type and reachability assertions.

These describe delivered source, not measured runtime results.

# Tests

No worker build, test, analyzer or invariant command was executed. Invariant 6 of
implement_worker.system-md assigns gate execution and verdicts to the supervisor
after worker exit. No worker development durations or deadline overruns exist
to report. commit_ready=false reflects unexecuted verification.

The attempt-03 task reports the latest supervisor build failed with two FS0039
errors in ScrollExtentTests.fs in 38.85 seconds, within the 600-second deadline.
Both referenced SlotBarTests, which compiles later. The repaired test finds the
grid and gain box in its existing subjects collection by their centralized IDs,
requiring exactly one match for each and retaining type/membership assertions.
The earlier attempt-02 failure (ten errors in 56.03 seconds) and its repairs
remain recorded in the implementation log. There is no measured post-repair
result; the supervisor must rerun the frozen roster.

The frozen roster requires build and arc-origin elevation (600 seconds each),
all constructor-unit-tests (180 seconds), all ui-smoke (600 seconds), ui-tests
selected over KitWindowTests, ScrollExtentTests and StudyViewTests (360 seconds),
kit-acceptance selected over KitAcceptanceTests (3600 seconds), and all twenty-one
s41 invariants (300 seconds each).

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

The selected UI count is expected to rise by two. Constructor, smoke and kit
acceptance identities are unchanged. The YAML carries constructor 1761 from
supervisor checkpoint 424 and smoke 264 from checkpoint 423; ui_tests 9 and
kit_acceptance_tests 0 are inherited from the preceding handoffs, not measured
current counts. The supervisor owns measured counts and checkpoint publication.

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
- The pre-existing specs/0050/.manifest.state.json edit belongs to the supervisor
  and was not changed by this worker.
- No runtime artifacts were produced. Any persistent run output belongs only
  under the supplied specs/0050/.artifacts folder.

# Changelog

- 2026-11-15: Added session gain and fit-mode controls with the declared UI coverage; handed verification to the supervisor.
- 2026-11-15 (attempt 02): Repaired missing view imports and stale experiment references in views and the test helper; handed post-repair verification to the supervisor.
- 2026-11-15 (attempt 03): Removed the scroll test's two forward references, preserved its assertions and handed verification to the supervisor.
