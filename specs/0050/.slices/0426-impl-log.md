# Implementation log

## Progress

- Read repository instructions, naming conventions, both worker prompts, the
  project prompt, slice 0426 and its resolved gate snapshot. Read standing
  manuals 013 and 015. The pre-existing manifest state edit is supervisor-owned.
- Inspected the study views, existing IDs, messages and mounted UI test helpers.
  The prior slice already supplies gain offers and both semantic messages.
- Before implementation: extend the authorized tests first, then add IDs,
  captions and view controls using existing presentation and dispatch patterns.
- Added the two exact declared facts. The existing mounted window helper now
  optionally records messages; its original callers retain their behavior.
  The gain fact covers all three offers, dispatch and persistence, initialization
  without writes and the empty-session text. The mode fact covers both selected
  styles, dispatch, persistence and placement before Run. Existing ID, read-only
  and Unknowns reachability facts retain their assertions and add the new controls.
- Before product edits: add centralized fixed/derived IDs and model captions,
  render sessions from the domain's existing members, and render mode choices
  with existing button helpers. Key mode controls by study to refresh retained
  action handlers; refresh ComboBox callbacks on each patch.
- Added the fixed sessions-grid ID, derived gain/mode IDs and gain captions.
  Unknowns renders the ordered session grid or its empty explanation, and only
  writable kits enable the gain boxes. Fit renders mode choices immediately
  before Run, with positive selection styling, or the read-only mode text.
  Run-stage buttons and all existing commands remain unchanged.
- Reviewed the source against the immutable S121-STUDY-GAIN-VIEW obligation.
  Corrected a test parenthesis and annotated its fold state. Captured messages
  are restricted to StudyCommand so unrelated UI initialization is irrelevant.
  Session experiment captions are elevated at construction. No implementation
  deviation or extra test identity was needed.
- Source diff line counts agree with and without ignore-cr-at-eol; there is no
  line-ending churn. The seven authorized source files are the only product/test
  edits. The pre-existing manifest state edit remains untouched.
- Before handoff: write the required state-of-the-world, carrying constructor
  1761 from supervisor checkpoint 424, smoke 264 from checkpoint 423, and the
  prior handoff's unmeasured ui_tests 9 / kit_acceptance_tests 0 floors.

## Attempt 02 progress

- Read the retry task and supervisor build diagnostics: compilation failed with
  ten errors in StudyUnknownsView and StudyFitView after 56.03 seconds, within
  the 600-second build deadline. No successful build is claimed.
- Before repair: replace the nonexistent kitExperiments member with the current
  experimentCollection.experiments surface in both session-grid reads and the
  new test helper. Add the missing Avalonia.Automation, Avalonia and
  Domain.Studies imports at the affected view declarations. Preserve every
  authorized test identity and assertion; no compatibility member is needed.
- Repaired both session-grid reads and the calibration-run helper to use the
  model's existing experiment collection. Unknowns now imports
  Avalonia.Automation for AutomationProperties; Fit now imports Avalonia for
  Thickness and Domain.Studies for FitMode and SetFitMode. These source changes
  address every reported compiler error and the same stale reference in the
  test project, which the failed build had not reached.
- The spec and immutable oracle spell the model collection kitExperiments;
  TableAndElementRotationModel actually declares experimentCollection, and
  existing study controls and SetExperimentRole use its experiments field.
  Reusing that live surface preserves the acceptance behavior without adding
  an alias or expanding the authorized effects.
- Kept the existing session/mode interactions, read-only behavior, exact test
  identities and assertions. No gate was run; supervisor verification remains
  required after this repair.

## Attempt 03 progress

- Read the retry task and supervisor diagnostics: the build reached the UI test
  project and failed on two FS0039 references to SlotBarTests in
  ScrollExtentTests.fs, in 38.85 seconds against the 600-second deadline.
- Source inspection identifies a forward reference: the project compiles
  ScrollExtentTests before SlotBarTests. The scroll test already owns subjects,
  the collection of controls whose reachability its existing law measures.
- Before repair: locate the sessions grid and gain box by centralized automation
  IDs in that existing collection, preserving their type/membership assertions
  and both screen sizes. Do not copy the later module's lookup helper, change
  compile order, remove assertions or add test identities. Gate execution stays
  with the supervisor under the act-only IMPLEMENT protocol.
- Replaced both forward references with Assert.Single lookups over subjects,
  matched by ConstructorUiId.studySessionGrid and studySessionGainChoice.
  Retained the Grid/ComboBox type assertions and both membership assertions;
  the existing reachability law still covers both small screen sizes. This also
  requires each target automation ID to identify exactly one scroll subject.
- The retry changes only ScrollExtentTests.fs and the three supplied handoff
  documents. All earlier product edits, test identities and other assertions
  remain in place. No helper copy or project-order change was needed.

## Files modified

- Created the supplied 0426-impl-plan.md, this log and 0426-state-of-the-world.md.
- Edited: Controls/UiIds.fs; Ui/ConstructorUiIdOfDomain.fs,
  StudyUnknownsView.fs, StudyFitView.fs; Ui.Tests/StudyViewTests.fs,
  ScrollExtentTests.fs and KitWindowTests.fs.

## Testing state

No worker gates run. Invariant 6 of implement_worker.system-md assigns gate execution
and verdicts to the supervisor. Exactly two new UI test identities are
authorized; all other test identities and existing assertions are retained.

The attempt-03 task supplies the latest supervisor build failure: two FS0039
errors in ScrollExtentTests.fs, exit code 1, 38.85 seconds against a 600-second
deadline. The earlier attempt-02 task reported ten compiler errors in 56.03
seconds. Both are historical supervisor evidence, not results of this repair.
The forward references are now removed; there is no post-repair gate verdict.

All requested implementation is present. Build, arc-origin elevation, the full
constructor suite, full ui-smoke, selected KitWindowTests / ScrollExtentTests /
StudyViewTests, selected KitAcceptanceTests and all twenty-one s41 invariants
remain for the supervisor. No worker development run or deadline overrun exists
to report. commit_ready=false reflects pending verification, not missing scope.
The expected selected UI delta is +2; constructor, smoke and acceptance test
identities are unchanged. No gate result or measured current count is claimed.

## Artifacts

No runtime artifacts. Any persistent run output belongs only in the supplied
specs/0050/.artifacts folder.

## Gotchas

- The act-only worker rule supersedes legacy local gate instructions.
- ScrollExtentTests compiles before SlotBarTests; its control-presence assertions
  use its own subjects collection instead of depending on the later module.
- No durable shape change, migration, version bump or Git mutation is needed.
- Gain options come only from gainModelOffers; if a saved gain no longer appears
  in those offers, the ComboBox has no selected index rather than changing the
  saved model on render. Initial and unchanged selections dispatch no edit.
- Mode choices use a study-specific parent key because actionButton retains its
  handler. ComboBox handlers use SubPatchOptions.Always for current study data.
- No gate/oracle/census, analyzer rules, known findings, checkpoint, external
  repository, notebook or lab-data edit was made.
