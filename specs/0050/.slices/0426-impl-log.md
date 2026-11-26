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

## Operator recovery progress, 2026-11-15

- Before handoff refresh: read the recovery task, both worker prompts, project
  prompt, manuals 013/015/028/029, both post-mortem documents, the immutable
  S121-STUDY-GAIN-VIEW obligation and the current source/test changes.
- Source review confirms implementation 1fae25fb and repair 1c5a7f80 are already
  committed. The repair's only product change replaces the session-cell
  call-site construction with session.name.caption. The existing extension
  returns the same text; no alias, annotation or suppression was added.
- The seven-file implementation contains the requested IDs/captions, ordered
  sessions grid, gain choices, empty-session text, fit-mode choices and read-only
  behavior. Both exact new facts and the three existing-fact extensions remain
  present. Run-stage commands and the 0414 rendering/scrollbar fixes remain.
- The supervisor log records this recovery's IMPLEMENT dispatch at index 425
  and its unchanged 27-gate snapshot. Normal review, gate execution and measured
  checkpoint publication belong to that existing supervisor after worker exit;
  launching another instance would duplicate its work.
- Read the operator's existing .artifacts/0426-operator-repair/gate-results.json
  as historical evidence only. No worker gate command was executed and no
  current supervisor acceptance is asserted.
- Refreshed only the plan, log and state-of-the-world at their supplied paths.
  Keep the historical attempts above and inherited YAML floors. No product,
  test, contract, descriptor, counter, checkpoint or supervisor-state edit is
  needed; the existing manifest and lock-diagnostic edits are supervisor-owned.
- Final source diff is empty under Berreman/. The handoff diff has no whitespace
  errors or CR-at-EOL churn. Git's line-ending notice refers only to the
  pre-existing supervisor-owned .supervisor-lock.diag, which remains untouched.
  Returning to the active supervisor for its review and verification cycle.

## Testing state

No worker gates run. Invariant 6 of implement_worker.system-md assigns gate
execution and verdicts to the supervisor. Exactly two new UI test identities
are authorized; all other test identities and existing assertions are retained.
The earlier compiler failures and the later ELEV001 failure are historical;
the operator committed their repairs before this recovery dispatch.

Manual 029 and the existing operator gate-results.json record a separate
27-gate run: build 4.0 s; elevation 230.2 s; constructor 1761/1761 in 157.5 s;
smoke 264/264 in 230.4 s; selected UI 44/44 in 47.9 s; selected kit acceptance
2/2 in 23.8 s; and each of the 21 invariants in 0.2 s. The record marks all
passed, with zero failed/skipped test cases. These are operator measurements,
not worker verdicts or a substitute for the supervisor's recovery verification.
Every recorded duration is within the task's effective deadline. Manual 029
records elevation from f395597c across 13 projects, excluding ELEV004 only.

All requested implementation is committed and source-reviewed. Normal review
and the same 27-gate roster remain for the active supervisor, followed by
measured checkpoint 425 only on green. No worker development duration or
deadline overrun exists. commit_ready=false denotes pending supervisor
verification; it does not identify missing implementation. Do not use
--accept-current-slice or change the profile-1 recovery route.

## Artifacts

No new runtime artifacts. Existing operator evidence is
specs/0050/.artifacts/0426-operator-repair/gate-results.json. Any persistent run
output belongs only in the supplied specs/0050/.artifacts folder.

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
