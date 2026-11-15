# Implementation log

## Progress

- Read AGENTS.md, naming conventions, both worker prompts, the project prompt,
  slice 0425, its resolved gate contract and S121-STUDY-GAIN-MSG oracle entry.
  Read standing manuals 013 and 015. The only pre-existing worktree edit is
  specs/0050/.manifest.state.json; it belongs to the supervisor.
- Confirmed the existing StudyCommand uses guardKitWrite and recordStudy for
  any successful StudyMsg. Existing gain validation already checks unknown
  sessions and anchors outside them. No window or storage change is needed.
- Before test changes: add the two authorized facts using the existing fixture,
  sent/recorded helpers and kitFileBytes; retain every existing fact/assertion.
- Added both declared facts. The message fact records Day 1's anchored gain and
  StagedFit, then checks both missing-study messages, a missing session and a
  valid kit AIR run outside Day 1, including file paths/bytes after each refusal.
  The offers fact covers zero/one/two/three Calibration runs, reordered session
  membership, unrelated Calibration runs, other roles and empty inputs.
- Before production changes: delegate the two messages through studyNamed and
  the existing members; add the typed session extension with ordered offers.
  No test execution was performed under the worker's act-only contract.
- Added SetSessionGainModel and SetFitMode with one-line S12.1 contract docs.
  applyTo reuses studyNamed and delegates without changing the existing errors.
  gainModelOffers filters session membership by CalibrationRun, always offers
  free gain, anchors when nonempty and interpolates first-to-last with two or
  more runs. Its List.last input is nonempty after the empty/singleton matches.
- Reviewed the source diff against the authorized effects and retained all
  existing assertions. Adjusted new record expressions to the repository's
  brace style. Diff line counts with and without ignore-cr-at-eol agree; no
  line-ending churn was introduced. No implementation deviation was needed.
- Before handoff: write the required state-of-the-world at its supplied path,
  preserving constructor floor 1759 and inherited kit-acceptance floor 0.
- Wrote the state-of-the-world with all required sections, inherited floors,
  the two exact test identities and explicit supervisor-owned verification.
  Implementation is complete within the three authorized source files.

## Files modified

- Edited: Studies.fs, StudyMessageTests.fs, StudySessionTests.fs.
- Created: 0425-impl-plan.md, this log and 0425-state-of-the-world.md at the
  supplied paths.

## Testing state

No gates run. Invariant 6 of implement_worker.system-md assigns execution and
verdicts to the supervisor after worker exit. Two new test identities are
authorized; no other identity changes are planned. The expected constructor
delta is +2; kit-acceptance is unchanged.

All requested implementation is present. Build, arc-origin elevation, the full
constructor suite, selected KitAcceptanceTests and all twenty-one s41 invariants
remain for the supervisor. No development duration or deadline overrun exists
to report. commit_ready=false reflects unexecuted verification, not a missing
requirement. Constructor 1759 is inherited from supervisor checkpoint 422;
kit-acceptance 0 is the prior 0424 handoff's floor, not a measured count here.

## Artifacts

No runtime artifacts. Persistent run output, if any, belongs only in the
supplied specs/0050/.artifacts folder.

## Gotchas

- Session order, not the kit's order, determines AIR anchors and first/last.
- CalibrationRun defines AIR offers; there is no cross-session fallback.
- The worker role's explicit act-only instruction supersedes the legacy
  self-check sections of the shared/project prompts. No green verdict is claimed.
- No durable shape changes or migration, version bump, Git mutation, external
  repository write or gate/oracle/census change is part of this slice.
